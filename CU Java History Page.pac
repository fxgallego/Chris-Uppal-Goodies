| package |
package := Package name: 'CU Java History Page'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Adds a histogram showing the rates of object creation/deletion, the size of the population of live references, and the frequency of callbacks, to the JVM Status Monitor.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.06'.


package classNames
	add: #JVMHistoryPage;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #JVMHistoryPage -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Graphics Base';
	add: 'CU Java Status Monitor';
	add: 'CU PolyViewer';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

JVMStatusPageAbstract subclass: #JVMHistoryPage
	instanceVariableNames: 'showLegend scaleBy offsetBy updateNotified'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

JVMHistoryPage guid: (GUID fromString: '{75A0CBA9-4309-4E8E-9389-F558EA5C4815}')!
JVMHistoryPage comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A pluggin page for the Status Monitor.  Adds a histogram of:
	- the current population of Java object references (split by global vs. local)
	- the rate at which object references have been created
	- the rate at which object references have been released
	- the rate at which callbacks have been serviced.

Installing this class will automatically add it to the Status Monitor.
'!
!JVMHistoryPage categoriesForClass!Unclassified! !
!JVMHistoryPage methodsFor!

birthsColor
	"private - answer the colour to paint births in"

	^ Color green.!

calculatePaintingTransformTo: aRectangle maxX: anXInteger maxY: aYInteger
	"private -- work out the transform to use to render points in the range (1, 0) to (anXInteger+1, aYInteger), inclusive"

	| inset size |

	#CUtodo. "either take account of the origin of aRectangle (we're assuming it's 0), or
			switch to using the Graph Plot framework"

	inset := self insets.
	size := aRectangle extent.
	scaleBy := (size - (inset * 2)) / (anXInteger asFloat @ aYInteger negated asFloat).
	offsetBy := inset x @ (size y - inset y).
!

callbacksColor
	"private - answer the colour to paint callbacks in"

	^ Color yellow.!

createComponents
	"private - create presenters for the necessary components"

	super createComponents.

	self add: PaintingPresenter new name: 'Graph'.
!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self graphPresenter
		when: #paintRequired: send: #onPaintRequired: to: self.
!

deathsColor
	"private - answer the colour to paint deaths in"

	^ Color magenta.!

defer: a0Block
	"private -- evaluate a0Block in the UI thread"

	SessionManager inputState queueDeferredAction: a0Block.
!

globalRefsColor
	"private - answer the colour to paint population sizes of global refs in"

	^ Color darkCyan.!

graphPresenter
	"private -- answer the presenter named 'Graph'"

	^ self presenterNamed: 'Graph'.
!

helpFileName
	"private -- answer the filename of our help text; this
	is relative to the documentation directory"

	^ 'status-monitor-history.html'.!

initialize
	"private -- establish a coherent initial state"

	showLegend := true.
	updateNotified := false.

	super initialize.
!

insets
	"private - answer how much we prefer to be inset by"

	^ 5 @ 5.!

model: aJVMStatusModel
	"private -- set the model for this Presenter"

	super model: aJVMStatusModel.
	self model
		when: #historyChanged send: #onHistoryChanged to: self.

	self onHistoryChanged.
!

objectRefsColor
	"private - answer the colour to paint population sizes in"

	^ Color blue.!

onHistoryChanged
	"private -- the data that we are graphing has changed, arrange to update"

	updateNotified ifTrue:[^ self].

	updateNotified := true.
	self defer: [updateNotified := false. self graphPresenter view refreshContents].!

onPaintRequired: aPaintRequest
	"the graph view has determined that it needs us to repaint it"

	self paint: aPaintRequest.!

paint: aPaintRequest
	"private -- satisfy the paint request by drawing a graph of our history data"

	| canvas rectangle history max hScale vScale first |

	history := self model history.
	canvas := aPaintRequest canvas.
	rectangle := aPaintRequest rectangle.

	"work out the common scale to use"
	hScale := history size.
	max := history inject: 0 into: [:acc :each | acc max: each max].
	vScale := 100.
	[vScale < max] whileTrue: [vScale := vScale * 10].
	(max * 2.2 < vScale) ifTrue: [vScale := vScale / 2].

	"find the index of the first valid item (since we'll skip the ones before it)"
	first := history findFirst: [:each | each isValid].
	first = 0 ifTrue: [first := history size + 1].

	"work out the graphical transform"
	self calculatePaintingTransformTo: rectangle maxX: hScale maxY: vScale.
	scaleBy y >= 0 ifTrue: [^ self].	"scaleBy y is normally negative"

	"paint the gridlines first, then paint the data of them, then paint the grid numbers
	on top of all"
	self
		paintGridOn: canvas maxX: hScale maxY: vScale;
		paintObjectRefsFrom: history on: canvas startingAt: first;
		paintGlobalRefsFrom: history on: canvas startingAt: first;
		paintDeathsFrom: history on: canvas startingAt: first;
		paintCallbacksFrom: history on: canvas startingAt: first;
		paintBirthsFrom: history on: canvas startingAt: first;
		paintGridNumbersOn: canvas maxY: vScale.

	showLegend ifTrue: [self paintLegendOn: canvas].
!

paintArea: aCollection in: aColor on: aCanvas startingAt: anInteger
	"private -- paint the (valid) elements from aCollection of Floats on aCanvas,
	skipping the first anInteger-1 elements"

	| oldBrush oldPen points |

	anInteger > aCollection size ifTrue: [^ self].

	oldBrush := aCanvas brush: (Brush color: aColor).
	oldPen := aCanvas pen: (Pen withStyle: PS_NULL width: 0 color: aColor). 

	"start the polygon on the X axis"
	points := OrderedCollection new: aCollection size + 3.
	points add: (self transformX: anInteger-1 y: 0).

	"paint an exta initial block, although we don't know the previous number, and so
	should not really do so, since we don't know what to join up to"
	points add: (self transformX: anInteger-1 y: (aCollection at: anInteger)).

	"define the upper boundary"
	aCollection
		from: anInteger
		to: aCollection size
		keysAndValuesDo: [:i :each | points add: (self transformX: i y: each)].

	"end the polygon on the X axis"
	points add: (self transformX: aCollection size y: 0).
	aCanvas polygon: points.

	oldPen isNil ifFalse: [(aCanvas pen: oldPen) release].
	oldBrush isNil ifFalse: [(aCanvas brush: oldBrush) release].
!

paintBirthsFrom: aCollection on: aCanvas startingAt: anInteger
	"private -- paint the births from aCollection of JVMHistoryItems on aCanvas, skipping
	the first anInteger-1 elements"

	self
		paintLine: (aCollection collect: [:each | each birthsPerSecond])
		in: self birthsColor
		on: aCanvas
		startingAt: anInteger.
!

paintCallbacksFrom: aCollection on: aCanvas startingAt: anInteger
	"private -- paint the callbacks from aCollection of JVMHistoryItems on aCanvas, skipping
	the first anInteger-1 elements"

	self
		paintLine: (aCollection collect: [:each | each callbacksPerSecond])
		in: self callbacksColor
		on: aCanvas
		startingAt: anInteger.
!

paintDeathsFrom: aCollection on: aCanvas startingAt: anInteger
	"private -- paint the births from aCollection of JVMHistoryItems on aCanvas, skipping
	the first anInteger-1 elements"

	self
		paintLine: (aCollection collect: [:each | each deathsPerSecond])
		in: self deathsColor
		on: aCanvas
		startingAt: anInteger.
!

paintGlobalRefsFrom: aCollection on: aCanvas startingAt: anInteger
	"private -- paint the global ref counts from aCollection of JVMHistoryItems on
	aCanvas, skipping the first anInteger-1 elements"

	self
		paintArea: (aCollection collect: [:each | each globalRefs asFloat])
		in: self globalRefsColor
		on: aCanvas
		startingAt: anInteger.
!

paintGridNumbersOn: aCanvas maxY: anInteger
	"private -- paint numbers for the backgorund grid on aCanvas; aNumber is the vertical scale"

	| textHeight height offset |

	textHeight := aCanvas textMetrics tmHeight.
	offset := 2 @ textHeight.

	height := anInteger * scaleBy y negated.
	(height > (textHeight * 10 + 30)) ifFalse:
		[| point |
		point := (self transformX: 0 y: anInteger) - offset.
		(point y < 3) ifTrue: [point y: 3].
		aCanvas text: anInteger displayString at: point.
		^ self].

	0 to: anInteger by: anInteger / 10 do:
		[:y || point |
		point := (self transformX: 0 y: y) - offset.
		(point y > 0) ifTrue: [aCanvas text: y displayString at: point]].
!

paintGridOn: aCanvas maxX: anInteger maxY: anotherInteger
	"private -- paint background grid on aCanvas; aNumber is the vertical scale"

	| height extra |

	height := anotherInteger * scaleBy y negated.
	(height > 50) ifFalse: [^ self].

	extra := 2@0.
	0 to: anotherInteger by: anotherInteger / 10 do:
		[:y || point |
		point := self transformX: 0 y: y.
		aCanvas moveTo: point - extra.
		point := self transformX: anInteger y: y.
		aCanvas lineTo: point + extra].
!

paintLegendOn: aCanvas
	"private -- paint a legend on aCanvas"

	| size height width textHeight textWidth x y box oldBackcolor oldForecolor |

	showLegend ifFalse: [^ self].

	size := self graphPresenter view clientExtent.
	width := size x.
	height := size y.
	textHeight := aCanvas textMetrics tmHeight.
	textWidth := (aCanvas textExtent: 'Refs released per sec') x.

	x := width - textWidth - 10.
	y := 5.
	box := Rectangle origin: x-2@y-2 extent: textWidth+6@(textHeight*5+6).

	box left < 8 ifTrue: [^ self].
	box bottom > (height-10) ifTrue: [^ self].

	oldBackcolor := aCanvas backcolor: Color gray.
	oldForecolor := aCanvas forecolor: Color black.

	aCanvas fillRectangle: box brush: (Brush color: Color gray).

	aCanvas forecolor: self objectRefsColor.
	aCanvas text: 'Local refs' at: x@y.

	y := y + textHeight.
	aCanvas forecolor: self globalRefsColor.
	aCanvas text: 'Global refs' at: x@y.

	y := y + textHeight.
	aCanvas forecolor: self birthsColor.
	aCanvas text: 'Refs created per sec' at: x@y.

	y := y + textHeight.
	aCanvas forecolor: self deathsColor.
	aCanvas text: 'Refs released per sec' at: x@y.

	y := y + textHeight.
	aCanvas forecolor: self callbacksColor.
	aCanvas text: 'Callbacks per sec' at: x@y.

	aCanvas forecolor: oldForecolor.
!

paintLine: aCollection in: aColor on: aCanvas startingAt: anInteger
	"private -- paint the (valid) elements from aCollection of Floats on aCanvas, skipping
	the first anInteger-1 elements"

	| oldPen last point |

	anInteger > aCollection size ifTrue: [^ self].

	oldPen := aCanvas pen: (Pen color: aColor).

	last := self transformX: anInteger-1 y: (aCollection at: anInteger).
	aCanvas moveTo: last.

	aCollection from: anInteger to: aCollection size keysAndValuesDo:
		[:i :each |
		point := self transformX: i y: each.
		aCanvas lineTo: (last x @ point y).
		aCanvas lineTo: (last := point)].

	oldPen isNil ifFalse: [(aCanvas pen: oldPen) release].
!

paintObjectRefsFrom: aCollection on: aCanvas startingAt: anInteger
	"private -- paint the object ref counts from aCollection of JVMHistoryItems on
	aCanvas, skipping the first anInteger-1 elements"

	self
		paintArea: (aCollection collect: [:each | each objectRefs asFloat])
		in: self objectRefsColor
		on: aCanvas
		startingAt: anInteger.
!

queryCommand: aCommandQuery
	"private -- set the enabledness of a command"

	| cmd checked |

	super queryCommand: aCommandQuery.

	checked := aCommandQuery isChecked.
	cmd := aCommandQuery command.

	cmd == #toggleLegend ifTrue: [checked := showLegend].

	aCommandQuery isChecked: checked.!

toggleLegend
	"command -- toggle whether we show a legend on the graph"

	showLegend := showLegend not.
	self graphPresenter view refreshContents.!

transform: aPoint
	"private -- answer aPoint transformed by our painting transform"

	^ (aPoint * scaleBy) rounded + offsetBy.
!

transformX: anInteger y: anotherInteger
	"private -- answer anInteger@anotherInteger transformed by our painting transform"

	^ self transform: (anInteger@anotherInteger).
! !
!JVMHistoryPage categoriesFor: #birthsColor!constants!painting!private! !
!JVMHistoryPage categoriesFor: #calculatePaintingTransformTo:maxX:maxY:!painting!private! !
!JVMHistoryPage categoriesFor: #callbacksColor!constants!painting!private! !
!JVMHistoryPage categoriesFor: #createComponents!initializing!private!subpresenters! !
!JVMHistoryPage categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!JVMHistoryPage categoriesFor: #deathsColor!constants!painting!private! !
!JVMHistoryPage categoriesFor: #defer:!helpers!private! !
!JVMHistoryPage categoriesFor: #globalRefsColor!constants!painting!private! !
!JVMHistoryPage categoriesFor: #graphPresenter!private!subpresenters! !
!JVMHistoryPage categoriesFor: #helpFileName!constants!private! !
!JVMHistoryPage categoriesFor: #initialize!initializing!private! !
!JVMHistoryPage categoriesFor: #insets!constants!painting!private! !
!JVMHistoryPage categoriesFor: #model:!event handling!initializing!models!private! !
!JVMHistoryPage categoriesFor: #objectRefsColor!constants!painting!private! !
!JVMHistoryPage categoriesFor: #onHistoryChanged!event handling!private! !
!JVMHistoryPage categoriesFor: #onPaintRequired:!event handling!private! !
!JVMHistoryPage categoriesFor: #paint:!painting!private! !
!JVMHistoryPage categoriesFor: #paintArea:in:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintBirthsFrom:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintCallbacksFrom:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintDeathsFrom:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintGlobalRefsFrom:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintGridNumbersOn:maxY:!painting!private! !
!JVMHistoryPage categoriesFor: #paintGridOn:maxX:maxY:!painting!private! !
!JVMHistoryPage categoriesFor: #paintLegendOn:!painting!private! !
!JVMHistoryPage categoriesFor: #paintLine:in:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #paintObjectRefsFrom:on:startingAt:!painting!private! !
!JVMHistoryPage categoriesFor: #queryCommand:!commands!menus!public! !
!JVMHistoryPage categoriesFor: #toggleLegend!commands!public! !
!JVMHistoryPage categoriesFor: #transform:!painting!private! !
!JVMHistoryPage categoriesFor: #transformX:y:!painting!private! !

!JVMHistoryPage class methodsFor!

initialize
	"private -- class initialization.

		self initialize.
	"

	| page |

	page := (PolyViewerPageDescription new)
			initiallyVisible: true;
			presenterClass: self;
			label: 'History';
			yourself.

	JVMStatusShell addPolyViewerPage: page ownedBy: self.
!

polyViewerMenuCommands
	"Invoked by the PolyViewer framework.  Should answer a PolyViewerCommandList
	which defines a menu bar which will be merged into the menu bar of our enclosing
	PolyViewerShell instance"

	^ (PolyViewerCommandList new)

		"view menu"
		add: ((PolyViewerCommandList new)
			text: '&View';

			add: ((PolyViewerCommandList new)
				text: '&Update interval';
				name: #dynamicUpdateIntervalMenu;
				yourself);

			add: ((PolyViewerCommandList new)
				text: '&Sample size';
				name: #dynamicSampleSizeMenu;
				yourself);

			add: ((PolyViewerCommandItem new)
				text: 'Show &legend';
				command: #toggleLegend;
				yourself);

			add: PolyViewerCommandSeparator new;

			yourself);

		yourself.
!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	JVMStatusShell removePolyViewerPagesOwnedBy: self.
! !
!JVMHistoryPage class categoriesFor: #initialize!initializing!private! !
!JVMHistoryPage class categoriesFor: #polyViewerMenuCommands!constants!public! !
!JVMHistoryPage class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: JVMHistoryPage name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOUEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29s
b3IAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAoAEAAAYHDABCb3JkZXJMYXlvdXQAAAAAAQAA
AAEAAAAAAAAAAAAAAAAAAAAAAAAAmgEAAAAAAACaAAAAAAAAAFIAAAAQAAAAQ1UgR3JhcGhpY3Mg
QmFzZVIAAAANAAAAUGFpbnRhYmxlVmlld2IAAAASAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAA
AAABRAEEAABQAgAAAAAAAEYBAwABAAAAUkdCAAAAAAEBAQEAAAAABwAAAAAAAAAGBAQARm9udAAA
AAAAAAAAEAAAAAYBBwBMT0dGT05UAAAAAHIAAAA8AAAA9f///wAAAAAAAAAAAAAAAJABAAAAAAAA
AQIBIk1TIFNhbnMgU2VyaWYAAAAAAAAAAAAAAAAAAAAAAAAABgIFAFBvaW50AAAAAMEAAADBAAAA
AAAAAFACAABGCBAAAwAAAEdyYXBoaWNzU2V0dGluZ3MAAAAAAAAAAAYDAwBQZW4AAAAAAAAAABAA
AAAGAQYATE9HUEVOAAAAAHIAAAAQAAAAAAAAAAEAAAAAAAAAwMDAAAAAAADwAgAAEgIAAAAAAAAR
AAAA0AIAAAAAAAAgAAAARggGAAMAAABCaXRtYXAAAAAAAQAAABAAAAAAAAAAAAAAAAAAAAAAAAAA
AQAAADIDAAAAAAAAAQAAAAEAAAAHAAAAAAAAAAYCCQBSZWN0YW5nbGUAAAAAMgMAAAAAAAABAAAA
AQAAADIDAAAAAAAAAQAAAAEAAAAAAAAABgEPAE1lc3NhZ2VTZXF1ZW5jZQAAAADKAAAAAAAAANAA
AABiAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAAAAAAAFIAAAAQAAAAY3JlYXRlQXQ6ZXh0
ZW50OmIAAAACAAAAMgMAAAAAAAALAAAACwAAADIDAAAAAAAAqQIAAOEBAABQAgAABgEPAFdJTkRP
V1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8FAAAABQAA
AFkBAAD1AAAAygAAAAAAAADQAAAAYgAAAAAAAAAyAwAAAAAAAMEAAADBAAAAAAAAABcAAADqAAAA
AAAAAAABAABiAAAAAgAAAFACAABSAAAABQAAAEdyYXBoAgQAAAAAAAAyAwAAAAAAAAsAAAALAAAA
MgMAAAAAAAALAAAACwAAAEIEAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAACCBAAAAAAAAKAEAABi
AAAAAgAAADIDAAAAAAAACwAAAAsAAAAyAwAAAAAAAL0CAAD1AQAAoAEAAPIEAAAAAAAAcgAAACwA
AAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUAAABjAQAA/wAAAMoAAAAAAAAA0AAA
AGIAAAABAAAAUAIAAEAFAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJT
aW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxh
dGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAAAENvbnRhaW5lclZp
ZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xw
aGluZHIwMDUuZGxsAAAAAA=='))!

