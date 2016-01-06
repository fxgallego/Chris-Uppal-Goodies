| package |
package := Package name: 'CU Weirdness'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A little demo of some ways to use Dolphin to produce an, um, "unconventional" user interface.

It requires the ''WalicXe - Widgets'' package from:

	http://www.walicxe.com/pages/descargas.htm

and Udo Schneider''s ''US LayeredView'' package from:

	http://udos.swiki.net/3

Thanks to every concerned for providing that software (and they are /not/ to blame for the horrors hein ;-)


The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.13'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Weirdness'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\Metagnostic.ico''
		''Resources\Weirdness.ico''
		''Resources\Weirdness-pressed.ico''
		''Resources\Weirdness-pressed-toggled.ico''
		''Resources\Weirdness-toggled.ico''
		''Resources\Linen.jpg''
	).
!!'.

package classNames
	add: #WeirdnessMouseTracker;
	add: #WeirdnessShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #WeirdnessShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Graphics Base';
	add: 'CU Package-relative File Locator';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\ActiveX\Components\Picture\OLE Picture';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Third Party\Udo Schneider\US LayeredView';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\WalicXe\WalicXe - Widgets';
	yourself).

package setManualPrerequisites: #(
	'US LayeredView').

package!

"Class Definitions"!

Interactor subclass: #WeirdnessMouseTracker
	instanceVariableNames: 'observer selector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #WeirdnessShell
	instanceVariableNames: 'isClipping isTransparent isNothing isTopMost buttons preTexts postTexts diameters texture icons'
	classVariableNames: 'PostTexts PreTexts'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

WeirdnessMouseTracker guid: (GUID fromString: '{1291DEC6-4B34-4533-93B8-A6F9F367F8F3}')!
WeirdnessMouseTracker comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Helper to allow a WierdnessShell to follow the mouse as it moves across sub-presenters.'!
!WeirdnessMouseTracker categoriesForClass!Unclassified! !
!WeirdnessMouseTracker methodsFor!

observer
	"answer our observer"

	^ observer.
!

observer: anObject
	"private -- set our observer to anObject"

	observer := anObject.!

onMouseMoved: aMouseEvent

	observer perform: selector with: aMouseEvent.

	^ super onMouseMoved: aMouseEvent.!

selector
	"answer our selector"

	^ selector.
!

selector: anObject
	"private -- set our selector to anObject"

	selector := anObject.! !
!WeirdnessMouseTracker categoriesFor: #observer!accessing!public! !
!WeirdnessMouseTracker categoriesFor: #observer:!initializing!private! !
!WeirdnessMouseTracker categoriesFor: #onMouseMoved:!event handling!public! !
!WeirdnessMouseTracker categoriesFor: #selector!accessing!public! !
!WeirdnessMouseTracker categoriesFor: #selector:!initializing!private! !

!WeirdnessMouseTracker class methodsFor!

forPresenter: aPresenter observer: anObject selector: aSymbol

	^ (self forPresenter: aPresenter)
		observer: anObject;
		selector: aSymbol;
		yourself.! !
!WeirdnessMouseTracker class categoriesFor: #forPresenter:observer:selector:!instance creation!public! !

WeirdnessShell guid: (GUID fromString: '{FDBAE8EE-C4AE-40EB-9F6F-98566028CAA0}')!
WeirdnessShell comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org.

A demo of some ways to use Dolphin to produce an, um, "unconventional" user interface.

It features:

  - Use of "Regions" to clip the main window''s visible area to an irregular shape.
  - Use of "color keying" to make parts of the window transparent.
  - Use of a nil layout manager to place buttons in absolute positions.
  - A user interface that is largely drawn as raw graphics (the buttons are ''real'').
  - Some mouse tracking abuse.
  - A horredous denial of any notion of design quality or aesthetic value.

(Actually I rather like the idea of putting explanatory text in a halo around
''active'' widgets as the mouise approaches them -- but then, I have a
sick sense of humour...)'!
!WeirdnessShell categoriesForClass!Unclassified! !
!WeirdnessShell methodsFor!

backgroundPresenter
	"private -- answer the presenter named 'Background'"

	^ self presenterNamed: 'Background'.
!

bugs
	"command -- display the bugs-box"

	MessageBox
		warning: self class bugs
		caption: ('Known bugs in ' , self class toolName).!

cancelTrackingAt: aPoint
	"private -- called by the MouseTracker as the user cancels a drag operation.
	Just ignore it"
!

clippingButtonPresenter
	"private -- answer the presenter named 'ClippingButton'"

	^ self presenterNamed: 'ClippingButton'.
!

clippingRegion
	"private -- answer a Region that is made up of several ellipses centered on our
	buttons.
	NB: I've positioned the buttons by hand to ensure that the resulting
	shape does not overlap any edge of our window"

	| outer inner offset shape |

	outer := self view screenRectangle.
	inner := self backgroundPresenter view screenRectangle.
	offset := inner origin - outer origin.

	shape := Region empty.
	buttons do:
		[:each || ellipse |
		ellipse := each view clientRectangle insetBy: self maxDiameter negated.
		ellipse moveBy: offset.
		ellipse moveBy: each view position.
		shape := shape union: (Region ellipse: ellipse)].

	^ shape.
!

closeButtonPresenter
	"private -- answer the presenter named 'CloseButton'"

	^ self presenterNamed: 'CloseButton'.
!

continueTrackingAt: newPoint from: oldPoint
	"private -- called by the MouseTracker as we allow it to drag our window around.
	Move the window by the indicated amount"

	| oldPos newPos |

	newPoint = oldPoint ifTrue: [^ newPoint].

	oldPos := self view position.
	newPos := oldPos + newPoint - oldPoint.

	self view position: newPos.

	^ oldPoint.	"because we've shifted the reference point"!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (PaintingPresenter new) name: 'Background';
		yourself.

	"since we are going to be manipulating our buttons directly
	(which is rather unusual in an MVP app), we may as well
	create presenters for them -- not strictly necessary, but..."
	#( 'Clipping' 'Transparency' 'Close' 'Nothing' ) do:
		[:each || button |
		button := Presenter new.
		self add: button name: (each , 'Button').
		buttons add: button.
		diameters at: button put: 100].

	^ super createComponents.
!

createSchematicWiring
	"private -- arrange triggering between our components"

	self backgroundPresenter
		when: #paintRequired: send: #onPaintRequired: to: self;
		when: #leftButtonPressed: send: #startWindowDrag: to: self.

	"we install a special 'Interactor' into our subviews which will tell us about
	mouse movements"
	self backgroundPresenter view interactor: (WeirdnessMouseTracker
								forPresenter: self backgroundPresenter
								observer: self
								selector: #onMouseMoved:).
	buttons do: [:each | each view interactor: (WeirdnessMouseTracker
								forPresenter: each
								observer: self
								selector: #onMouseMoved:)].

	^ super createSchematicWiring.
!

endTrackingAt: aPoint
	"private -- called by the MouseTracker as the user completes a drag operation.
	Just ignore it"

!

help
	"command -- display the help-box"

	MessageBox
		notify: self class help
		caption: ('Help for ' , self class toolName).!

helpAbout
	"command -- display the about-box"

	MessageBox
		notify: self class about
		caption: ('About ' , self class toolName).!

iconPressed: aBool toggled: anotherBool
	"private -- answer an image that is sutable for using for a button in the indicated state.
	We swap button's images dynamically since they aren't capable of holding
	four distinct images for the four states we want to distinguish" 

	icons isNil ifTrue:
		[| finder |
		finder := self class resourceLocator.
		icons := #('-pressed-toggled' '-pressed' '-toggled' '') collect:
				[:each || name |
				name := 'Weirdness%s.ico' sprintfWith: each.
				Icon fromFile: name usingLocator: finder]].

	^ icons at: (aBool
			ifTrue: [anotherBool ifTrue: [1] ifFalse: [2]]
			ifFalse: [anotherBool ifTrue: [3] ifFalse: [4]]).!

initialize
	"private -- establish a coherent initial state"

	isClipping := isTransparent := isNothing := isTopMost := false.
	buttons := OrderedCollection new.
	diameters := IdentityDictionary new.

	^ super initialize.!

isClipping
	"answer whether we are clipping"

	^ isClipping.
!

isClipping: aBool
	"set whether we are clipping"

	| clip |

	isClipping = aBool ifTrue: [^ self].

	isClipping := aBool.

	"set the clipping button's images appropriately"
	(self clippingButtonPresenter view)
		imageDown: (self iconPressed: true toggled: aBool);
		imageUp: (self iconPressed: false toggled: aBool).

	"and either set our clipping region to something 'interesting' or to null"
	clip := aBool ifTrue: [self clippingRegion].
	self view setRegion: clip redraw: true.!

isNothing
	"answer whether we are nothing"

	^ isNothing.
!

isNothing: aBool
	"set whether we are nothing"

	isNothing := aBool.!

isTransparent
	"answer whether we are transparent"

	^ isTransparent.
!

isTransparent: aBool
	"set whether we are transparent"

	isTransparent = aBool ifTrue: [^ self].

	isTransparent := aBool.

	"set the control button appropriately"
	(self transparencyButtonPresenter view)
		imageDown: (self iconPressed: true toggled: aBool);
		imageUp: (self iconPressed: false toggled: aBool).

	"if we want to use transparency then we have to turn on layering
	and set a colour-key colour.  If not then we just turn layering off.
	NB1: I have found no way to turn off the borders and window caption.
	Whatever cobination of windows flags I try, they still show.  The only
	way that I've found to get rid of them is the set a clipping Region that
	is inside our client extent.
	NB2: under some circumstances this can become /extremely/ slow.
	I don't know why, but suspect some sort of fragmentation in the memory
	that Windows uses (internally) to implement this stuff"
	aBool
		ifTrue: [self view isLayered: true; colorKey: Color dialog]
		ifFalse: [self view isLayered: false].

	self backgroundPresenter invalidateView.
!

maxDiameter
	"private -- answer the max diameter that we will allow our
	button's 'halo's to grow to"

	^ 150.!

minDiameter
	"private -- answer the min diameter that we will allow our
	button's 'halo's to grow to"

	^ 30.!

nothingButtonPresenter
	"private -- answer the presenter named 'NothingButton'"

	^ self presenterNamed: 'NothingButton'.
!

onMouseMoved: aMouseEvent
	"private -- called (by our WierdnessMouseTrackers) as the mouse is moved over our
	subviews.  We use this to recalculate the sizes of the button's 'halo's and redraw the
	background if they've changed"

	| pos mustRefresh |

	pos := aMouseEvent screenPosition - self backgroundPresenter view screenRectangle origin.

	mustRefresh := self recalculateDiameters: pos.

	mustRefresh ifTrue: [	self backgroundPresenter invalidateView].
!

onPaintRequired: aPaintRequest
	"private -- we've arranged to be sent this when the backgound wants to be
	repainted"

	self paint: aPaintRequest.!

paint: aPaintRequest
	"satisfy the request for painting our background canvas, this is where
	we draw most of the interesting stuff"

	self
		paintTexture: aPaintRequest;
		paintButtons: aPaintRequest;
		paintText: aPaintRequest.
!

paintButtons: aPaintRequest
	"private -- paint the 'halo's around each button.
	(The buttons themselves are 'real' components and will paint themselves on top
	later)"

	| canvas offset |

	canvas := aPaintRequest canvas.
	offset := aPaintRequest rectangle origin.

	buttons  do:
		[:each || ellipse |
		ellipse := each view clientRectangle insetBy: (diameters at: each ifAbsent: [50]) negated.
		ellipse moveBy: (each view position + offset).
		canvas ellipse: ellipse].!

paintText: aPaintRequest
	"private -- paint the 'help' text above and below the current 'best' button.
	We clip and wrap the text to a rectangle that lies within the diameter
	of the 'halo' around each button.  Ideally I'd like to make the text
	wrap within the circular boundary, but there doesn't seem to be
	any simple way to do that"

	| button diameter canvas rect pre post above below style |

	diameter := 0.
	diameters keysAndValuesDo: [:each :diam | diam > diameter ifTrue: [diameter := diam. button := each]].

	diameter := (diameter * 0.6) rounded.
	diameter < self minDiameter ifTrue: [^ self].

	canvas := aPaintRequest canvas.
	rect := aPaintRequest rectangle.

	pre := PreTexts at: button view command ifAbsent: [''].
	post := PostTexts at: button view command ifAbsent: [''].

	rect := button view clientRectangle insetBy: diameter negated.
	rect moveBy: (button view position + aPaintRequest rectangle origin).
	above := (rect copy) bottom: rect centerY - 24; yourself.
	below := (rect copy) top: rect centerY + 24; yourself.

	style := (GraphicsTextStyle new)
			ellisionMode: #Word;
			wordWrap: true;
			font: self view actualFont;
			yourself.

	style
		alignment: #BottomCenter;
		paint: pre on: canvas in: above;
		alignment: #TopCenter;
		paint: post on: canvas in: below.
!

paintTexture: aPaintRequest
	"private -- paint background texture.  If we are currently transparent
	then do nothing and allowed the background colour (on which we
	are colour-keyed) to show"


	isTransparent ifTrue: [^ self].

	aPaintRequest canvas
		fillRectangle: aPaintRequest rectangle
		brush: self texture. 
	!

queryCommand: aCommandQuery
	"set the enabledness of the command represented by aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	"these are wasted on the actual buttons 'cos they don't toggle..."
	cmd == #toggleClipped ifTrue: [enabled := true. checked := isClipping].
	cmd == #toggleTransparent ifTrue: [enabled := true. checked := isTransparent].
	cmd == #toggleNothing ifTrue: [checked := isNothing].
	cmd == #toggleTopMost ifTrue: [checked := isTopMost].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

recalculateDiameters: aPoint
	"private -- this gets called as we track the mouse.  Given the position of the mouse,
	calculate and store new diameters for each one's 'halo' based on how close the
	mouse is to it.  Answer whether any of the diameters have changed as a result"

	| changes |

	changes := false.

	buttons do:
		[:each || distance diameter |
		distance := (each view rectangle center dist: aPoint) asInteger.
		diameter := ((self maxDiameter - distance) max: self minDiameter) min: self maxDiameter.
		(diameters at: each) = diameter ifFalse:
			[changes := true.
			diameters at: each put: diameter]].

	^ changes.!

simpleClippingRegion
	"private -- this is currently unused, but it illustrates the (unwarrentedly complicated)
	calculation needed to create a clipping Region that exactly covers our main window's
	client area"

	| outer inner |

	outer := self view screenRectangle.
	inner := self backgroundPresenter view screenRectangle.
	inner moveBy: outer origin negated.

	^ Region rectangle: inner.
!

startTrackingAt: aPoint
	"private -- called by the MouseTracker as the user starts a drag operation.
	Answer the supplied point which will start the sequence of points fed to
	#continueTrackingAt:from:"

	^ aPoint.!

startWindowDrag: aMouseEvent
	"private -- start a dragging operation that will cause the window to follow the
	mouse for as long as tracking is active.  I.e. allow the user to drag the window
	arround.  This is called when the user presses a mouse button over our background"

	"we only allow dragging if we don't have the normal window title-bar"
	isClipping ifFalse: [^ false].

	"a mouse tracker will look after the details, and callback to us as the
	operation progresses"
	(MouseTracker forPresenter: self startingAt: 0@0)
		origin: aMouseEvent position;
		startTracking: self.

	^ false.!

texture
	"private -- answer the texture we will use for our background"

	^ texture ifNil: [texture := Brush bitmap: (OLEPicture
							fromFile: 'Linen.jpg'
							usingLocator: self class resourceLocator)].!

todo
	"command -- display the todo-box"

	MessageBox
		notify: self class todo
		caption: ('Known deficiencies in ' , self class toolName).
!

toggleClipping
	"command -- toggle whether we use a clipped window shape"

	self isClipping: self isClipping not.!

toggleNothing
	"command -- toggle nothing"

	self isNothing: self isNothing not.!

toggleTopMost
	"command -- toggle whether this is a 'topmost' window"

	(isTopMost := isTopMost not)
		ifTrue: [self view beTopMost]
		ifFalse: [self view beNotTopMost].!

toggleTransparency
	"command -- toggle whether we use a window with holes in it"

	self isTransparent: self isTransparent not.!

transparencyButtonPresenter
	"private -- answer the presenter named 'TransparencyButton'"

	^ self presenterNamed: 'TransparencyButton'.
! !
!WeirdnessShell categoriesFor: #backgroundPresenter!private!subpresenters! !
!WeirdnessShell categoriesFor: #bugs!commands!public! !
!WeirdnessShell categoriesFor: #cancelTrackingAt:!dragging!private! !
!WeirdnessShell categoriesFor: #clippingButtonPresenter!private!subpresenters! !
!WeirdnessShell categoriesFor: #clippingRegion!clipping regions!private! !
!WeirdnessShell categoriesFor: #closeButtonPresenter!private!subpresenters! !
!WeirdnessShell categoriesFor: #continueTrackingAt:from:!dragging!private! !
!WeirdnessShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!WeirdnessShell categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!WeirdnessShell categoriesFor: #endTrackingAt:!dragging!private! !
!WeirdnessShell categoriesFor: #help!commands!public! !
!WeirdnessShell categoriesFor: #helpAbout!commands!public! !
!WeirdnessShell categoriesFor: #iconPressed:toggled:!helpers!private! !
!WeirdnessShell categoriesFor: #initialize!initializing!private! !
!WeirdnessShell categoriesFor: #isClipping!public!testing! !
!WeirdnessShell categoriesFor: #isClipping:!accessing!public! !
!WeirdnessShell categoriesFor: #isNothing!public!testing! !
!WeirdnessShell categoriesFor: #isNothing:!accessing!public! !
!WeirdnessShell categoriesFor: #isTransparent!public!testing! !
!WeirdnessShell categoriesFor: #isTransparent:!accessing!public! !
!WeirdnessShell categoriesFor: #maxDiameter!buttons!constants!private! !
!WeirdnessShell categoriesFor: #minDiameter!buttons!constants!private! !
!WeirdnessShell categoriesFor: #nothingButtonPresenter!private!subpresenters! !
!WeirdnessShell categoriesFor: #onMouseMoved:!event handling!private! !
!WeirdnessShell categoriesFor: #onPaintRequired:!event handling!private! !
!WeirdnessShell categoriesFor: #paint:!painting!public! !
!WeirdnessShell categoriesFor: #paintButtons:!buttons!painting!private! !
!WeirdnessShell categoriesFor: #paintText:!painting!private! !
!WeirdnessShell categoriesFor: #paintTexture:!painting!private! !
!WeirdnessShell categoriesFor: #queryCommand:!commands!public! !
!WeirdnessShell categoriesFor: #recalculateDiameters:!buttons!private! !
!WeirdnessShell categoriesFor: #simpleClippingRegion!clipping regions!private! !
!WeirdnessShell categoriesFor: #startTrackingAt:!dragging!private! !
!WeirdnessShell categoriesFor: #startWindowDrag:!dragging!event handling!private! !
!WeirdnessShell categoriesFor: #texture!helpers!private! !
!WeirdnessShell categoriesFor: #todo!commands!public! !
!WeirdnessShell categoriesFor: #toggleClipping!commands!public! !
!WeirdnessShell categoriesFor: #toggleNothing!commands!public! !
!WeirdnessShell categoriesFor: #toggleTopMost!commands!private! !
!WeirdnessShell categoriesFor: #toggleTransparency!commands!public! !
!WeirdnessShell categoriesFor: #transparencyButtonPresenter!private!subpresenters! !

!WeirdnessShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Weirdness.  Version 4.
Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org'.
!

bugs
	"answer a String describing the less than outstanding work"

	^
'Depressed buttons look wrong.
Pressed buttons look like inflamed pustules.
When reverting to the "boring" look, we should ensure that the title-bar is not off the edge of the screen.
'!

defaultIconName
	"answers the name of the Icon that can be used to represent this class"

	^ 'Metagnostic.ico'.!

description
	"answer a string briefly describing ourself"

	#subclassResponsibility.
	^ self comment.	"handy hack"!

help
	"answer our help text"

	"hacky, very hacky..."
	^ self description.!

icon
	"answers an Icon that can be used to represent this class"

	^ (Smalltalk at: #Icon)
		fromFile: self defaultIconName
		usingLocator: (self resourceLocator).
!

initialize
	"
		self initialize.
	"

	(PreTexts := IdentityDictionary new)
		at: #toggleClipping put: 'Turn clipping on or off';
		at: #toggleTransparency put: 'Turn transparency on or off';
		at: #exit put: 'Quit this demonstration';
		at: #toggleNothing put: 'Do nothin''';
		shrink.

	(PostTexts := IdentityDictionary new)
		at: #toggleClipping put: 'This switches the window between a "bland" mode with a normal rectangular frame and title-bar, and a mode where it is an irregular shape made up of several circles centred on the buttons.';
		at: #toggleTransparency put: 'This switches the window between a mode where the backgound is filled in with a simple linen ''texture'', and one where the background is removed allowing other windows to show through the hole.  This looks very odd unless you have also turned clipping on ;-)';
		at: #exit put: 'This button causes the demonstration to exit.
The ''X'' button on this window''s title-bar does the same thing, but that''s not available unless clipping is turned off (which is very boring).';
		at: #toggleNothing put: 'This button does nothing whatsoever ;-)
It''s here because I though that four buttons looked more interesting than only three.  It also gives you a button you can play with safely.
Have fun !!';
		shrink.

!

resourceLocator

	^ PackageResourceLocator packageNamed: 'CU Weirdness'.!

todo
	"answer a String describing the outstanding work"

	^
'It''d be nice to make the blue circles look 3-d with rounded, "plasticky", edges.
It''d be nice to use fluid shapes than mere circles.
Can we make the buttons gradually start glowing as the mouse approaches them ?
A more "SF"-ish background texture would look even more vulgar.
'
!

toolName
	"answer the Sting name to use for the name of the tool we define"

	| base stream lastWasUpper |

	base := self name.
	(base endsWith: 'Shell') ifTrue: [base := base allButLast: 5].

	"useful default implementation"
	stream := String writeStream.
	lastWasUpper := true.
	base do:
		[:each || isUpper |
		isUpper := each isUppercase.
		(isUpper and: [lastWasUpper not]) ifTrue: [stream space].
		lastWasUpper := isUpper.
		stream nextPut: each].

	^ stream contents.! !
!WeirdnessShell class categoriesFor: #about!documentation!public! !
!WeirdnessShell class categoriesFor: #bugs!documentation!public! !
!WeirdnessShell class categoriesFor: #defaultIconName!constants!public! !
!WeirdnessShell class categoriesFor: #description!documentation!public! !
!WeirdnessShell class categoriesFor: #help!documentation!public! !
!WeirdnessShell class categoriesFor: #icon!constants!public! !
!WeirdnessShell class categoriesFor: #initialize!public! !
!WeirdnessShell class categoriesFor: #resourceLocator!constants!public! !
!WeirdnessShell class categoriesFor: #todo!documentation!public! !
!WeirdnessShell class categoriesFor: #toolName!constants!displaying!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: WeirdnessShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOcTAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBBAIAoAEAAAAAAAAAAAAABgIFAFBvaW50AAAAAHkFAAB5BQAA
BwIAAAAAAAAAAAAAAAAAAKABAAAGBwwAQm9yZGVyTGF5b3V0AAAAAAEAAAABAAAAAAAAAAAAAAAA
AAAAAAAAAJoBAAAAAAAAmgAAAAAAAABSAAAAEAAAAENVIEdyYXBoaWNzIEJhc2VSAAAADQAAAFBh
aW50YWJsZVZpZXdiAAAAFQAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAAAQAIAAAAA
AAAGAQsAU3lzdGVtQ29sb3IAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAQAIAAEYIEAADAAAA
R3JhcGhpY3NTZXR0aW5ncwAAAAAAAAAABgMDAFBlbgAAAAAAAAAAEAAAAAYBBgBMT0dQRU4AAAAA
cgAAABAAAAAFAAAAAQAAAAAAAAAAAAAABgMFAEJydXNoAAAAAAAAAAAQAAAABgEIAExPR0JSVVNI
AAAAAHIAAAAMAAAAAAAAAAD//wAAAAAAAAAAALICAAAAAAAAEQAAAMACAAAAAAAAIAAAAAAAAAAH
AAAAAAAAAAYCCQBSZWN0YW5nbGUAAAAAAgIAAAAAAAABAAAAAQAAAAICAAAAAAAAAQAAAAEAAAAA
AAAAAAAAAAAAAAAAAAAABgEPAE1lc3NhZ2VTZXF1ZW5jZQAAAADKAAAAAAAAANAAAABiAAAAAQAA
AAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAAAAAAAFIAAAAQAAAAY3JlYXRlQXQ6ZXh0ZW50OmIAAAAC
AAAAAgIAAAAAAAABAAAAAQAAAAICAAAAAAAAYQUAADkFAABAAgAABgEPAFdJTkRPV1BMQUNFTUVO
VAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAAAAAALACAACcAgAA
ygAAAAAAAADQAAAAYgAAAAAAAAACAgAAAAAAAMEAAADBAAAAAAAAABsAAADqAAAAAAAAAAABAABi
AAAACgAAAJoBAAAAAAAAmgAAAAAAAABSAAAAEQAAAFdhbGljWGUgLSBXaWRnZXRzUgAAAAoAAABJ
Y29uQnV0dG9uYgAAABUAAAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAABLIAFEAQAAABAFAAAAAAAA
RgEDAAEAAABSR0IAAAAAAf7/AQAAAAAHAAAAAAAAAAAAAAAAAAAAEAUAAAAAAACCAAAACAAAAGkE
//8AAAAARgUSAAQAAABDb21tYW5kRGVzY3JpcHRpb24AAAAAugAAAAAAAABSAAAADQAAAHRvZ2ds
ZU5vdGhpbmdSAAAADgAAAFRvZ2dsZSBub3RoaW5nAQAAAAEAAAAAAAAARgUEAAMAAABJY29uAAAA
AAAAAAAQAAAABgIWAFBhY2thZ2VSZXNvdXJjZUxvY2F0b3IAAAAAUgAAAAwAAABDVSBXZWlyZG5l
c3NSAAAACgAAAFJlc291cmNlcy9SAAAADQAAAFdlaXJkbmVzcy5pY28AAAAAAQAAAAIGAAAAAAAA
AAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5S
AAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAA
AEEAAAAuLlwuLlwuLlwuLlxXaWRnZXRzXEljb25vc1xCb3RvbmVzXEJvdPNuVmVyZGVBY3Rpdm9Q
cmVzaW9uYWRvLmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQ
AAAAZG9scGhpbmRyMDA1LmRsbAAAAAAAAAAAEAYAAAIGAAAAAAAAAAAAABAAAAAiBgAAAAAAAFIA
AAAMAAAAQ1UgV2VpcmRuZXNzUAYAAFIAAAAVAAAAV2VpcmRuZXNzLXByZXNzZWQuaWNvAAAAAOID
AAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAAiBAAAAAAAAEAEAABiAAAAAgAAAAICAAAAAAAAZwMA
ALkBAAACAgAAAAAAAGEAAABhAAAAEAUAAJIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////
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'))!

