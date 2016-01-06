| package |
package := Package name: 'CU Form Layout'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003 - 2005.
chris.uppal@metagnostic.org

Layout manager specialised to laying out simple forms which are just lists of <label, value> pairs.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '2.01'.


package classNames
	add: #FormLayout;
	add: #FormLayoutSpec;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #FormLayoutSpec
	instanceVariableNames: 'style placement indent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LayoutManager subclass: #FormLayout
	instanceVariableNames: 'labelWidth horizontalGap verticalGap labelIndent headerIndent wideItemIndent hasRaggedEdge arrangements'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

FormLayoutSpec guid: (GUID fromString: '{6F17AE73-EA9E-4F73-B4DA-B19D61AABCA0}')!
FormLayoutSpec comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Helper class for the FormLayout layout manager.'!
!FormLayoutSpec categoriesForClass!Unclassified! !
!FormLayoutSpec methodsFor!

defaultIndent
	"private -- answer the indent to use by default"

	^ 0.!

defaultPlacement
	"private -- answer the placement to use by default"

	^ self class placements first.!

defaultStyle
	"private -- answer the style to use by default"

	^ self class styles first.
!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: style.!

indent
	"answer our indent"

	^ indent.
!

indent: anInteger
	"set our indent to anInteger"

	indent := anInteger.!

initialize
	"private -- establish a coherent initial state"

	indent := self defaultIndent.
	placement := self defaultPlacement.
	style := self defaultStyle.

	^ super initialize.!

placement
	"answer our placement"

	^ placement.
!

placement: aSymbol
	"set our placement to aSymbol.  The placement determines how the FormLayout will treat us.
	See FormLayout class>>placements for a list"

	placement := aSymbol.!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		space;
		display: self.!

resolutionScaledBy: scale
	"private -- I'm not sure what this is supposed to do"

	indent := (indent * scale x) truncated.
!

style
	"answer our style"

	^ style.
!

style: aSymbol
	"set our style to aSymbol.  The style determines how the FormLayout will treat us.
	See FormLayout class>>styles for a list"

	style := aSymbol.! !
!FormLayoutSpec categoriesFor: #defaultIndent!constants!initializing!private! !
!FormLayoutSpec categoriesFor: #defaultPlacement!constants!initializing!private! !
!FormLayoutSpec categoriesFor: #defaultStyle!constants!initializing!private! !
!FormLayoutSpec categoriesFor: #displayOn:!displaying!public! !
!FormLayoutSpec categoriesFor: #indent!accessing!public! !
!FormLayoutSpec categoriesFor: #indent:!accessing!public! !
!FormLayoutSpec categoriesFor: #initialize!initializing!private! !
!FormLayoutSpec categoriesFor: #placement!accessing!public! !
!FormLayoutSpec categoriesFor: #placement:!accessing!public! !
!FormLayoutSpec categoriesFor: #printOn:!printing!public! !
!FormLayoutSpec categoriesFor: #resolutionScaledBy:!geometry!private! !
!FormLayoutSpec categoriesFor: #style!accessing!public! !
!FormLayoutSpec categoriesFor: #style:!accessing!public! !

!FormLayoutSpec class methodsFor!

header
	"answer a new instance with #header style"

	^ self style: #header.!

item
	"answer a new instance with #item style"

	^ self style: #item.!

label
	"answer a new instance with #label style"

	^ self style: #label.!

new
	"answer a new instance default initialisation"

	^ (self basicNew)
		initialize;
		yourself.!

placements
	"answer a collection of all the placements that FormLayouts understand"

	^ FormLayout placements.!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

    	^ (super publishedAspectsOfInstances)
		add: (Aspect integer: #indent);
		add: (Aspect choice: #placement from: self placements);
		add: (Aspect choice: #style from: self styles);
		yourself.!

style: aSymbol
	"answer a new instance with the given style"

	^ (self new)
		style: aSymbol;
		yourself.!

styles
	"answer a collection of all the styles that FormLayouts understand"

	^ FormLayout styles.!

wideItem
	"answer a new instance with #wideItem style"

	^ self style: #wideItem.! !
!FormLayoutSpec class categoriesFor: #header!instance creation!public! !
!FormLayoutSpec class categoriesFor: #item!instance creation!public! !
!FormLayoutSpec class categoriesFor: #label!instance creation!public! !
!FormLayoutSpec class categoriesFor: #new!instance creation!public! !
!FormLayoutSpec class categoriesFor: #placements!constants!public! !
!FormLayoutSpec class categoriesFor: #publishedAspectsOfInstances!commands!constants!development!must strip!public! !
!FormLayoutSpec class categoriesFor: #style:!instance creation!public! !
!FormLayoutSpec class categoriesFor: #styles!constants!public! !
!FormLayoutSpec class categoriesFor: #wideItem!instance creation!public! !

FormLayout guid: (GUID fromString: '{98BB7D70-80CE-43CD-AEC5-7945B8910870}')!
FormLayout comment: 'Copyright © Chris Uppal, 2003 - 2005.
chris.uppal@metagnostic.org

Layout manager specialised to laying out simple forms.

We lay out the sub-views vertically in pairs.  Each subview is either a #label or an #item (as determined by the #style aspect of its arrangement).  The labels are arranged in a fixed width collumn on the left (the width is determined by our #labelWidth aspect or computed if that is nil), the items go on the right, and extend up to the edge of the containing view.

Subviews can also be given the styles #header or #wideItem, which are just full-width versions of #label and #item respectively.  These take up a full row of the form.

The arrangements also have a #placement aspect which determines how subviews are alligned vertically within each row.  The row''s height is set to the maximum of the label and the item, and the placement is used to position the shorter subview within the overal row.  As a special case, a #placement of #lastItem can be given which causes that item to be extended to the bottom of the container (note that this is not compatible with vertically scrolling containers).

There are a couple of other aspects that should be clear enough, but which are too trivial to bother writing about...

We use the subviews'' #preferredExtents to determine how high they should be.  This has two effects.  One is that, since most items just answer their current actual extent to that message, once an item has been set to #fill its row -- i.e. to expand to fill the vertical space -- it will not shrink again if you change the #placement back to, say, #top.  You will have to re-size it manually or set a preferred extent.  The other effect is that it means we are slightly odd in that we acommodate ourself to fit the width of our container exactly, but have a height that is only fixed by our contents.

If you use the #labelOffset hack (which just adds an offset to the position of each label) to try to compensate for the way that Windows paints label text at the top-right of each subview, then you should ensure that the #horizontalGap and #verticalGap are big enough for the labels not to overlap the next subviews.

If you wish to use a scrolling decorator with this kind of layout then an EnhancedScrollingDecorators (from my package of the same name) will work better than a standard ScrollingDecorator, since that can be set up with:

	doScrollX: false;
	doScrollY: true;
	doStretchX: true;
	doStretchY; true;

So that our form expands and contracts to fit the container in every direction, except that it will use a vertical scrollbar if the container becomes too short.'!
!FormLayout categoriesForClass!MVP-Layout Managers-General! !
!FormLayout methodsFor!

arrangementAspect
	"not sure why this is needed"

	^ (Aspect framingConstraints: #arrangement) beImmutable.
!

arrangementOf: aView 
	"answer the current arrangement for aView. This will lazily
	generate one if none has currently been set."

	^ arrangements at: aView ifAbsentPut: [self defaultArrangementFor: aView].
!

arrangementOf: aView ifAbsent: a0Block
	"answer the current arrangement for aView or the result of evaluating
	a0Block if none has yet been set"

	^ arrangements at: aView ifAbsent: a0Block.
!

arrangementOf: aView put: aFormLayoutSpec
	"set the arrangement spec that we will use for the given View"

	arrangements at: aView put: aFormLayoutSpec.
!

defaultArrangementFor: aView
	"private -- given a View, which has presumably just been added to its parent
	view (but we can't assume that), answer a FormLayoutSpec that seems suitable
	for it"

	| shouldBeItem |

	shouldBeItem := false.
	self labelsAndItemsOf: aView parentView do:
		[:label :item | 
		label = aView ifTrue: [^ FormLayoutSpec label].
		item = aView ifTrue: [^ FormLayoutSpec item].
		shouldBeItem := item isNil and: [(self styleOf: label) ~= #header]].

	^ shouldBeItem ifTrue: [FormLayoutSpec item] ifFalse: [FormLayoutSpec label].!

defaultHasRaggedEdge
	"private -- answer whether we have ragged edge by default"

	^ false.!

defaultHeaderIndent
	"private -- answer the header indent to use by default"

	^ 0.!

defaultHorizontalGap
	"private -- answer the default gap between the rows"

	^ 5.!

defaultLabelIndent
	"private -- answer the label indent to use by default"

	^ 0.!

defaultLabelWidth
	"private -- answer the label width to use by default"

	^ 120.!

defaultVerticalGap
	"private -- answer the default gap between the labels and the items"

	^ 5.!

defaultWideItemIndent
	"private -- answer the wide item indent to use by default"

	^ 0.!

fullIindentFor: aView
	"private -- answer the current indent for aView or a default if none has yet been set"

	| arrangement |

	arrangement := self arrangementOf: aView ifAbsent: [^ 0].

	^ (self indentForStyle: arrangement style) + (arrangement indent).!

hasRaggedEdge
	"answer whether we align all items to the right edge of the container"

	^ hasRaggedEdge.
!

hasRaggedEdge: aBool
	"set whether we align all items to the right edge of the container"

	hasRaggedEdge := aBool.!

headerIndent
	"answer the extra amount by which we indent all #header subviews"

	^ headerIndent.
!

headerIndent: anInteger
	"set the extra amount by which we indent all #header subviews"

	headerIndent := anInteger.!

horizontalGap
	"answer the horizontal gap between the labels and the items"

	^ horizontalGap.!

horizontalGap: anInteger
	"set the horizontal gap between the labels and the items"

	horizontalGap := anInteger.!

indentForStyle: aSymbol
	"private -- answer the indent to use for the given style"

	#CUtodo.  "why didn't I keep these values in a dictionay ?"

	aSymbol = #label ifTrue: [^ labelIndent].
	aSymbol = #item ifTrue: [^ 0].		"fixed at zero"
	aSymbol = #header ifTrue: [^ headerIndent].
	aSymbol = #wideItem ifTrue: [^ wideItemIndent].

	^ 0.	"?? WTF ??"
!

indentOf: aView
	"answer the current indent for aView or a default if none has yet been set"

	^ self indentOf: aView ifAbsent: [0].
!

indentOf: aView ifAbsent: a0Block
	"answer the current indent for aView or the result of evaluating
	a0Block if none has yet been set"

	^ (self arrangementOf: aView ifAbsent: [^ a0Block value]) indent.
!

initialize
	"private -- establish a coherent initial state"

	arrangements := LookupTable new.
	hasRaggedEdge := self defaultHasRaggedEdge.
	headerIndent := self defaultHeaderIndent.
	horizontalGap := self defaultHorizontalGap.
	labelIndent := self defaultLabelIndent.
	labelWidth := self defaultLabelWidth.
	verticalGap := self defaultVerticalGap.
	wideItemIndent := self defaultWideItemIndent.


	^ super initialize.!

isFullWidth: aViewOrNil
	"private -- answer whether we think this view is a full-width item"

	^aViewOrNil
		ifNil: [false]
		ifNotNil: [:it | #( #header #wideItem ) includes: (self styleOf: it)].!

labelIndent
	"answer the extra amount by which we indent all #label subviews"

	^ labelIndent.
!

labelIndent: anInteger
	"set the extra amount by which we indent all #label subviews"

	labelIndent := anInteger.!

labelsAndItemsOf: aContainerView do: a2Block
	"private -- evaluate a2Block for each {label, item} pair, passing the label and item as the
	two parameters.
	The label or item may be nil.  The item may be nil if two labels follow in
	succession, or if the next subview wants to be treated as a header, or at the
	end of the list.  The label may be nil if two items follow in succession or if an item
	follows immediately after a header.  A #wideItem is treated like a label+nil followed
	by a nil+item.
	NB: because this is used by #defaultArrangementFor:, we cannot use the items'
	#arrangements directly"

	| subviews |

	subviews := aContainerView managedSubViews readStream.

	[| label item |
	label := self readLabelFrom: subviews.
	item := self readItemFrom: subviews.
	(label isNil and: [item isNil]) ifTrue: [^ self].
	(item notNil and: [self isFullWidth: label]) ifTrue: [subviews pop. item := nil].
	(label notNil and: [self isFullWidth: item]) ifTrue: [subviews pop. item := nil].
	a2Block value: label value: item]
		repeat.
!

labelWidth
	"answer the width of the labels.  May be nil"

	^ labelWidth.!

labelWidth: anIntegerOrNil
	"set the width of the labels, if this is nil then we will compute a width using the
	label's #layoutExtents"

	labelWidth := anIntegerOrNil.!

labelWidthOf: aContainerView
	"private -- answer how wide we should make the labels in aContainerView; this is only
	called if our #labelWidth is nil"

	| width |

	width := 0.
	self labelsAndItemsOf: aContainerView do:
		[:label :item || extent |
		(label notNil and: [(self styleOf: label) = #label]) ifTrue:
			[width := width max: label layoutExtent x]].

	^ labelIndent + width.!

layoutContainer: aContainerView
	"perform a layout the contents of aContainerView"

	self withLayoutOf: aContainerView do: [:view :rect | view rectangle: rect].!

layoutOfItem: aView extent: aPoint in: aRectangle labelWidth: anInteger
	"private -- answer the rectangle to use for the given item within the space
	allocated for one row of the form"

	| answer |

	answer := aRectangle copy.

	(self styleOf: aView) = #wideItem ifFalse: [answer left: answer left + anInteger + horizontalGap].
	answer left: answer left + (self fullIindentFor: aView).
	hasRaggedEdge ifTrue: [answer right: answer left + aPoint x].

	self place: aView height: aPoint y into: answer.

	^ answer.!

layoutOfLabel: aView extent: aPoint in: aRectangle labelWidth: anInteger
	"private -- answer the rectangle to use for the given label within the space
	allocated for one row of the form"

	| answer place |

	answer := aRectangle copy.

	(self styleOf: aView) = #header
		ifTrue: [hasRaggedEdge ifTrue: [answer right: answer left + aPoint x]]
		ifFalse: [answer right: (answer left + anInteger)].
	answer left: answer left + (self fullIindentFor: aView).

	self place: aView height: aPoint y into: answer.

	^ answer.!

place: aView height: anInteger into: aRectangle
	"private -- update the top and bottom of the given rectangle to reflect aView's
	preferred (vertical) placement"

	| left right top bottom place |

	top := aRectangle top.
	bottom := aRectangle bottom.
	place := self placementOf: aView.

	place = #top ifTrue:
		[bottom := top + anInteger].
	place = #bottom ifTrue:
		[top := bottom - anInteger].
	place = #center ifTrue:
		[top := (top + bottom - anInteger) // 2.
		bottom := top + anInteger].

	aRectangle
		top: top;
		bottom: bottom.

!

placementOf: aView
	"answer the current placement for aView a default if none has yet been set"

	^ self placementOf: aView ifAbsent: [#top].
!

placementOf: aView ifAbsent: a0Block
	"answer the current placement for aView or the result of evaluating
	a0Block if none has yet been set"

	^ (self arrangementOf: aView ifAbsent: [^ a0Block value]) placement.
!

preferredLayoutExtentOf: aContainerView
	"answer aPoint which is the preferred extent when laying out aContainerView"

	| insets cover |

	insets := aContainerView actualInsets.
	cover := Rectangle origin: insets origin corner: insets origin.
	self withLayoutOf: aContainerView do: [:view :rect | cover := cover merge: rect].
	cover := cover expandBy: insets.

	^ cover extent.!

readItemFrom: aStream
	"private -- answer the next item from a stream /provided/ that it wants to be treated
	as an item.  If not then answer nil.
	NB: because this is used by #defaultArrangementFor:, we cannot use the items'
	#arrangements directly"

	^ self
		readViewOfStyles: #( #item #wideItem )
		from: aStream.!

readLabelFrom: aStream
	"private -- answer the next item from a stream /provided/ that it wants to be treated
	as a label (or header).  If not then answer nil.
	NB: because this is used by #defaultArrangementFor:, we cannot use the items'
	#arrangements directly"

	^ self
		readViewOfStyles: #( #label #header )
		from: aStream.
!

readViewOfStyles: aCollection from: aStream
	"private -- answer the next item from a stream /provided/ that it has a style in the given collection
	If not then answer nil.
	NB: because this is used by #defaultArrangementFor:, we cannot use the items'
	#arrangements directly"

	| next style |

	aStream atEnd ifTrue: [^ nil].
	next := aStream next.

	"if it doesn't have an arrangement then assume it's OK"
	style := self styleOf: next ifAbsent: [^ next].

	"if it doesn't match then re-wind the stream and leave the
	item for later"
	(aCollection includes: style) ifFalse: [aStream pop. ^ nil].

	^ next.!

removeSubView: aView
	"remove aView from the collection of views for which we hold arrangements.
	Answers the View (for some reason)"

	arrangements removeKey: aView ifAbsent: [].

	^ super removeSubView: aView.!

resize: aView to: aRectangle
	"called to allow us to veto/modify/reflect asked-for changes to aView's visible position"

	aView rectangle: aRectangle.
	super resize: aView to: aRectangle.!

resolutionScaledBy: scale
	"private -- I'm not sure what this is supposed to do.  Just copied from other LayoutManagers"

	labelWidth := (labelWidth * scale x) truncated.
	horizontalGap := (horizontalGap * scale x) truncated.
	verticalGap := (verticalGap * scale y) truncated.

	arrangements do: [:each | each resolutionScaledBy: scale].!

styleOf: aView
	"answer the current style for aView or nil if none has yet been set"

	^ (self arrangementOf: aView ifAbsent: [^ nil]) style.
!

styleOf: aView ifAbsent: a0Block
	"answer the current style for aView or the result of evaluating
	a0Block if none has yet been set"

	^ (self arrangementOf: aView ifAbsent: [^ a0Block value]) style.
!

verticalGap
	"answer the vertical gap between rows"

	^ verticalGap.!

verticalGap: anInteger
	"set the vertical gap between rows"

	verticalGap := anInteger.!

wideItemIndent
	"answer the extra amount by which we indent all #wideItem subviews"

	^ wideItemIndent.
!

wideItemIndent: anInteger
	"set the extra amount by which we indent all #wideItem subviews"

	wideItemIndent := anInteger.!

withLayoutOf: aContainerView do: a2Block
	"private -- evaluate a2Block for each managed item of aContainerView passing the
	item and our intended layout for that item as parameters"

	| insets left right top actualLabelWidth |

	"the #items' heights are independent of the size of the container except that the last
	#item/#wideItem may extend to the bottom of the container;
	the label width may be fixed or may be computed from the max of the labels'
	layout extents;
	#items/#wideItems stretch to the edge of the container, as do #header items.
	Everything except #item starts at labelX + its own indent.
	#items start after the label-item gap"

	insets := aContainerView actualInsets.
	left := insets left.
	right := aContainerView clientWidth - insets right.
	actualLabelWidth := labelWidth ifNil: [self labelWidthOf: aContainerView].

	top := insets top.
	self labelsAndItemsOf: aContainerView do:
		[:label :item || labelExtent itemExtent rowHeight bottom bounds |
		labelExtent := label ifNil: [0@0] ifNotNil: [:it | it layoutExtent].
		itemExtent := item ifNil: [0@0] ifNotNil: [:it | it layoutExtent].
		(item notNil and: [(self placementOf: item) = #lastItem]) ifTrue:
			[itemExtent := itemExtent x @ (aContainerView clientHeight - insets bottom - top)].
		rowHeight := labelExtent y max: itemExtent y.
		bottom := top + rowHeight.
		bounds := Rectangle left: left top: top right: right bottom: bottom.
		label isNil ifFalse: [a2Block
					value: label
					value: (self layoutOfLabel: label extent: labelExtent in: bounds labelWidth: actualLabelWidth)].
		item isNil ifFalse: [a2Block
					value: item
					value: (self layoutOfItem: item extent: itemExtent in: bounds labelWidth: actualLabelWidth)].
		top := bottom + verticalGap].! !
!FormLayout categoriesFor: #arrangementAspect!constants!development!must strip!public! !
!FormLayout categoriesFor: #arrangementOf:!accessing!public! !
!FormLayout categoriesFor: #arrangementOf:ifAbsent:!accessing!public! !
!FormLayout categoriesFor: #arrangementOf:put:!accessing!public! !
!FormLayout categoriesFor: #defaultArrangementFor:!labels and items!private! !
!FormLayout categoriesFor: #defaultHasRaggedEdge!constants!private! !
!FormLayout categoriesFor: #defaultHeaderIndent!constants!initializing!private! !
!FormLayout categoriesFor: #defaultHorizontalGap!constants!private! !
!FormLayout categoriesFor: #defaultLabelIndent!constants!initializing!private! !
!FormLayout categoriesFor: #defaultLabelWidth!constants!private! !
!FormLayout categoriesFor: #defaultVerticalGap!constants!private! !
!FormLayout categoriesFor: #defaultWideItemIndent!constants!initializing!private! !
!FormLayout categoriesFor: #fullIindentFor:!geometry!private! !
!FormLayout categoriesFor: #hasRaggedEdge!accessing!public!testing! !
!FormLayout categoriesFor: #hasRaggedEdge:!accessing!public! !
!FormLayout categoriesFor: #headerIndent!accessing!public! !
!FormLayout categoriesFor: #headerIndent:!accessing!public! !
!FormLayout categoriesFor: #horizontalGap!accessing!public! !
!FormLayout categoriesFor: #horizontalGap:!accessing!public! !
!FormLayout categoriesFor: #indentForStyle:!geometry!private! !
!FormLayout categoriesFor: #indentOf:!accessing!public! !
!FormLayout categoriesFor: #indentOf:ifAbsent:!accessing!public! !
!FormLayout categoriesFor: #initialize!initializing!private! !
!FormLayout categoriesFor: #isFullWidth:!labels and items!private! !
!FormLayout categoriesFor: #labelIndent!accessing!public! !
!FormLayout categoriesFor: #labelIndent:!accessing!public! !
!FormLayout categoriesFor: #labelsAndItemsOf:do:!labels and items!private! !
!FormLayout categoriesFor: #labelWidth!accessing!public! !
!FormLayout categoriesFor: #labelWidth:!accessing!public! !
!FormLayout categoriesFor: #labelWidthOf:!geometry!private! !
!FormLayout categoriesFor: #layoutContainer:!geometry!public! !
!FormLayout categoriesFor: #layoutOfItem:extent:in:labelWidth:!geometry!private! !
!FormLayout categoriesFor: #layoutOfLabel:extent:in:labelWidth:!geometry!private! !
!FormLayout categoriesFor: #place:height:into:!geometry!private! !
!FormLayout categoriesFor: #placementOf:!accessing!public! !
!FormLayout categoriesFor: #placementOf:ifAbsent:!accessing!public! !
!FormLayout categoriesFor: #preferredLayoutExtentOf:!geometry!public! !
!FormLayout categoriesFor: #readItemFrom:!labels and items!private! !
!FormLayout categoriesFor: #readLabelFrom:!labels and items!private! !
!FormLayout categoriesFor: #readViewOfStyles:from:!labels and items!private! !
!FormLayout categoriesFor: #removeSubView:!public!removing! !
!FormLayout categoriesFor: #resize:to:!geometry!private! !
!FormLayout categoriesFor: #resolutionScaledBy:!geometry!private! !
!FormLayout categoriesFor: #styleOf:!accessing!public! !
!FormLayout categoriesFor: #styleOf:ifAbsent:!accessing!public! !
!FormLayout categoriesFor: #verticalGap!accessing!public! !
!FormLayout categoriesFor: #verticalGap:!accessing!public! !
!FormLayout categoriesFor: #wideItemIndent!accessing!public! !
!FormLayout categoriesFor: #wideItemIndent:!accessing!public! !
!FormLayout categoriesFor: #withLayoutOf:do:!geometry!private! !

!FormLayout class methodsFor!

addToLayoutManagerList
	"can't think of a better name...
		self addToLayoutManagerList.
	"

	(ClassCategory name: 'MVP-Layout Managers-General') addClass: self.!

placements
	"answer a collection of all the placements that our instances understand"

	^ #(
		#top
		#center
		#bottom
		#fill
		#lastItem
	).
!

publishedAspectsOfInstances
    	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."
    
    	^(super publishedAspectsOfInstances)
    		add: ((Aspect integer: #horizontalGap) isNullable: false);
    		add: ((Aspect integer: #verticalGap) isNullable: false);
    		add: ((Aspect integer: #labelWidth) isNullable: true);
    		add: ((Aspect integer: #headerIndent) isNullable: false);
		add: ((Aspect integer: #labelIndent) isNullable: false);
		add: ((Aspect integer: #wideItemIndent) isNullable: false);
		add: ((Aspect boolean: #hasRaggedEdge) isNullable: false);
		yourself
!

styles
	"answer a collection of all the styles that our instances understand"

	^ #(
		#header
		#label
		#item
		#wideItem
	).! !
!FormLayout class categoriesFor: #addToLayoutManagerList!development!public! !
!FormLayout class categoriesFor: #placements!constants!public! !
!FormLayout class categoriesFor: #publishedAspectsOfInstances!development!must strip!public! !
!FormLayout class categoriesFor: #styles!constants!public! !

"Binary Globals"!

"Resources"!

