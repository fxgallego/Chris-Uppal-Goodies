| package |
package := Package name: 'CU Enhanced Scrolling Decorator'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

A slightly enhanced version of the Dolphin ScrollingDecorator.  It differs in that you can control whether it has horizontal and vertical scrollbars independently.  Also you can set whether it stretches the decorated View if that is smaller than the scrollable area.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.02'.


package classNames
	add: #EnhancedScrollingDecorator;
	add: #EnhancedScrollingDecoratorLayout;
	yourself.

package resourceNames
	add: #Presenter -> 'Enhanced scrolling container';
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #Presenter -> 'Enhanced scrolling container';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

LayoutManager subclass: #EnhancedScrollingDecoratorLayout
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ScrollingDecorator subclass: #EnhancedScrollingDecorator
	instanceVariableNames: 'scrollFlags'
	classVariableNames: 'ScrollXFlag ScrollYFlag StretchXFlag StretchYFlag'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

EnhancedScrollingDecoratorLayout guid: (GUID fromString: '{137911C6-8DBC-4ADF-A8FD-F20A97968732}')!
EnhancedScrollingDecoratorLayout comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

These are used by EnhancedScrollingDecorator views as their layout manager.'!
!EnhancedScrollingDecoratorLayout categoriesForClass!Unclassified! !
!EnhancedScrollingDecoratorLayout methodsFor!

layoutContainer: aScrollingDecorator
	"lay out the sub-views of the given ScrollingDecorator.  In our case all we are doing
	is setting the sub-view's size and adjusting any scrollbars"

	| target extent desiredExtent newExtent offset newOffset |

	#CUtodo. "eliminate the duplication between this and #preferredLayoutExtentOf:"

	target := self targetViewIn: aScrollingDecorator.
	target isNil ifTrue: [^ self].

	extent := aScrollingDecorator clientExtent.
	desiredExtent := target layoutExtent.
	newExtent := extent x @ extent y.
	(extent x < desiredExtent x)
		ifTrue: [aScrollingDecorator doScrollX ifTrue: [newExtent x: desiredExtent x]]
		ifFalse: [aScrollingDecorator doStretchX ifFalse: [newExtent x: desiredExtent x]].
	(extent y < desiredExtent y)
		ifTrue: [aScrollingDecorator doScrollY ifTrue: [newExtent y: desiredExtent y]]
		ifFalse: [aScrollingDecorator doStretchY ifFalse: [newExtent y: desiredExtent y]].

	offset := aScrollingDecorator scrollOffset.
	newOffset := 0 @ 0.
	aScrollingDecorator doScrollX ifTrue: [newOffset x: ((offset x min: newExtent x - extent x) max: 0)].
	aScrollingDecorator doScrollY ifTrue: [newOffset y: ((offset y min: newExtent y - extent y) max: 0)].

	target rectangle: (newOffset negated extent: newExtent).
	aScrollingDecorator
		scrollOffset: newOffset;
		updateScrollBars.
!

positionViewsOf: aScrollingDecorator
	"private -- reposition aScrollingDecorator's subview"

	| target offset |

	target := self targetViewIn: aScrollingDecorator.
	target isNil ifTrue: [^ self].

	offset := aScrollingDecorator scrollOffset.

	target position: offset negated.
!

preferredLayoutExtentOf: aScrollingDecorator
	"answer how big we think the given ScrollingDecorator 'wants' to be"

	| desiredExtent actualExtent x y |

	#CUtodo. "eliminate the duplication between this and #layoutContainer:"

	"we start with the size from its sub-view since that, presumably, would rather
	not be stretched or scrolled, even if it is willing to be"
	desiredExtent := (self targetViewIn: aScrollingDecorator)
				ifNil: [^ Point new]
				ifNotNil: [:it |  it layoutExtent].

	"if we're allowed to scroll then we can just answer that"
	(aScrollingDecorator doScrollX and: [aScrollingDecorator doScrollY]) ifTrue: [^ desiredExtent].

	"otherwise we have to pretend that the target 'wants' to be the size it is, or else the
	system will add scrollbars that don't work"
	actualExtent := aScrollingDecorator clientExtent.
	x := aScrollingDecorator doScrollX ifTrue: [desiredExtent x] ifFalse: [desiredExtent x min: actualExtent x].
	y := aScrollingDecorator doScrollY ifTrue: [desiredExtent y] ifFalse: [desiredExtent y min: actualExtent y].

	^ x @ y.
!

targetViewIn: aScrollingDecorator
	"private -- answer the subview of aScrollingDecorator that we
	will control the size of.
	NB: we assume that the decorator has at most one subview"

	^ aScrollingDecorator managedSubViews at: 1 ifAbsent: [nil].
! !
!EnhancedScrollingDecoratorLayout categoriesFor: #layoutContainer:!geometry!public! !
!EnhancedScrollingDecoratorLayout categoriesFor: #positionViewsOf:!operations!private! !
!EnhancedScrollingDecoratorLayout categoriesFor: #preferredLayoutExtentOf:!geometry!public! !
!EnhancedScrollingDecoratorLayout categoriesFor: #targetViewIn:!helpers!private! !

EnhancedScrollingDecorator guid: (GUID fromString: '{CB38A6CE-BA2A-4D0E-B501-5CAC8E2AB475}')!
EnhancedScrollingDecorator comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

An enhanced version of ScrollingDecorator.   It differs in that you can control whether it has horizontal and vertical scrollbars independently.  Also you can set whether it stretches the decorated View if that is smaller than the scrollable area.
'!
!EnhancedScrollingDecorator categoriesForClass!Unclassified! !
!EnhancedScrollingDecorator methodsFor!

defaultLayoutManager
	"private -- answer the layout manager to use by default.  Note that
	we depend on this custom layout manager to implement most of our
	scrolling behaviour"

	^ EnhancedScrollingDecoratorLayout new.!

defaultScrollFlags
	"private -- answer the scroll flags to use by default"

	^ ScrollXFlag | ScrollYFlag | StretchXFlag | StretchYFlag.!

defaultWindowStyle

	| style |

	style := super defaultWindowStyle.

	self doScrollX ifTrue: [style := style bitXor: WS_HSCROLL].
	self doScrollY ifTrue: [style := style bitXor: WS_VSCROLL].

	^ style
!

doScrollX
	"answer whether we treat our target as horizonally scrollable (if not then
	we *attempt* to shrink it to fit)"

	^ scrollFlags allMask: ScrollXFlag.
!

doScrollX: aBool
	"set whether we treat our target as horizonally scrollable (if not then
	we *attempt* to shrink it to fit)"

	scrollFlags := scrollFlags mask: ScrollXFlag set: aBool.
	self baseStyleMask: WS_HSCROLL set: aBool recreateIfChanged: true.
	self invalidateLayout.
!

doScrollY
	"answer whether we treat our target as vertically scrollable (if not then
	we *attempt* to shrink it to fit)"

	^ scrollFlags allMask: ScrollYFlag.
!

doScrollY: aBool
	"set whether we treat our target as vertically scrollable (if not then
	we *attempt* to shrink it to fit)"

	scrollFlags := scrollFlags mask: ScrollYFlag set: aBool.
	self baseStyleMask: WS_VSCROLL set: aBool recreateIfChanged: true.
	self invalidateLayout.
!

doStretchX
	"answer whether we will stretch the View we decorate beyond its #preferredExtent"

	^ scrollFlags allMask: StretchXFlag.
!

doStretchX: aBool
	"set whether we will stretch the View we decorate beyond its #preferredExtent"

	scrollFlags := scrollFlags mask: StretchXFlag set: aBool.
	self invalidateLayout.!

doStretchY
	"answer whether we will stretch the View we decorate beyond its #preferredExtent"

	^ scrollFlags allMask: StretchYFlag.
!

doStretchY: aBool
	"set whether we will stretch the View we decorate beyond its #preferredExtent"

	scrollFlags := scrollFlags mask: StretchYFlag set: aBool.
	self invalidateLayout.!

initialize
	"private -- establish a coherent initial state"

	scrollFlags := self defaultScrollFlags.

	super initialize.
! !
!EnhancedScrollingDecorator categoriesFor: #defaultLayoutManager!constants!private! !
!EnhancedScrollingDecorator categoriesFor: #defaultScrollFlags!constants!initializing!private! !
!EnhancedScrollingDecorator categoriesFor: #defaultWindowStyle!constants!private! !
!EnhancedScrollingDecorator categoriesFor: #doScrollX!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doScrollX:!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doScrollY!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doScrollY:!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doStretchX!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doStretchX:!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doStretchY!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #doStretchY:!accessing!public! !
!EnhancedScrollingDecorator categoriesFor: #initialize!initializing!private! !

!EnhancedScrollingDecorator class methodsFor!

convertFromVersion9: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 9  to version 10"

	| doScrollX doScrollY doStretchX doStretchY scrollFlags answer |

	"we merged 4 booleans into one flagset"
	doScrollX := anArray at: (anArray size - 3).
	doScrollY := anArray at: (anArray size - 2).
	doStretchX := anArray at: (anArray size - 1).
	doStretchY := anArray at: (anArray size - 0).

	scrollFlags := 0.
	doScrollX ifTrue: [scrollFlags := scrollFlags | ScrollXFlag].
	doScrollY ifTrue: [scrollFlags := scrollFlags | ScrollYFlag].
	doStretchX ifTrue: [scrollFlags := scrollFlags | StretchXFlag].
	doStretchY ifTrue: [scrollFlags := scrollFlags | StretchYFlag].

	answer := anArray allButLast: 3.
	answer at: answer size put: scrollFlags.

	^ answer.
	!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	| bit |

	"NB: these values are used in stored View resources -- don't change them"
	bit := -1.
	ScrollXFlag := 1 << (bit := bit+1).
	ScrollYFlag := 1 << (bit := bit+1).
	StretchXFlag := 1 << (bit := bit+1).
	StretchYFlag := 1 << (bit := bit+1).
!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	Presenter addView: self asResource: 'Enhanced scrolling container'.
!

publishedAspectsOfInstances

	^ (super publishedAspectsOfInstances)
		add: (Aspect boolean: #doScrollX);
		add: (Aspect boolean: #doScrollY);
		add: (Aspect boolean: #doStretchX);
		add: (Aspect boolean: #doStretchY);
		yourself.
!

stbConvert: anArray fromVersion: anInteger
	"private -- convert from earlier version by updating and answering the array of instance
	variables' values"

	| slots |

	slots := super stbConvert: anArray fromVersion: anInteger.

	anInteger <= 9 ifTrue: [slots := self convertFromVersion9: slots].

	^ slots.!

stbVersion
	"version 9 was the intial one (number inherited from View)
	version 10 replaced 4 boolean instvars with a single flagset"

	^ 10.! !
!EnhancedScrollingDecorator class categoriesFor: #convertFromVersion9:!binary filing!private! !
!EnhancedScrollingDecorator class categoriesFor: #initialize!initializing!private! !
!EnhancedScrollingDecorator class categoriesFor: #installViewResources!development!must strip!private! !
!EnhancedScrollingDecorator class categoriesFor: #publishedAspectsOfInstances!development!must strip!public! !
!EnhancedScrollingDecorator class categoriesFor: #stbConvert:fromVersion:!binary filing!private! !
!EnhancedScrollingDecorator class categoriesFor: #stbVersion!binary filing!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: Presenter name: 'Enhanced scrolling container') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAACYCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAHwAAAENVIEVuaGFuY2VkIFNjcm9sbGluZyBEZWNvcmF0b3JSAAAAGgAAAEVu
aGFuY2VkU2Nyb2xsaW5nRGVjb3JhdG9yYgAAABMAAAAAAAAAAAAAAGIAAAACAAAAggAAAAQAAAAA
AABEAQACAKABAAAAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAoAEAAAYAIABFbmhhbmNlZFNj
cm9sbGluZ0RlY29yYXRvckxheW91dAAAAADqAAAAAAAAAAABAABiAAAAAAAAAAAAAAAGAgUAUG9p
bnQAAAAAAQAAAAEAAAAQAAAAUgIAAAAAAAARAAAAEQAAAB8AAAAGAQ8ATWVzc2FnZVNlcXVlbmNl
AAAAAMoAAAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAA
AABjcmVhdGVBdDpleHRlbnQ6YgAAAAIAAABSAgAAAAAAAAEAAAABAAAAUgIAAAAAAAC9AgAA9QEA
AKABAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////
/////////wAAAAAAAAAAXgEAAPoAAADKAAAAAAAAANAAAABAAgAAUgIAAAAAAADBAAAAwQAAAAAA
AAAVAAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoA
AAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAA
AAAAAFIAAAAHAAAAY3VycmVudFIAAAAWAAAAU2Nyb2xsaW5nRGVjb3JhdG9yLmljbw4CHwBTVEJF
eHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAA
AAA='))!

