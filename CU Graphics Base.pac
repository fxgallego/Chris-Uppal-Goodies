| package |
package := Package name: 'CU Graphics Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Some stuff to help with creating graphical views.

The central abstraction is of a <Painter> which is any object that understands the #paint: message, where the parameter is an instance of PaintRequest.  A PaintRequest contains a refernce to a target canvas, plus rectangle(s) indicating the size and position of the area that is to be painted.   The idea of splitting out this abstraction is that then any of the View, Presenter, Model can act as a <Painter> according to the needs of the application.  Alternatively the role of the <Painter> can be split out into a fourth object that is separate from all of the MVP triad.    The point of all this flexibility is that there is no single "right" place for the painting code to live, the responsibility can naturally belong to almost any part of an application depending on the nature of that application.  So this package attempts to leave that decision as open as possible, and to make it as easy as possible to change.

The <Painter> abstraction is also used by my printing stuff.

Most of the work is done by GraphicsViewAbstract which "knows" how to invoke a <Painter> in order to paint itself.  GraphicsViewAbstract is a pretty complicated class since it also includes (rather heavily optimised) code to handle mapping from a potentially very large "logical" viewing surface onto the physical window.  Since Dolphin''s ScrollijngDecorator is not suitable for large logical surfaces (it has tight size limits), GraphicsViewAbstract provides its own implementation of scrolling by talking directly to Windows (incidentally, this is actually handled by a helper instace of GraphicsScroller, and those objects can be used independently of GraphicsViewAbstract).  GraphicsViewAbstract also provides other services to subclasses, such as double-buffering and the notion of #insets (an optional blank border /between/ the graphics and the scrollbars).  See the class comment for more details.

Several other classes make use of GraphicsViewAbstract.  GraphicsPresenter is a Presenter that is intended for use with subclassses of GraphicsViewAbstract.  PaintableView is a subclass of GraphicsViewAbstract that expects its Presenter to be its <Painter>.  PaintingView is a view that holds a reference to any object that is a <Painter> and uses that for painting.  PaintingPresenter is a subclass of GraphicsPresenter that expects to be used with a PaintableView; it implements #paint: by triggering an #paintRequired: event.  Alternatively it can be used as a base class for Presenters that do their own painting by overriding #paint:.   Besides these pre-packaged applications of GraphicsViewAbstract, that class is also intended to be used as a base class for View that implement custom graphics.

This package also includes a few simple independent Painter classes.  For instance see ExamplePainter and ExamplePainter2 for examples of how to create a standalone <Painter> object and use that with an off-the-self GraphicsPresenter and PaintableView to implement painting from an object that is not directly part of the MVP triad.

Note: this package will later include a simple framework for interpreting mouse gestures too.  I know I need that, but don''t yet have enough use-cases to be confident of how it should be designed.


The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '0.0150'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Graphics Base'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\grabber.cur''
		''Resources\grabbed.cur''
		''Resources\grabber-NS.cur''
		''Resources\grabbed-NS.cur''
		''Resources\grabber-EW.cur''
		''Resources\grabbed-EW.cur''
	).
!!'.

package classNames
	add: #DragScroller;
	add: #ExamplePainter;
	add: #ExamplePainter2;
	add: #GraphicsPresenter;
	add: #GraphicsScroller;
	add: #GraphicsSettings;
	add: #GraphicsTextStyle;
	add: #GraphicsViewAbstract;
	add: #PaintableView;
	add: #Painter;
	add: #PaintingPresenter;
	add: #PaintingView;
	add: #PaintRequest;
	add: #PluggablePainter;
	add: #SubjectPainter;
	add: #TextPainter;
	yourself.

package methodNames
	add: #Rectangle -> #isEmpty;
	add: #Rectangle -> #notEmpty;
	yourself.

package globalNames
	add: #RenderSettings;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	add: #RenderSettings;
	yourself).

package allResourceNames: (Set new
	add: #GraphicsPresenter -> 'Default view';
	add: #GraphicsPresenter -> 'SubjectPainter view';
	add: #GraphicsPresenter -> 'TextPainter view';
	add: #PaintingPresenter -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU GDI Extensions';
	add: 'CU Package-relative File Locator';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package setManualPrerequisites: #(
	'CU GDI Extensions').

package!

"Class Definitions"!

Object subclass: #DragScroller
	instanceVariableNames: 'target draggingCursor oldCursor'
	classVariableNames: 'FreeDragableCursor FreeDraggingCursor HorizontalDragableCursor HorizontalDraggingCursor VerticalDragableCursor VerticalDraggingCursor'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GraphicsScroller
	instanceVariableNames: 'scrollOffset lineExtent target'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #Painter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #PaintRequest
	instanceVariableNames: 'canvas rectangle viewport damage subject features'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #GraphicsSettings
	instanceVariableNames: 'pen brush font forecolor backcolor features isTextTransparent'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Model subclass: #GraphicsTextStyle
	instanceVariableNames: 'font forecolor backcolor insets textFlags'
	classVariableNames: 'AlignmentMap AlignmentMask DTx_BOTTOM DTx_CENTER DTx_TOP EllisionMap EllisionMask HMask VMask'
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Painter subclass: #ExamplePainter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Painter subclass: #ExamplePainter2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Painter subclass: #PluggablePainter
	instanceVariableNames: 'paintBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Painter subclass: #SubjectPainter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Painter subclass: #TextPainter
	instanceVariableNames: 'getTextBlock style'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #GraphicsPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicsPresenter subclass: #PaintingPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #GraphicsViewAbstract
	instanceVariableNames: 'settings buffer modeFlags scroller insets paintableExtent gva_dummy3 gva_dummy2 gva_dummy1'
	classVariableNames: 'AllowDragScrollingFlag AllowPaintInsetsFlag DoubleBufferedFlag EraseRequiredFlag OwnScrollingFlag RefillBufferFlag SimpleScrollingFlag'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicsViewAbstract subclass: #PaintableView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicsViewAbstract subclass: #PaintingView
	instanceVariableNames: 'painter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!

RenderSettings := GraphicsSettings!


"Loose Methods"!

!Rectangle methodsFor!

isEmpty
	"Answer true iff receiver contains no points."

	^ corner x <= origin x or: [corner y <= origin y].
!

notEmpty
	"Answer whether the receiver contains any points."

	^ self isEmpty not! !
!Rectangle categoriesFor: #isEmpty!public!testing! !
!Rectangle categoriesFor: #notEmpty!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

DragScroller guid: (GUID fromString: '{28CB77CF-AEC0-4336-8478-96BAC93E9C78}')!
DragScroller comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these takes a scrollable <View>and manages drag scrolling for it.

Unfortunately this doesn''t work with ScrollingDecorator, since that doesn''t have the right interpretation of #scrollOffset: (that only sets the scrollbars, it doesn''t actually invoke a scroll operation).
'!
!DragScroller categoriesForClass!Unclassified! !
!DragScroller methodsFor!

cancelTrackingAt: aPoint
	"private -- called by the MouseTracker as the user abandons the drag operation.
	Just turn off our cursor again"

	Cursor current: oldCursor.
!

continueTrackingAt: newPoint from: oldPoint
	"private -- called by the MouseTracker as we allow it to drag our window around"

	| delta oldOffset newOffset |

	delta := oldPoint - newPoint.

	oldOffset := target scrollOffset.
	newOffset := oldOffset + delta.
	target scrollOffset: newOffset.

	newOffset := target scrollOffset.
	delta := oldOffset - newOffset.

	^ oldPoint + delta.!

endTrackingAt: aPoint
	"private -- called by the MouseTracker as the user completes the drag operation.
	Just turn off our cursor again"

	Cursor current: oldCursor.!

start
	"start tracking the mouse"

	self startAt: Cursor position.!

startAt: aPoint
	"start tracking the mouse, starting at the given position"

	draggingCursor := self class freeDraggingCursor.
	(self trackerStartingAt: aPoint)
		startTracking: self.!

startHorizontal
	"start tracking the mouse allowing only horizontal movement"

	self startHorizontalAt: Cursor position.!

startHorizontalAt: aPoint
	"start tracking the mouse, starting at the given position and allowing only horizontal movement"

	draggingCursor := self class horizontalDraggingCursor.
	(self trackerStartingAt: aPoint)
		beHorizontalOnly
		startTracking: self.!

startTrackingAt: aPoint
	"private -- called by the MouseTracker as the user starts a drag operation.
	Answer the supplied point which will start the sequence of points fed to
	#continueTrackingAt:from:"

	oldCursor := Cursor current: draggingCursor.

	^ aPoint.!

startVertical
	"start tracking the mouse allowing only verticall movement"

	self startVerticalAt: Cursor position.!

startVerticalAt: aPoint
	"start tracking the mouse, starting at the given position and allowing only vertical movement"

	draggingCursor := self class verticalDraggingCursor.
	(self trackerStartingAt: aPoint)
		beVerticalOnly
		startTracking: self.!

target: aGraphicsView
	"set our scrollee"

	target := aGraphicsView.!

trackerStartingAt: aPoint
	"private -- answer a mouse tracker that starts at aPoint"

	^ MouseTracker forPresenter: target presenter startingAt: aPoint.! !
!DragScroller categoriesFor: #cancelTrackingAt:!event handling!private! !
!DragScroller categoriesFor: #continueTrackingAt:from:!event handling!private! !
!DragScroller categoriesFor: #endTrackingAt:!event handling!private! !
!DragScroller categoriesFor: #start!public!tracking! !
!DragScroller categoriesFor: #startAt:!public!tracking! !
!DragScroller categoriesFor: #startHorizontal!public!tracking! !
!DragScroller categoriesFor: #startHorizontalAt:!public!tracking! !
!DragScroller categoriesFor: #startTrackingAt:!event handling!private! !
!DragScroller categoriesFor: #startVertical!public!tracking! !
!DragScroller categoriesFor: #startVerticalAt:!public!tracking! !
!DragScroller categoriesFor: #target:!initializing!public! !
!DragScroller categoriesFor: #trackerStartingAt:!helpers!private! !

DragScroller methodProtocol: #mouseTrackerTarget attributes: #(#readOnly) selectors: #(#cancelTrackingAt: #continueTrackingAt:from: #endTrackingAt: #startTrackingAt:)!

!DragScroller class methodsFor!

cursor: aString
	"private -- answer a cursor with the given name from our standard location"

	^ Cursor
		fromFile: (File composePath: 'Resources' stem: aString extension: 'cur')
		usingLocator: self resourceLocator.
		!

discardCursorCache
	"throw away any cached cursors.

		self discardCursorCache.
	"

	FreeDragableCursor := FreeDraggingCursor := HorizontalDragableCursor := HorizontalDraggingCursor := VerticalDragableCursor := VerticalDraggingCursor
		:= nil.!

for: aGraphicsView
	"answer a new instance that will manage scrolling on behalf of the given GraphicsView"

	^ (self new)
		target: aGraphicsView;
		yourself.!

freeDragableCursor
	"answer the cursor to use to indicate the possibility of unrestricted dragging"

	^ FreeDragableCursor ifNil: [FreeDragableCursor := self cursor: 'grabber'].
!

freeDraggingCursor
	"answer the cursor to use to indicate ungoing unrestricted dragging"

	^ FreeDraggingCursor ifNil: [FreeDraggingCursor := self cursor: 'grabbed'].
!

horizontalDragableCursor
	"answer the cursor to use to indicate the possibility of horizontal-only dragging"

	^ HorizontalDragableCursor ifNil: [HorizontalDragableCursor := self cursor: 'grabber-EW'].
!

horizontalDraggingCursor
	"answer the cursor to use to indicate ungoing horizontal-only dragging"

	^ HorizontalDraggingCursor ifNil: [HorizontalDraggingCursor := self cursor: 'grabbed-EW'].
!

onPreStripImage
	"private -- discard any cached cursors in the (faint) hope that it will assist stipping"

	self discardCursorCache.!

resourceLocator
	"private -- answer a FileLocator that will find files our standard location for resources"

	^ PackageResourceLocator packageNamed: ##(self owningPackage name).!

verticalDragableCursor
	"answer the cursor to use to indicate the possibility of vertical-only dragging"

	^ VerticalDragableCursor ifNil: [VerticalDragableCursor := self cursor: 'grabber-NS'].
!

verticalDraggingCursor
	"answer the cursor to use to indicate ungoing vertical-only dragging"

	^ VerticalDraggingCursor ifNil: [VerticalDraggingCursor := self cursor: 'grabbed-NS'].! !
!DragScroller class categoriesFor: #cursor:!helpers!private! !
!DragScroller class categoriesFor: #discardCursorCache!helpers!public! !
!DragScroller class categoriesFor: #for:!instance creation!public! !
!DragScroller class categoriesFor: #freeDragableCursor!constants!public! !
!DragScroller class categoriesFor: #freeDraggingCursor!constants!public! !
!DragScroller class categoriesFor: #horizontalDragableCursor!constants!public! !
!DragScroller class categoriesFor: #horizontalDraggingCursor!constants!public! !
!DragScroller class categoriesFor: #onPreStripImage!helpers!private! !
!DragScroller class categoriesFor: #resourceLocator!helpers!private! !
!DragScroller class categoriesFor: #verticalDragableCursor!constants!public! !
!DragScroller class categoriesFor: #verticalDraggingCursor!constants!public! !

GraphicsScroller guid: (GUID fromString: '{A71947B4-DADF-4DBA-8232-7F41EC5FC2E2}')!
GraphicsScroller comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Helper objects used by a GraphicsViewAbstract to implement its #doOwnScrolling mode.

This class can be used by other Views, they should forward the following methods to the scroller:
	#onHScroll:
	#onVScroll:

The should be prepared to be sent the notification message, #onScrolledFrom:to: (a simple but perfectly workable response to which is just to do an #invalidate).

And, if they want keyboard scrollng to work, then they should also forward:
	#preTranslateKeyboardInput:

The scrollable area is set by asking our target for its #scrollableExtent, and our notion of how much of that is visible by asking our target for its #scrollViewportExtent.  (Note that this is different from ScrollingDecorators which "know" that the scrollable extent is their subview''s #layoutExtent and the scroll viewport extent is its #clientExtent.)

See the GraphicsViewAbstract code for examples of how to use these things.'!
!GraphicsScroller categoriesForClass!Unclassified! !
!GraphicsScroller methodsFor!

connect: aView
	"take over the management of the given View's scrolling"

	target isNil ifFalse: [self disconnect].

	target := aView.
	target
		baseStyleMask: (WS_HSCROLL|WS_VSCROLL) set: true recreateIfChanged: true;
		invalidateLayout.

	self updateScrollbars.
!

disconnect
	"relinquish management of our target's scrolling"

	target isNil ifTrue: [^ self].

	target
		baseStyleMask: (WS_HSCROLL|WS_VSCROLL) set: false recreateIfChanged: true;
		invalidateLayout.
	target := nil.
!

ensureRectangleVisible: aRectangle
	"ensure that as much as possible of the part of our target defined by aRectangle is scrolled
	into view"

	| visible offset |

	visible := self scrollViewport.
	(visible contains: aRectangle) ifTrue: [^ self].

	offset := visible origin.
	offset := (aRectangle corner - visible extent) max: offset.
	offset := aRectangle origin min: offset.

	self scrollTo: offset.!

handleHScroll: aScrollEvent
	"handle a horizontal scroll event sent to our target view.
	Answer whether we did anything"

	aScrollEvent thumbTrack ifTrue: [^ self horizontalScrollTo: aScrollEvent pos].
	aScrollEvent lineRight ifTrue: [^ self scrollLinesRight: 1].
	aScrollEvent lineLeft ifTrue: [^ self scrollLinesRight: -1].
	aScrollEvent pageRight ifTrue: [^ self scrollPagesRight: 1].
	aScrollEvent pageLeft ifTrue: [^ self scrollPagesRight: -1].
	aScrollEvent top ifTrue: [^ self horizontalScrollTo: 0].
	aScrollEvent bottom ifTrue: [^ self horizontalScrollTo: self scrollableWidth - 1].

	^ false.!

handleVScroll: aScrollEvent
	"handle a vertical scroll event sent to our target view.
	Answer whether we did anything"

	aScrollEvent thumbTrack ifTrue: [^ self verticalScrollTo: aScrollEvent pos].
	aScrollEvent lineDown ifTrue: [^ self scrollLinesDown: 1].
	aScrollEvent lineUp ifTrue: [^ self scrollLinesDown: -1].
	aScrollEvent pageDown ifTrue: [^ self scrollPagesDown: 1].
	aScrollEvent pageUp ifTrue: [^ self scrollPagesDown: -1].
	aScrollEvent top ifTrue: [^ self verticalScrollTo: 0].
	aScrollEvent bottom ifTrue: [^ self verticalScrollTo: self scrollableHeight - 1].

	^ false.!

horizontalScrollTo: anInteger
	"scroll horizontally to the given position, answer whether we made any
	changes"

	| new |

	new := self limitX: anInteger.

	new = scrollOffset x ifTrue: [^ false].

	self scrollTargetX: new.

	^ true.!

initialize
	"private -- establish a coherent initial state"

	lineExtent := 50@50.		"since we are intended mainly for graphics, this figure is pretty arbitrary"
	scrollOffset := 0@0.
	target := nil.

	^ super initialize.!

invalidateTarget
	"private -- ensure that our target view is not holding on to outdated
	assumptions about its size, shape, or scroll viewport"

	target ifNotNil: [:it | it invalidate].

!

limitX: anInteger
	"private -- answer an integer limited by our actualScrollExtent and adjusted so that we can't
	try to scroll so the end of a page isn't visible"

	| max |

	max := self scrollableWidth - self pageWidth.

	^ (anInteger max: 0) min: (max max: 0).!

limitY: anInteger
	"private -- answer an integer limited by our actualScrollExtent and adjusted so that we can't
	try to scroll so the end of a page isn't visible"

	| max |

	max := self scrollableHeight - self pageHeight.

	^ (anInteger max: 0) min: (max max: 0).!

lineExtent
	"answer our line extent"

	^ lineExtent.
!

lineExtent: aPoint
	"set our line extent to aPoint"

	lineExtent := aPoint max: (1@1).!

lineHeight
	"answer the height that we scroll by if the user asks to scroll by on 'line'"

	^ lineExtent y.!

lineWidth
	"answer the width that we scroll by if the user asks to scroll by on 'line'"

	^ lineExtent x.!

onHScroll: aScrollEvent
	"handler for a horizontal scroll event sent to our target view.
	Answer 0 if we did anything, nil otherwise"

	target isNil ifTrue: [^ nil].	"just in case"

	^ (self handleHScroll: aScrollEvent)
		ifTrue: [0]
		ifFalse: [nil].!

onVScroll: aScrollEvent
	"Handler for a vertical scroll event sent to our target view.
	Answer 0 if we did anything, nil otherwise"

	target isNil ifTrue: [^ nil].	"just in case"

	^ (self handleVScroll: aScrollEvent)
		ifTrue: [0]
		ifFalse: [nil].
!

pageExtent 
	"answer our page extent.
	This is derived from our target's size if we have one"

	^ target
		ifNil: [100@100]	"arbitrary"
		ifNotNil: [:it | it scrollViewportExtent].
!

pageHeight
	"answer our page height.
	This is derived from our target's size if we have one"

	^ self pageExtent y.!

pageWidth
	"answer our page width.
	This is derived from our target's size if we have one"

	^ self pageExtent x.!

preTranslateKeyboardInput: aMSG
	"answer whether the receiver would like to consume the argument aMSG,
	which is a keyboard message. Handle keyboard navigation"

	"NB: this is just copied (with very minor changes) from ScrollingDecorator.  I don't claim to
	understand a word of it..."

	| msg mask vk |

	"First ask the original view for its input mask"
	mask := UserLibrary default sendMessage: aMSG hwnd msg: WM_GETDLGCODE wParam: 0 lpParam: aMSG.

	"If control wants all keys, or this specific message, then don't absorb"
	(mask anyMask: ##(DLGC_WANTALLKEYS|DLGC_WANTMESSAGE)) ifTrue: [^false].

	msg := aMSG message.
	vk := aMSG wParam.

	(vk >= VK_LEFT and: [vk <= VK_DOWN]) ifTrue: [ "Arrow keys"
		"If control wants arrows, then don't absorb..."
		(mask allMask: DLGC_WANTARROWS) ifTrue: [^false].
		"...otherwise use keydowns for line scrolling"
		msg == WM_KEYDOWN ifTrue: [
			(vk == VK_LEFT or: [vk == VK_RIGHT]) ifTrue: [
				target sendMessage: WM_HSCROLL wParam: ((vk - VK_LEFT)//2 + SB_LINELEFT)].
			(vk == VK_DOWN or: [vk == VK_UP]) ifTrue: [
				target sendMessage: WM_VSCROLL wParam: ((vk - VK_UP)//2 + SB_LINEUP)].
			^true "absorbed for line scrolling"].
		^false].

	(vk == VK_END or: [vk == VK_HOME]) ifTrue: [
		msg == WM_KEYDOWN ifTrue: [
			target sendMessage: WM_VSCROLL wParam: ((vk - VK_HOME) abs + SB_TOP)].
		"Absorb home/end for scrolling to top/bottom"
		^true].

	(vk == VK_NEXT or: [vk == VK_PRIOR]) ifTrue: [
		msg == WM_KEYDOWN ifTrue: [
			target sendMessage: WM_VSCROLL wParam: ((vk - VK_PRIOR) + SB_PAGEUP)].
		"Absorb page-up/page-down for page scrolling"
		^true].

	^false.!

scrollableExtent
	"answer our target's logical size"

	^ target
		ifNil: [100@100]	"arbitrary"
		ifNotNil: [:it | it scrollableExtent].!

scrollableHeight
	"answer the maximum vertical scroll offset that may obtain"

	^ self scrollableExtent y.!

scrollableWidth
	"answer the maximum horizontal scroll offset that may obtain"

	^ self scrollableExtent x.!

scrollDownBy: anInteger
	"scroll down by the specified distance.
	Answers whether it did anything"

	^ self verticalScrollTo: scrollOffset y + anInteger.!

scrollLinesDown: anInteger
	"scroll down by the specified number of lines.
	Answers whether it did anything"

	^ self scrollDownBy: anInteger * self lineHeight.!

scrollLinesRight: anInteger
	"scroll right by the specified number of lines.
	Answers whether it did anything"

	^ self scrollRightBy: anInteger * self lineWidth.!

scrollOffset
	"answer our scroll offset"

	^ scrollOffset copy.
!

scrollOffset: aPoint
	"set our scroll offset to aPoint.
	NB this will update our target view if we have one"

	self scrollTo: aPoint.!

scrollPagesDown: anInteger
	"scroll down by the specified number of pages.
	Answers whether it did anything"

	^ self scrollDownBy: anInteger * self pageHeight.!

scrollPagesRight: anInteger
	"scroll right by the specified number of pages.
	Answers whether it did anything"

	^ self scrollRightBy: anInteger * self pageWidth.!

scrollRightBy: anInteger
	"scroll right by the specified distance.
	Answers whether it did anything"

	^ self horizontalScrollTo: scrollOffset x + anInteger.!

scrollTargetX: anInteger
	"private -- tell our target that it now has a new horizontal scroll offset, so it should
	probably re-draw itself or something.
	Updates the target's horizontal scrollbar too"

	| old |

	old := scrollOffset.
	scrollOffset := anInteger @ scrollOffset y.

	target ifNotNil:
		[:it |
		self setScrollInfo: SB_HORZ position: scrollOffset x.
		it onScrolledFrom: old to: scrollOffset].
!

scrollTargetY: anInteger
	"private -- tell our target that it now has a new vertical scroll offset, so it should
	probably re-draw itself or something.
	Updates the target's vertical scrollbar too"

	| old |

	old := scrollOffset.
	scrollOffset := scrollOffset x @ anInteger.

	target ifNotNil:
		[:it |
		self setScrollInfo: SB_VERT position: scrollOffset y.
		it onScrolledFrom: old to: scrollOffset].
!

scrollTo: aPoint
	"set our scroll offset to aPoint.
	NB this will update our target view if we have one"

	self
		horizontalScrollTo: aPoint x;
		verticalScrollTo: aPoint y.
!

scrollViewport
	"answer a Rectangle defining the part of the #scrollableRectangle
	that is currently scrolled into view"

	^ scrollOffset extent: self pageExtent.!

setScrollInfo: aBar info: aScrollinfo
	"private -- update the target's scrollbar indicated by aBar with data from the given SCROLLINFO"

	UserLibrary default 
		setScrollInfo: target asParameter
		fnBar: aBar
		lpsi: aScrollinfo
		fRedraw: true.!

setScrollInfo: aBar position: anInteger
	"private -- update the target's scrollbar indicated by aBar to show the given position"

	| scrollinfo |

	scrollinfo := (SCROLLINFO new)
				pos: anInteger;
				yourself.
	self setScrollInfo: aBar info: scrollinfo.!

setScrollInfo: aBar position: positionInteger range: rangeInterval page: pageInteger
	"private -- update the target's scrollbar indicated by aBar to show the given position, range, and page"

	| scrollinfo |

	scrollinfo := (SCROLLINFO new)
				range: rangeInterval;
				pos: positionInteger;
				page: pageInteger;
				yourself.
	self setScrollInfo: aBar info: scrollinfo.
!

updateHorizontalScrollbar
	"private -- update our client's vertical scrollbar to reflect its current width"

	scrollOffset x: (self limitX: scrollOffset x).

	self
		setScrollInfo: SB_HORZ
		position: scrollOffset x
		range: (0 to: self scrollableWidth - 1)
		page: self pageWidth.
!

updateScrollbars
	"update the state of our target's scroll bars to reflect its current size"

	target isNil ifTrue: [^ self].

	self
		updateHorizontalScrollbar;
		updateVerticalScrollbar;
		invalidateTarget.!

updateVerticalScrollbar
	"private -- update our client's vertical scrollbar to reflect its current height"

	scrollOffset y: (self limitY: scrollOffset y).

	self
		setScrollInfo: SB_VERT
		position: scrollOffset y
		range: (0 to: self scrollableHeight - 1)
		page: self pageHeight.
!

verticalScrollTo: anInteger
	"scroll vertically to the given position, answer whether we made any
	changes"

	| new |

	new := self limitY: anInteger.

	new = scrollOffset y ifTrue: [^ false].

	self scrollTargetY: new.

	^ true.! !
!GraphicsScroller categoriesFor: #connect:!managing!public! !
!GraphicsScroller categoriesFor: #disconnect!managing!public! !
!GraphicsScroller categoriesFor: #ensureRectangleVisible:!public!scrolling! !
!GraphicsScroller categoriesFor: #handleHScroll:!public!scrolling! !
!GraphicsScroller categoriesFor: #handleVScroll:!public!scrolling! !
!GraphicsScroller categoriesFor: #horizontalScrollTo:!public!scrolling! !
!GraphicsScroller categoriesFor: #initialize!initializing!private! !
!GraphicsScroller categoriesFor: #invalidateTarget!managing!private! !
!GraphicsScroller categoriesFor: #limitX:!helpers!private! !
!GraphicsScroller categoriesFor: #limitY:!helpers!private! !
!GraphicsScroller categoriesFor: #lineExtent!accessing!public! !
!GraphicsScroller categoriesFor: #lineExtent:!accessing!public! !
!GraphicsScroller categoriesFor: #lineHeight!accessing!public! !
!GraphicsScroller categoriesFor: #lineWidth!accessing!public! !
!GraphicsScroller categoriesFor: #onHScroll:!event handling!public! !
!GraphicsScroller categoriesFor: #onVScroll:!event handling!public! !
!GraphicsScroller categoriesFor: #pageExtent!accessing!public! !
!GraphicsScroller categoriesFor: #pageHeight!accessing!public! !
!GraphicsScroller categoriesFor: #pageWidth!accessing!public! !
!GraphicsScroller categoriesFor: #preTranslateKeyboardInput:!dispatching!public! !
!GraphicsScroller categoriesFor: #scrollableExtent!accessing!public! !
!GraphicsScroller categoriesFor: #scrollableHeight!accessing!public! !
!GraphicsScroller categoriesFor: #scrollableWidth!accessing!public! !
!GraphicsScroller categoriesFor: #scrollDownBy:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollLinesDown:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollLinesRight:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollOffset!accessing!public! !
!GraphicsScroller categoriesFor: #scrollOffset:!accessing!public! !
!GraphicsScroller categoriesFor: #scrollPagesDown:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollPagesRight:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollRightBy:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollTargetX:!managing!private!scrolling! !
!GraphicsScroller categoriesFor: #scrollTargetY:!managing!private!scrolling! !
!GraphicsScroller categoriesFor: #scrollTo:!public!scrolling! !
!GraphicsScroller categoriesFor: #scrollViewport!accessing!public! !
!GraphicsScroller categoriesFor: #setScrollInfo:info:!private!updating! !
!GraphicsScroller categoriesFor: #setScrollInfo:position:!private!updating! !
!GraphicsScroller categoriesFor: #setScrollInfo:position:range:page:!private!updating! !
!GraphicsScroller categoriesFor: #updateHorizontalScrollbar!private!updating! !
!GraphicsScroller categoriesFor: #updateScrollbars!public!updating! !
!GraphicsScroller categoriesFor: #updateVerticalScrollbar!private!updating! !
!GraphicsScroller categoriesFor: #verticalScrollTo:!public!scrolling! !

!GraphicsScroller class methodsFor!

convertFromVersion1: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 1 to version 2"

	"we removed the first instvar"
	^ anArray allButFirst.!

forView: aView
	"answer a new instance that will manage/configure scrolling for the given View"

	^ (self new)
		connect: aView;
		yourself.!

new
	"answer a new instance that is not yet connected to any view"

	^ (self basicNew)
		initialize;
		yourself.!

publishedAspectsOfInstances

	^ (super publishedAspectsOfInstances)
    		add: (Aspect name: #lineExtent) beImmutable;
		yourself.
!

stbConvert: anArray fromVersion: anInteger
	"private -- convert from earlier version by updating and answering the array of instance
	variables' values"

	| slots |

	slots := anArray.

	anInteger <= 1 ifTrue: [slots := self convertFromVersion1: slots].

	^ slots.!

stbConvertFrom: anSTBClassFormat
	"answer a Block that will convert an old version repesented as an Array to the current
	version"

	^ [:slots |
		| converted new |
		converted := self stbConvert: slots fromVersion: anSTBClassFormat version.
		new := self basicNew.
		converted keysAndValuesDo: [:i :each | new instVarAt: i put: each].
		new].
!

stbVersion
	"version 2 removed #scrollableExtent"

	^ 2.! !
!GraphicsScroller class categoriesFor: #convertFromVersion1:!binary filing!private! !
!GraphicsScroller class categoriesFor: #forView:!instance creation!public! !
!GraphicsScroller class categoriesFor: #new!instance creation!public! !
!GraphicsScroller class categoriesFor: #publishedAspectsOfInstances!development!must strip!public! !
!GraphicsScroller class categoriesFor: #stbConvert:fromVersion:!binary filing!private! !
!GraphicsScroller class categoriesFor: #stbConvertFrom:!binary filing!public! !
!GraphicsScroller class categoriesFor: #stbVersion!binary filing!public! !

Painter guid: (GUID fromString: '{78734E1D-E699-46C8-9AF4-0AB9A0574FFD}')!
Painter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Abstract base class that /can/ be used as a basis for implementations of <Painter>.

The class really only exists in order to document the <Painter> protocol (and to prevent that protocol evaporating).
'!
!Painter categoriesForClass!Unclassified! !
!Painter methodsFor!

pageCount
	"convenience method allowing us to implement <PrintProvider> that can
	print one page"

	^ 1.
!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted.
	The paint request holds the data that defines where to paint
	(#canvas and #rectangle), how to paint (#features), and
	what to paint (#subject)"

#subclassResponsibility.!

print: aPaintRequest page: anInteger
	"convenience method allowing us to implement <PrintProvider> by painting
	one page"

	self paint: aPaintRequest.
! !
!Painter categoriesFor: #pageCount!accessing!printing!public! !
!Painter categoriesFor: #paint:!painting!public! !
!Painter categoriesFor: #print:page:!printing!public! !

Painter methodProtocol: #PageProvider attributes: #() selectors: #(#pageCount #print:page:)!
Painter methodProtocol: #Painter attributes: #() selectors: #(#paint:)!

!Painter class methodsFor!

fromPageProvider: aPageProvider
	"answer a new instance that acts as an adaptor allowing a <PageProvider>
	to be used as a <Painter> that paints its first page.
	NB: instances of PageProvider (and its subclasses) are capable of acting as
	<Painter>s in their own right (needing no adaptor); the point of this method is
	that at will work with any object that understands the <PageProvider> protocol"

	^ self fromPageProvider: aPageProvider page: 1.!

fromPageProvider: aPageProvider page: anInteger
	"answer a new instance that acts as an adaptor allowing a <PageProvider>
	to be used as a <Painter> that paints its anInteger-th page.

	e.g:
		provider := ExamplePageProvider pageCount: 21.
		painter := Painter fromPageProvider: provider page: 17.
		GraphicsPresenter
			show: 'SubjectPainter view'
			on: painter.
	"

	^ PluggablePainter paintBlock: [:request | aPageProvider print: request page: anInteger].!

new
	"answer a new instance with default initalisation"

	^ (self basicNew)
		initialize;
		yourself.! !
!Painter class categoriesFor: #fromPageProvider:!adapters!public! !
!Painter class categoriesFor: #fromPageProvider:page:!adapters!public! !
!Painter class categoriesFor: #new!instance creation!public! !

PaintRequest guid: (GUID fromString: '{651EA4EF-E54C-4856-8F4A-FF11D3D1F9A7}')!
PaintRequest comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Simple data-holders that are passed to <Painter>''s #paint: methods by Views derived from GraphicsViewAbstract.

Incidentally, a PaintRequest is not unlike the Windows PaintEvent in that it is used to convey what should be painted where.  The important difference is that we can hold more data -- our #subject, #rectangle, #viewport, #damage, and #features.'!
!PaintRequest categoriesForClass!Unclassified! !
!PaintRequest methodsFor!

canvas
	"answer our canvas"

	^ canvas.
!

canvas: aCanvas
	"private -- set our canvas"

	canvas := aCanvas.!

copyForSubRectangle: aRectangle
	"answer a copy of this paint request with its own feature dictionary and
	all the rectangles restricted to that given"

	^ (self copy)
		restrictToRectangle: aRectangle;
		yourself.!

damage
	"answer our 'damage' rectangle, this is part of our #viewport that actually needs to
	be refeshed.  Drawing on the entire #rectangle is easier, but if you want to optimise,
	then you can arrange to draw only stuff that is damaged"

	^ damage ifNil: [self viewport].
!

damage: aRectangle
	"set our damage rectangle to that given"

	damage := aRectangle.!

features
	"answer our features dictionary"

	^ features.
!

features: aDictionary
	"set our features dictionary"

	features := aDictionary.!

positionOfViewportInRectangle: aRectangle
	"answer the position of our viewport within aRectangle.
	I.e. take the mapping that goes from our #rectangle to aRectangle
	and apply it to our viewport.
	The answer is rounded 'outwards' to the nearest integers"

	| scale offset origin extent corner |

	(viewport isNil or: [viewport = rectangle]) ifTrue: [^ aRectangle copy].

	scale := aRectangle extent / rectangle extent.
	offset := aRectangle origin - (rectangle origin * scale).

	origin :=  viewport origin * scale + offset.
	extent := viewport extent * scale.
	corner := origin + extent.

	^ origin floor corner: corner ceiling.

	!

postCopy
	"overdien to ensure that we get a copy of our feature dictionary"

	features := features copy.!

rectangle
	"answer our rectangle, this is the outer rectangle that defines the entire
	area that can be be drawn.  See also #clip"

	^ rectangle.
!

rectangle: aRectangle
	"private -- set our rectangle"

	rectangle := aRectangle.!

restrictToRectangle: aRectangle
	"private -- reduce all of our rectangles to their intersection with the given rect"

	rectangle := rectangle ifNotNil: [:it | it intersect: aRectangle].
	viewport := viewport ifNotNil: [:it | it intersect: aRectangle].
	damage := damage ifNotNil: [:it | it intersect: aRectangle].!

subject
	"answer our subject"

	^ subject.
!

subject: anObject
	"set our subject to anObject"

	subject := anObject.!

viewport
	"answer our 'vieport' rectangle, this is part of our #rectangle that is currently scrolled
	into view on screen (may be the whole rectangle, or may  even be bigger).  Drawing
	on the entire #rectangle is easier, but if you want to optimise, then you can arrange
	to draw only stuff that appears in the viewport"

	^ viewport ifNil: [self rectangle].
!

viewport: aRectangle
	"set our viewport rectangle to that given"

	viewport := aRectangle.! !
!PaintRequest categoriesFor: #canvas!accessing!public! !
!PaintRequest categoriesFor: #canvas:!initializing!private! !
!PaintRequest categoriesFor: #copyForSubRectangle:!copying!public! !
!PaintRequest categoriesFor: #damage!accessing!public! !
!PaintRequest categoriesFor: #damage:!initializing!public! !
!PaintRequest categoriesFor: #features!accessing!public! !
!PaintRequest categoriesFor: #features:!initializing!public! !
!PaintRequest categoriesFor: #positionOfViewportInRectangle:!geometry!public! !
!PaintRequest categoriesFor: #postCopy!copying!public! !
!PaintRequest categoriesFor: #rectangle!accessing!public! !
!PaintRequest categoriesFor: #rectangle:!initializing!private! !
!PaintRequest categoriesFor: #restrictToRectangle:!initializing!private! !
!PaintRequest categoriesFor: #subject!accessing!public! !
!PaintRequest categoriesFor: #subject:!initializing!public! !
!PaintRequest categoriesFor: #viewport!accessing!public! !
!PaintRequest categoriesFor: #viewport:!initializing!public! !

!PaintRequest class methodsFor!

canvas: aCanvas rectangle: aRectangle
	"answer a new instance holding the given data"

	^ (self new)
		canvas: aCanvas;
		rectangle: aRectangle;
		yourself.! !
!PaintRequest class categoriesFor: #canvas:rectangle:!instance creation!public! !

GraphicsSettings guid: (GUID fromString: '{14E30C4F-1CEC-4A96-8F3C-ABBCBFABD750}')!
GraphicsSettings comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org
'!
!GraphicsSettings categoriesForClass!Unclassified! !
!GraphicsSettings methodsFor!

applyToCanvas: aCanvas
	"overwrite the graphics parameters of aCanvas with whichever of our own settings is not nil"

	pen ifNotNil: [:it | aCanvas pen: it].
	brush ifNotNil: [:it | aCanvas brush: brush].
	font ifNotNil: [:it | aCanvas font: font].
	forecolor ifNotNil: [:it | aCanvas forecolor: forecolor].
	backcolor ifNotNil: [:it | aCanvas backcolor: backcolor].
	isTextTransparent ifTrue: [aCanvas setBkMode: TRANSPARENT].
!

backcolor
	"answer the receiver's backcolor"

	^ backcolor.
!

backcolor: aColor
	"set the receiver's backcolor"

	backcolor := aColor.
	self triggerChanged.
!

brush
	"answer the receiver's brush"

	^ brush.
!

brush: aBrush
	"set the receiver's brush to aBrush"

	brush := aBrush.
	self triggerChanged.
!

brushWithBitmap: aBitmap
	"set the receiver's brush to use aBitmap"

	self brush: (Brush bitmap: aBitmap).
!

brushWithStyle: styleFlags color: aColor hatch: hatch
	"set the receiver's brush to one with the given attributes.
	The style flags are combinations of:
			(Win32Constants keys select: [:each | each startsWith: 'BS_']) asSortedCollection
		BS_AUTOCHECKBOX		3
		BS_AUTORADIOBUTTON		9
		BS_BITMAP				128
		BS_DEFPUSHBUTTON		1
		BS_DIBPATTERN			5
		BS_DIBPATTERN8X8		8
		BS_DIBPATTERNPT		6
		BS_GROUPBOX			7
		BS_HATCHED			2
		BS_HOLLOW			1
		BS_ICON				64
		BS_INDEXED			4
		BS_LEFTTEXT			32
		BS_MULTILINE			8192
		BS_NULL				1
		BS_PATTERN			3
		BS_PATTERN8X8			7
		BS_PUSHBUTTON			0
		BS_PUSHLIKE			4096
		BS_RADIOBUTTON			4
		BS_SOLID				0
	The hatch value is one of:
			(Win32Constants keys select: [:each | each startsWith: 'HS_']) asSortedCollection
		HS_BDIAGONAL			3
		HS_CROSS				4
		HS_DIAGCROSS			5
		HS_FDIAGONAL			2
		HS_HORIZONTAL			0
		HS_VERTICAL			1
	or may take bitmap values too"

	self brush: (Brush withStyle: styleFlags color: aColor hatch: hatch).
!

featureAt: aSymbol
	"answer the value contained in our features dictionary under the given name"

	^ features at: aSymbol.!

featureAt: aSymbol ifAbsent: a0Block
	"answer the value contained in our features dictionary for the given symbol, or the result
	of evaluating a0Block if it's missing"

	^ features at: aSymbol ifAbsent: a0Block.!

featureAt: aSymbol ifPresent: a1Block
	"if we have a feature named by aSymbol, then answer the result of evaluating a1Block, passing the 
	value of the feature as its argument"

	^ features at: aSymbol ifPresent: a1Block.!

featureAt: aSymbol put: anObject
	"set the value contained in our features dictionary to anObject (typically a size such
	as an integer or extent, or a color, or something similar)"

	features at: aSymbol put: anObject.
	self triggerChanged.!

features
	"answer our features dictionary"

	^ features ifNil: [IdentityDictionary new].
!

features: aDictionary
	"set our features map to aDictionary"

	features := aDictionary isEmpty ifTrue: [nil] ifFalse: [aDictionary].
	self triggerChanged.!

font
	"answer the receiver's font"

	^ font.
!

font: aFont
	"set the receiver's font to aFont"

	font := aFont.
	self triggerChanged.
!

forecolor
	"answer the receiver's forecolor"

	^ forecolor.
!

forecolor: aColor
	"set the receiver's forecolor"

	forecolor := aColor.
	self triggerChanged.
!

initialize
	"private -- establish a coherent initial state"

	isTextTransparent := false.

	^ super initialize.!

isTextTransparent
	"answer whether we are to draw transparent text"

	^ isTextTransparent.
!

isTextTransparent: aBool
	"set whether we are to draw transparent text"

	isTextTransparent := aBool.
	self triggerChanged.!

onCanvas: aCanvas do: a1Block
	"evaluate a0Block while temporarily overriding the graphics parameters of aCanvas
	with whichever of our own settings is not nil.  Answers the result of the evaluation"

	aCanvas save.
	[self applyToCanvas: aCanvas.
	^ a1Block value: aCanvas]
		ensure: [aCanvas restore].
!

pen
	"answer the receiver's pen"

	^ pen.
!

pen: aPen
	"set the receiver's pen to aPen"

	pen := aPen.
	self triggerChanged.
!

penWithStyle: styleFlags width: anInteger color: aColor
	"set the receiver's pen to one with the given attributes.
	The style flags are combinations of:
			(Win32Constants keys select: [:each | each startsWith: 'PS_']) asSortedCollection
		PS_ALTERNATE		8
		PS_COSMETIC		0
		PS_DASH			1
		PS_DASHDOT		3
		PS_DASHDOTDOT		4
		PS_DOT			2
		PS_ENDCAPFLAT		512
		PS_ENDCAPMASK		3840
		PS_ENDCAPROUND	0
		PS_ENDCAPSQUARE	256
		PS_GEOMETRIC		65536
		PS_INSIDEFRAME		6
		PS_JOINBEVEL		4096
		PS_JOINMASK		61440
		PS_JOINMITER		8192
		PS_JOINROUND		0
		PS_NULL			5
		PS_SOLID			0
		PS_STYLEMASK		15
		PS_TYPEMASK		983040
		PS_USERSTYLE		7
	"

	self pen: (Pen withStyle: styleFlags width: anInteger color: aColor).
!

triggerChanged
	"private -- inform whom it may concern that we have changed"

	events notNil ifTrue: [self trigger: #changed].
! !
!GraphicsSettings categoriesFor: #applyToCanvas:!CU-graphics!operations!public! !
!GraphicsSettings categoriesFor: #backcolor!accessing!public! !
!GraphicsSettings categoriesFor: #backcolor:!accessing!public! !
!GraphicsSettings categoriesFor: #brush!accessing!public! !
!GraphicsSettings categoriesFor: #brush:!accessing!public! !
!GraphicsSettings categoriesFor: #brushWithBitmap:!accessing!public! !
!GraphicsSettings categoriesFor: #brushWithStyle:color:hatch:!accessing!public! !
!GraphicsSettings categoriesFor: #featureAt:!accessing!public! !
!GraphicsSettings categoriesFor: #featureAt:ifAbsent:!accessing!public! !
!GraphicsSettings categoriesFor: #featureAt:ifPresent:!accessing!public! !
!GraphicsSettings categoriesFor: #featureAt:put:!accessing!public! !
!GraphicsSettings categoriesFor: #features!accessing!public! !
!GraphicsSettings categoriesFor: #features:!accessing!public! !
!GraphicsSettings categoriesFor: #font!accessing!public! !
!GraphicsSettings categoriesFor: #font:!accessing!public! !
!GraphicsSettings categoriesFor: #forecolor!accessing!public! !
!GraphicsSettings categoriesFor: #forecolor:!accessing!public! !
!GraphicsSettings categoriesFor: #initialize!initializing!private! !
!GraphicsSettings categoriesFor: #isTextTransparent!public!testing! !
!GraphicsSettings categoriesFor: #isTextTransparent:!accessing!public! !
!GraphicsSettings categoriesFor: #onCanvas:do:!CU-graphics!operations!public! !
!GraphicsSettings categoriesFor: #pen!accessing!public! !
!GraphicsSettings categoriesFor: #pen:!accessing!public! !
!GraphicsSettings categoriesFor: #penWithStyle:width:color:!accessing!public! !
!GraphicsSettings categoriesFor: #triggerChanged!events!private! !

!GraphicsSettings class methodsFor!

publishedAspectsOfInstances
	"Answer a LookupTable of AspectDescriptors that describe the aspects published
	by an instance of the receiver. Overridden by subclasses to add the aspects
	published locally"

	^ (super publishedAspectsOfInstances)
		add: (Aspect name: #pen) beImmutable;
		add: (Aspect name: #brush) beImmutable;
		add: (Aspect font: #font) beImmutable;
		add: (Aspect color: #forecolor) beImmutable;
		add: (Aspect color: #backcolor) beImmutable;
		add: (Aspect dictionary: #features) beImmutable;
		add: (Aspect boolean: #isTextTransparent);
		yourself.

!

publishedEventsOfInstances
	"answer our instances' published events"

	^ (super publishedEventsOfInstances)
		add: #changed;
		yourself.!

stbConvertFrom: anSTBClassFormat
	"answer a Block that will convert an old version repesented as an Array to the current
	version"

	"generic implementation that will just result in default values in any missing slots"
	^ [:slots || new |
		new := self new.
		1 to: slots size do: [:i | new instVarAt: i put: (slots at: i)].
		[new] ensure: [new := nil]].!

stbVersion
	"version 2 added #features.
	version 3 added #isTextTransparent"

	^ 3.! !
!GraphicsSettings class categoriesFor: #publishedAspectsOfInstances!constants!must strip!public! !
!GraphicsSettings class categoriesFor: #publishedEventsOfInstances!constants!events!public! !
!GraphicsSettings class categoriesFor: #stbConvertFrom:!binary filing!public! !
!GraphicsSettings class categoriesFor: #stbVersion!binary filing!public! !

GraphicsTextStyle guid: (GUID fromString: '{14E30C4F-1CEC-4A96-8F3C-ABBCBFABD750}')!
GraphicsTextStyle comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Holds style information for text.  Also can paint text with the implied style.

#CUtodo: Unify this with GraphicsSettings.'!
!GraphicsTextStyle categoriesForClass!Unclassified! !
!GraphicsTextStyle methodsFor!

alignment
	"answer our text alignment mode as a Symbol"

	^ AlignmentMap
		keyAtValue: (textFlags bitAnd: AlignmentMask)
		ifAbsent: [#TopLeft].!

alignment: aSymbol
	"set our alignment to that named by aSymbol.
	"
	self
		textFlags: (AlignmentMap at: aSymbol ifAbsent: [0])
		maskedBy: AlignmentMask.
!

applySettingsTo: aCanvas
	"apply any graphics settings that we imply to aCanvas"

	backcolor ifNotNil: [:it | aCanvas backcolor: it].
	forecolor ifNotNil: [:it | aCanvas setTextColor: it].
	font ifNotNil: [:it | aCanvas font: it].
	aCanvas setBkMode: TRANSPARENT.
!

backcolor
	"answer the receiver's backcolor"

	^ backcolor.
!

backcolor: aColorOrNil
	"set the receiver's backcolor"

	backcolor := aColorOrNil.
	self triggerChanged.
!

calculateExtentOf: aString on: aCanvas in: aRectangle
	"answer a calculated extent based on the text and the font in aCanvas.
	NB: we do not apply our insets to the given rectangle"

	| rect |

	rect := RECT new.

	"if we're in wrapping mode then we have to tell Windows how wide
	the destination is before it can possibly calculate anything"
	#CUtodo.  "allow for insets (but change #reallyPaint:on:in: too)"
	self wordWrap ifTrue: [rect right: aRectangle width].

	aCanvas
		formatText: aString
		in: rect
		flags: (DT_CALCRECT | self windowsTextFlags).

	^ rect extent.!

defaultTextFlags
	"private -- answer the default flags to use for drawing our text"

	^ DT_LEFT | DTx_TOP | DT_WORDBREAK | DT_NOPREFIX | DT_EXPANDTABS.
!

ellisionMode
	"answer our ellision mode as a Symbol"

	^ EllisionMap
		keyAtValue: (textFlags bitAnd: EllisionMask)
		ifAbsent: [#None]
!

ellisionMode: aSymbol
	"set our ellision mode to that namedf by aSymbol"

	self
		textFlags: (EllisionMap at: aSymbol ifAbsent: [0])
		maskedBy: EllisionMask.!

font
	"answer the receiver's font"

	^ font.
!

font: aFontOrNil
	"set the receiver's font to aFont"

	font := aFontOrNil.
	self triggerChanged.
!

forecolor
	"answer the receiver's forecolor"

	^ forecolor.
!

forecolor: aColorOrNil
	"set the receiver's forecolor"

	forecolor := aColorOrNil.
	self triggerChanged.
!

initialize
	"private -- establish a coherent initial state"

	textFlags := self defaultTextFlags.
	insets := Rectangle new.

	^ super initialize.!

insets
	"answer how far in we ask that text should be inset from its context"

	^ insets.!

insets: aRectangle
	"set how far in we ask that text should be inset from its context"

	insets := aRectangle ifNil: [Rectangle new].
	self triggerChanged.!

layedOver: aGraphicsTextStyle
	"answer a new instance created with the 'union' of our settings and those
	of aGraphicsTextStyle.  Settings are taken from the reciever, unless they
	are at their defaults, in which case they are taken from aGraphicsTextStyle"

	| new bits rect other |

	new := self shallowCopy.

	backcolor isNil ifTrue: [new backcolor: aGraphicsTextStyle backcolor].
	forecolor isNil ifTrue: [new forecolor: aGraphicsTextStyle forecolor].
	font isNil ifTrue: [new font: aGraphicsTextStyle font].

	"NB: here we rely on the fact that the defaults are encoded as zeros"
	bits := textFlags bitAnd: EllisionMask.
	bits = 0 ifTrue: [new textFlags: aGraphicsTextStyle textFlags maskedBy: EllisionMask].
	bits := textFlags bitAnd: AlignmentMask.
	bits = 0 ifTrue: [new textFlags: aGraphicsTextStyle textFlags maskedBy: AlignmentMask].

	rect := insets copy.
	other := aGraphicsTextStyle insets.
	rect top = 0 ifTrue: [rect top: other top].
	rect bottom = 0 ifTrue: [rect bottom: other bottom].
	rect left = 0 ifTrue: [rect left: other left].
	rect right = 0 ifTrue: [rect right: other right].
	new insets: rect.

	^ new.!

paint: aString on: aCanvas in: aRectangle
	"paint the given text on the given canvas in the way implied by our settings.
	NB: actually we will accept anything that has a sensible #displayString as
	the 'text' here, since we send #displayString before painting it"

	| saveAndRestore |

	saveAndRestore := self shouldSaveCanvas: aCanvas.

	saveAndRestore ifTrue: [aCanvas save].
	self
		prePaintOn: aCanvas in: aRectangle;
		reallyPaint: aString on: aCanvas in: aRectangle;
		postPaintOn: aCanvas in: aRectangle.
	saveAndRestore ifTrue: [aCanvas restore].!

postPaintOn: aCanvas in: aRectangle
	"private -- called after we've painted our text"

!

prePaintOn: aCanvas in: aRectangle
	"private -- called before we've painted our text"

	self applySettingsTo: aCanvas.
	backcolor ifNotNil: [:it | aCanvas eraseRectangle: aRectangle].

!

reallyPaint: aString on: aCanvas in: aRectangle
	"private -- paint the given text on the given canvas if the way implied by our settings"

	| text verticalAlignment rect |

	aString isNil ifTrue: [^ self].
	text := aString displayString.
	text isEmpty ifTrue: [^ self].

	rect := aRectangle insetBy: insets.

	verticalAlignment := textFlags bitAnd: VMask.
	verticalAlignment ~= DTx_TOP ifTrue:
		[| height padding |
		height := (self calculateExtentOf: text on: aCanvas in: rect) y.
		padding :=  rect height - height.
		verticalAlignment = DTx_CENTER ifTrue: [padding := padding // 2].
		padding > 0 ifTrue: [rect top: rect top + padding]].

	aCanvas formatText: text in: rect flags: self windowsTextFlags.
!

shouldSaveCanvas: aCanvas
	"private -- answer whether we make any changes to aCanvas's settings"

	(backcolor isNil or: [backcolor = aCanvas backcolor]) ifFalse: [^ true].
	(forecolor isNil or: [forecolor = aCanvas forecolor]) ifFalse: [^ true].
	(font isNil or: [font = aCanvas font]) ifFalse: [^ true].
	aCanvas getBkMode = TRANSPARENT ifFalse: [^ true].

	^ false.

!

textFlags
	"private -- answer our text flags"

	^ textFlags.!

textFlags: aFlagSet
	"private -- set our text flags to those indicated"

	textFlags = aFlagSet ifFalse:
		[textFlags := aFlagSet.
		self triggerChanged].!

textFlags: aFlagSet maskedBy: aMask
	"private -- ensure that the bits of our text mask indicated by aMask are set
	in aFlagSet"

	| new |

	new := (textFlags bitAnd: aMask bitInvert) bitOr: (aFlagSet bitAnd: aMask).
	self textFlags: new.!

triggerChanged
	"private -- inform whom it may concern that we have changed"

	events notNil ifTrue: [self trigger: #changed].!

windowsTextFlags
	"private -- answer the sub-set of our text flags that Windows should be allowed to see"

	^ textFlags bitAnd: VMask bitInvert.!

wordWrap
	"answer whether we are set to wrap lines that are overlong"

	^ textFlags allMask: DT_WORDBREAK.!

wordWrap: aBool
	"set whether we are set to wrap lines that are overlong"

	| new |

	new := textFlags mask: DT_WORDBREAK set: aBool.

	self textFlags: new.! !
!GraphicsTextStyle categoriesFor: #alignment!accessing!public! !
!GraphicsTextStyle categoriesFor: #alignment:!accessing!public! !
!GraphicsTextStyle categoriesFor: #applySettingsTo:!painting!public! !
!GraphicsTextStyle categoriesFor: #backcolor!accessing!public! !
!GraphicsTextStyle categoriesFor: #backcolor:!accessing!public! !
!GraphicsTextStyle categoriesFor: #calculateExtentOf:on:in:!geometry!public! !
!GraphicsTextStyle categoriesFor: #defaultTextFlags!constants!private! !
!GraphicsTextStyle categoriesFor: #ellisionMode!accessing!public! !
!GraphicsTextStyle categoriesFor: #ellisionMode:!accessing!public! !
!GraphicsTextStyle categoriesFor: #font!accessing!public! !
!GraphicsTextStyle categoriesFor: #font:!accessing!public! !
!GraphicsTextStyle categoriesFor: #forecolor!accessing!public! !
!GraphicsTextStyle categoriesFor: #forecolor:!accessing!public! !
!GraphicsTextStyle categoriesFor: #initialize!initializing!private! !
!GraphicsTextStyle categoriesFor: #insets!accessing!public! !
!GraphicsTextStyle categoriesFor: #insets:!accessing!public! !
!GraphicsTextStyle categoriesFor: #layedOver:!merging!public! !
!GraphicsTextStyle categoriesFor: #paint:on:in:!painting!public! !
!GraphicsTextStyle categoriesFor: #postPaintOn:in:!painting!private! !
!GraphicsTextStyle categoriesFor: #prePaintOn:in:!painting!private! !
!GraphicsTextStyle categoriesFor: #reallyPaint:on:in:!painting!private! !
!GraphicsTextStyle categoriesFor: #shouldSaveCanvas:!painting!private!testing! !
!GraphicsTextStyle categoriesFor: #textFlags!accessing!private! !
!GraphicsTextStyle categoriesFor: #textFlags:!accessing!private! !
!GraphicsTextStyle categoriesFor: #textFlags:maskedBy:!accessing!private! !
!GraphicsTextStyle categoriesFor: #triggerChanged!events!private! !
!GraphicsTextStyle categoriesFor: #windowsTextFlags!accessing!private! !
!GraphicsTextStyle categoriesFor: #wordWrap!accessing!public!testing! !
!GraphicsTextStyle categoriesFor: #wordWrap:!accessing!public! !

!GraphicsTextStyle class methodsFor!

alignments
	"answer a collection of all the alignments that our instances understand"

	^ #(
		#TopLeft
		#TopCenter
		#TopRight

		#CenterLeft
		#Center
		#CenterRight

		#BottomLeft
		#BottomCenter
		#BottomRight
	).
!

ellisionModes
	"answer a collection of all the elllision modes that our instances understand"

	^ #(
		#None
		#End
		#Word
		#Path
	).!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	"NB: these /must/ play nicely with of the DT_* flags that we use, so we define them
	to be be the same as some of the DT_* flags that we /don't/ use -- simple but very,
	very, hacky"
	DTx_TOP := 0.
	DTx_CENTER := DT_VCENTER.
	DTx_BOTTOM := DT_SINGLELINE.

	(AlignmentMap := LookupTable new)
		at: #TopLeft put: (DTx_TOP | DT_LEFT);
		at: #TopCenter put: (DTx_TOP | DT_CENTER);
		at: #TopRight put: (DTx_TOP | DT_RIGHT);
		at: #CenterLeft put:(DTx_CENTER | DT_LEFT);
		at: #Center put: (DTx_CENTER | DT_CENTER);
		at: #CenterRight put: (DTx_CENTER | DT_RIGHT);
		at: #BottomLeft put: (DTx_BOTTOM | DT_LEFT);
		at: #BottomCenter put: (DTx_BOTTOM | DT_CENTER);
		at: #BottomRight put: (DTx_BOTTOM | DT_RIGHT);
		shrink.
	HMask := DT_LEFT | DT_RIGHT | DT_CENTER.
	VMask := DTx_TOP | DTx_CENTER | DTx_BOTTOM.
	AlignmentMask := HMask | VMask.

	(EllisionMap := LookupTable new)
		at: #None put: 0;
		at: #End put: DT_END_ELLIPSIS;
		at: #Path put: DT_PATH_ELLIPSIS;
		at: #Word put: DT_WORD_ELLIPSIS;
		shrink.
	EllisionMask := DT_END_ELLIPSIS | DT_PATH_ELLIPSIS | DT_WORD_ELLIPSIS.!

publishedAspectsOfInstances
    	"answer the <Aspect>s published by instances of the receiver"

	^ (super publishedAspectsOfInstances)
		add: (Aspect font: #font) beImmutable;
		add: (Aspect color: #forecolor) beImmutable;
		add: (Aspect color: #backcolor) beImmutable;
		add: (Aspect name: #insets) beImmutable;
		add: (Aspect boolean: #wordWrap);
		add: (Aspect choice: #alignment from: self alignments);
		add: (Aspect choice: #ellisionMode from: self ellisionModes);
		yourself.

#CUtodo.	"add: (Aspect boolean: #isTextTransparent);"
!

publishedEventsOfInstances
	"answer our instances' published events"

	^ (super publishedEventsOfInstances)
		add: #changed;
		yourself.! !
!GraphicsTextStyle class categoriesFor: #alignments!constants!public! !
!GraphicsTextStyle class categoriesFor: #ellisionModes!constants!public! !
!GraphicsTextStyle class categoriesFor: #initialize!development!initializing!private! !
!GraphicsTextStyle class categoriesFor: #publishedAspectsOfInstances!constants!must strip!public! !
!GraphicsTextStyle class categoriesFor: #publishedEventsOfInstances!constants!events!public! !

ExamplePainter guid: (GUID fromString: '{1454FE4E-6386-49D0-BA4A-101505CCF251}')!
ExamplePainter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Just an example...

E.g:

This creates a stock GraphicsPresenter, and then tells its View to use a new ExamplePainter as its <Painter>.

	p := GraphicsPresenter show.
	p view painter: ExamplePainter new.


This uses an instance as the Model in an MVP triad.  The use of the ''SubjectPainter view'' ensures that the <Painter> used by the view will be a SubjectPainter, and since that forwards #paint to the Model, our #paint: method will be called.

	model := ExamplePainter new.
	GraphicsPresenter
		show: ''SubjectPainter view''
		on: model.


Same example, but much, much, bigger.  Note how we have to #doOwnScrolling since the area we are going to scroll around is too big for a normal ScrollingDecorator.  We also turn on drag scrolling since that is useful when (as here) the logical extent is large enough to make precise scrolling with the scrollbars difficult.

	p := GraphicsPresenter show.
	(p view)
		doOwnScrolling: true;			"has to be before #paintableExtent: !!"
		allowDragScrolling: true;
		paintableExtent: 100000@100000;
		painter: ExamplePainter new.'!
!ExamplePainter categoriesForClass!Unclassified! !
!ExamplePainter methodsFor!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted"

	| main viewport top left bottom right rect canvas redBrush blueBrush |

	canvas := aPaintRequest canvas.
	redBrush := Color red brush.
	blueBrush := Color blue brush.

	"this won't be visible unless the destination of our painting allows
	painting outside the viewport.  E.g. a PaintableView with non-zero
	insets AND #allowPaintInInsets set to true"
	main := aPaintRequest rectangle.
	canvas frameRectangle: (main insetBy: -1) brush: redBrush.


	"in this example we paint the paint request's #viewport -- the visible area of the screen.
	The code would be simpler if we painted the entire #rectangle (the logical area that
	the #viewport is a 'view' of), but that would be very slow if we have a large #rectangle.
	Alternatively we could paint just the #damage region of the paint request which would
	be even faster in most cases, but then the code would be even more complicated"

	viewport := aPaintRequest viewport.
	top := viewport top truncateTo: 100.
	left := viewport left truncateTo: 100.
	right := viewport right roundUpTo: 100.
	bottom := viewport bottom roundUpTo: 100.

	rect := Rectangle new.
	top to: bottom - 1 by: 100 do:
		[:y || row |
		row := y // 100.
		rect top: y; bottom: y + 100.
		left to: right - 1 by: 100 do:
			[:x || col brush |
			col := x // 100.
			rect left: x; right: x + 100.
			brush := (row + col) odd ifTrue: [blueBrush] ifFalse: [redBrush].
			(main contains: rect) ifTrue:
				[canvas
					frameRectangle: (rect insetBy: 1)
					brush: brush.
				canvas
					formatText: ('(%d, %d)' sprintfWith: col with: row)
					in: rect
					flags: ##(DT_CENTER | DT_SINGLELINE | DT_VCENTER)]]].

	blueBrush release.
	redBrush release.! !
!ExamplePainter categoriesFor: #paint:!painting!public! !

ExamplePainter2 guid: (GUID fromString: '{5CE15297-EB31-4A65-9BC0-B50484C22B87}')!
ExamplePainter2 comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Example that displays the various paint rectangles from the PaintRequests.

E.g:
	p := GraphicsPresenter show.
	(p view)
		insets: (30@30 corner: 30@30);
		painter: ExamplePainter2 new.

Same example, but much bigger...  We tell the View to #doOwnScrolling because the extent that we will set is too big for a Dolphin ScrollingDecorator.  Also we tell it to #doSimpleScrolling, that''s because the graphics we display doesn''t scroll "normally", so we can''t take advantage of the optimised scrolling code, but have to refresh the entire viewport on each movement.

	p := GraphicsPresenter show.
	(p view)
		doOwnScrolling: true;
		doSimpleScrolling: true;
		insets: (30@30 corner: 30@30);
		paintableExtent: 10000@10000;
		painter: ExamplePainter2 new.'!
!ExamplePainter2 categoriesForClass!Unclassified! !
!ExamplePainter2 methodsFor!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted"

	| rectangle viewport damage canvas brush text |

	"in this example, we paint the paint request's #viewport in pink then
	we display the actual values of #rectangle, and #viewport, centered in the #viewport.
	Note that, since Windows clips the actual painting we do to the damage
	region, there is no point in trying to display the damage region too.  It would be
	wrong, or even garbled, because although we could attempt to print it on-screen,
	Windows wouldn't always allow the graphics to appear unless the text was actually
	in the damage region itself"

	rectangle := aPaintRequest rectangle.
	viewport := aPaintRequest viewport.
	damage := aPaintRequest damage.
	canvas := aPaintRequest canvas.

	brush := Color magenta brush.
	canvas fillRectangle: viewport brush: brush.
	brush release.

	text := (String writeStream)
			display: 'Rectangle: '; display: rectangle origin; display: ' -> '; display: rectangle corner;
			cr;
			display: 'Viewport: '; display: viewport origin; display: ' -> '; display: viewport corner;
			cr;
		"	display: 'Damage: '; display: damage origin; display: ' -> '; display: damage corner;			"
			display: 'Damage: <can''t be displayed>';
			contents.

	"hacky way to paint centred text"
	(GraphicsTextStyle new)
		wordWrap: false;
		alignment: #Center;
		paint: text on: canvas in: viewport.
! !
!ExamplePainter2 categoriesFor: #paint:!painting!public! !

PluggablePainter guid: (GUID fromString: '{FB1B0FDE-E992-4EDB-9271-DD92717C21F8}')!
PluggablePainter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Pluggable implementation of <Painter>.'!
!PluggablePainter categoriesForClass!Unclassified! !
!PluggablePainter methodsFor!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted.
	We forward to our #paintBlock"

	paintBlock value: aPaintRequest.!

paintBlock
	"answer the block we will use for painting"

	^ paintBlock.!

paintBlock: a1Block
	"private -- set the block we will use for painting"

	paintBlock := a1Block.! !
!PluggablePainter categoriesFor: #paint:!painting!public! !
!PluggablePainter categoriesFor: #paintBlock!accessing!public! !
!PluggablePainter categoriesFor: #paintBlock:!initializing!private! !

!PluggablePainter class methodsFor!

paintBlock: a1Block
	"answer a new instance that will #paint: by evaluating
	the given <monadicValuable> passing the PaintRequest
	as its parameter"

	^ (super new)
		paintBlock: a1Block;
		yourself.! !
!PluggablePainter class categoriesFor: #paintBlock:!instance creation!public! !

SubjectPainter guid: (GUID fromString: '{70D513F0-7BA2-48FE-A1FC-038EBB78E4C2}')!
SubjectPainter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Implementation of <Painter> that works by forwarding any #paint: request to the #subject of that request.'!
!SubjectPainter categoriesForClass!Unclassified! !
!SubjectPainter methodsFor!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted.
	We forward to the request's #subject"

	aPaintRequest subject paint: aPaintRequest.! !
!SubjectPainter categoriesFor: #paint:!painting!public! !

TextPainter guid: (GUID fromString: '{FE868E9C-0E2E-4D39-93CB-719305809669}')!
TextPainter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Implementation of <Painter> that works by displaying some aspect (by default, the #displayString) of the request''s subject as text.


Silly example:

	GraphicsPresenter
		show: ''TextPainter view''	"<-- already configured to use a TextPainter as its #painter:"
		on: self comment.'!
!TextPainter categoriesForClass!Unclassified! !
!TextPainter methodsFor!

getTextBlock
	"answer the <monadicValuable> we use to extract the text from PrintRequests' #subjects"

	^ getTextBlock.!

getTextBlock: a1Block
	"set the <monadicValuable> we use to extract the text from PrintRequests' #subjects"

	getTextBlock := a1Block.!

initialize
	"private -- establish a coherent initial state"

	getTextBlock := self class.
	style := GraphicsTextStyle new.

	^ super initialize.!

paint: aPaintRequest
	"called when a paintable view or similar wants to be painted.
	We paint the result of applying our #getTextBlock to the subject"

	style
		paint: (getTextBlock value: aPaintRequest subject displayString)
		on: aPaintRequest canvas
		in: aPaintRequest rectangle.!

style
	"answer the style in which we will paint our text"

	^ style.!

style: aGraphicsTextStyle
	"set the style in which we will paint our text"

	style := aGraphicsTextStyle.! !
!TextPainter categoriesFor: #getTextBlock!accessing!public! !
!TextPainter categoriesFor: #getTextBlock:!accessing!public! !
!TextPainter categoriesFor: #initialize!initializing!private! !
!TextPainter categoriesFor: #paint:!painting!public! !
!TextPainter categoriesFor: #style!accessing!public! !
!TextPainter categoriesFor: #style:!accessing!public! !

!TextPainter class methodsFor!

getTextBlock: a1Block
	"answer a new instance that will use the given <monadicValuable> to
	extract the text from PrintRequests' #subjects"

	^ (self new)
		getTextBlock: a1Block;
		yourself.!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

    	^ (super publishedAspectsOfInstances)
		add: (Aspect block: #getTextBlock);
		add: (Aspect name: #style);
		yourself.!

value: anObject
	"private -- implemented in order that we may act as our subject's default #getTextBlock"

	^ anObject displayString.! !
!TextPainter class categoriesFor: #getTextBlock:!instance creation!public! !
!TextPainter class categoriesFor: #publishedAspectsOfInstances!commands!constants!development!must strip!public! !
!TextPainter class categoriesFor: #value:!evaluating!private! !

GraphicsPresenter guid: (GUID fromString: '{FB541E8B-BC63-4411-897F-D47BE4D9636A}')!
GraphicsPresenter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Base class for presenters which use some subclass of GraphicsViewAbstract.  In point of fact this class does almost nothing except act as a place to put View Resources.'!
!GraphicsPresenter categoriesForClass!Unclassified! !
!GraphicsPresenter methodsFor!

invalidateView
	"ensure that our view is not holding on to old data in its buffer"

	self view invalidate.!

painter
	"answer the <Painter> that will be asked to painting or view"

	^ self view painter.!

refreshView
	"tell our view to refresh itself"

	self view refreshContents.! !
!GraphicsPresenter categoriesFor: #invalidateView!operations!public! !
!GraphicsPresenter categoriesFor: #painter!accessing!public! !
!GraphicsPresenter categoriesFor: #refreshView!operations!public! !

PaintingPresenter guid: (GUID fromString: '{6E3A47F7-469C-4966-AE86-92EA2C056C4A}')!
PaintingPresenter comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Base class for presenters which use a PaintableView.

PaintingPresenters are intended for the cases where the responsibility for drawing graphics can best be taken by a Presenter (as opposed to a View, or Model, or whatever).

Your can use these in one of two ways:

1)	Subclass it to provide a custom implementation of <Painter> by overriding #paint:
2)	By observing its #paintRequired: event.
3)	By overriding #painter to answer a different object that implements <Painter>.'!
!PaintingPresenter categoriesForClass!Unclassified! !
!PaintingPresenter methodsFor!

paint: aPaintRequest
	"called when our view (or something similar) wants to be painted.
	The paint request holds the data that defines where to paint
	(#canvas and #rectangle), how to paint (#features), and
	what to paint (#subject).
	Override this to paint in the canvas.  Alternatively, don't override it
	but Observe the #paintRequired: event that will
	be triggered by the default processing"

	"default is just to reflect back to our View"
	self painter paint: aPaintRequest.

#subclassResponsibility.!

painter
	"answer the <Painter> that will be asked to do our painting.
	By defalt we answer our View, thus causing paint requests
	to get the default PaintableView implementation, which is to
	trigger #paintRequired: off ourself"

	^ self view.! !
!PaintingPresenter categoriesFor: #paint:!painting!public! !
!PaintingPresenter categoriesFor: #painter!accessing!public! !

PaintingPresenter methodProtocol: #Painter attributes: #() selectors: #(#paint:)!

GraphicsViewAbstract guid: (GUID fromString: '{35032637-BBC7-4FDB-BBFA-A9174CD1EA3E}')!
GraphicsViewAbstract comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Abstract base for a number of Views that use custom graphics.  The point of this class is to abstract out the optional double-buffering, own-scrolling, and the use of the ''settings'' object, into a common service that can be inherited by concrete View classes.

Subclasses can plug in their own graphics in one of two ways:

1)	By overriding #paint: to provide a custom implementation of <Painter>.
2)	By overriding #painter to answer a different object that implements <Painter>.


The #doOwnScrolling stuff allows you to specify that the view will use its own scrollbars, rather than requiring some sort of ScrollingDecorator.  The advantages of this are:

1)	A ScrollingDecorator works by setting its client view''s #clientExtent to its #layoutExtent, and then scrolling around the large client extent -- this puts a limit to how big the scrollable area can be, since windows won''t accept values > 2**15.

2)	If the view is double-buffered, then the buffer is the same size as the #clientExtent; this puts an even tighter limit to how big the scrollable area can be -- on my machine, I can''t allocate a buffer much bigger than 3kx3k.

One disadvantage is that the implementation is more complex and hence may be slower.  Another is that since only the visible part of the display area is buffered, the paint code will be called whenever the view is scrolled (rather than just doing a refresh from a large off-screen buffer), which can obviously cause scrolling performance to suffer.  This can be mitigated by the fact that the PaintRequest contains a non-trivial #damaged rectangle, and sophisticated <Painter>s can take advantage of that and only paint the #damaged rather than the entire #viewport, or even the entire #rectangle.

The way we specify our size, and its interactiion with scrolling, is a little intricate.  We start with the "normal" Dolphin pattern, which is that we have a #layoutExtent which is used by layout managers to determine how big we should be.  The #layoutExtent is used, in particular, by ScrollingDecorator to determine how big the scrolled view should be.  The #layoutExtent is determied by first checking whether #usePreferredExtent is true, if not then it''s just the #clientExtent.  Otherwise if #preferredExtent is not nil then it''s that.  Otherwise it''s the answer from #caclulateExtent.  We don''t interfere with any of that, except that we have our own override of #calculateExtent (see below).  However, because we can do our own scrolling, and because we consider scrolling to be part of our function, not a decoration, we exend that picture.  As follows:

If #doOwnScrolling is set then we will have a #scroller which controls our scrolling for us (it''s just easier to factor it out into a separate object).  That sends us two messages in order to control and display scrollbars correctly.  These are our #scrollableExtent and our #scrollViewportExtent.  The #scrollableExtent is simply the same as our #paintableExtent, and the #scrollViewportExtent is simply the same as our #paintViewportExtent.  These are discussed in the next paragraph.  The scroller also sends us #onScrolledFrom:to: in order to get us to update our scroll position.

The #paintableExtent is the size of our underlying graphic.  If that is nil, then we answer the result of #calculatePaintableExtent, which (unless overridden) just answers our current client extent (less any insets), so our graphic will, by default, just "fit" our window, and no scrolling will ever happen (unless we have been given a larger explicit #preferredExtent /and/ there is a ScrollingDecorator wrapped around us to make us "stretch").  More typically, the #paintableExtent is set explictily either from code or in the View Composer.  The important point is that our #paintableExtent can be set separately from our #preferredExtent, so that we can, say, have a #paintableExtent of 100000@100000 (quite big!!) but a #preferredExtent of 600@400 (much smaller) that is used to set our size when we are used in a compound view.   Our #paintViewportExtent is the size of the area that our <Painter> will be asked to paint.  That is our #clientExtent less any insets.

Lastly, our implementation of #calculateExtent defaults to our #paintableExtent plus any insets.  The effect of this is that if our #usePreferredExtent is true, and no #preferredExtent has been set, then our #layoutExtent will be the same size as our graphic, plus enough to allow for our insets.

	... <whew>


TODO:
	- Find out whether there''s a good reason for the default #font being so horrible; correct if possible.
	- Document (and maybe revise) exactly what is guaranteed to be run inside a Canvas>>save/restore pair.
	- Do we want more integration with the Printing stuff ?
	- Either use #features or loose ''em !!
	- Add direct support for tracking/cursor updates.
	- Add direct support for mouse gesture interpretation ?.'!
!GraphicsViewAbstract categoriesForClass!Unclassified! !
!GraphicsViewAbstract methodsFor!

actualPaintableExtent
	"answer a Point indicating the preferred size of the rectangle that our <Painter> will be asked to paint in.
	This is determined by our #paintableExtent or, if that is not set, our #calculatePaintableExtent method"

	^ paintableExtent ifNil: [self calculatePaintableExtent].!

allowDragScrolling
	"answer whether we allow drag-scrolling"

	^ modeFlags allMask: AllowDragScrollingFlag.
!

allowDragScrolling: aBool
	"set whether we allow drag-scrolling"

	modeFlags := modeFlags mask: AllowDragScrollingFlag set: aBool.
!

allowPaintInInsets
	"answer whether we allow our <Painter> to draw in part of our client area that
	is in the border defined by our #insets"

	^ modeFlags allMask: AllowPaintInsetsFlag.
!

allowPaintInInsets: aBool
	"set whether we allow our <Painter> to draw in part of our client area that
	is in the border defined by our #insets"

	modeFlags := modeFlags mask: AllowPaintInsetsFlag set: aBool.
	self onSettingsChanged.

!

backcolor: aColorOrNil
	"set the background colour of the receiver"

	"overriden to add the backcolor to the settings"
	super backcolor: aColorOrNil.
	settings backcolor: aColorOrNil.
!

brush
	"answer the default Brush of the receiver"

	^ settings brush.
!

brush: aBrushOrNil
	"set the default Brush of the receiver"

	settings brush: aBrushOrNil.
!

calculateExtent
	"answer a calculated extent that will be used for our #layoutExtent if #usePreferredExtent
	is true, but our #preferredExtent is nil"

	^ self actualPaintableExtent + insets origin + insets corner.!

calculatePaintableExtent
	"answer a Point preferred size of the rectangle that our <Painter> will be asked to paint in.  This
	is the fallback method used if #paintableExtent is not set"

	"the default is just to answer the current paintable area"
	^ self paintViewportExtent.
!

connectObservables
	"ensure that we are Observing everything that we think we should be.
	This method exists mainly to patch up the observer system after we
	have been de-STBed, unfortunately we can't do it in #stbFixup:at: since
	we are STBed via a proxy"

	settings when: #changed send: #onSettingsChanged to: self.!

cursor
	"answer the cursor we should display given the current mouse position"

	| hScrollable vScrollable |

	#CUtodo.  "cache this"

	"NB: #hasXxxxScrollbar  is false unless #doOwnScrolling is true"
	hScrollable := self allowDragScrolling and: [self hasHorizontalScrollbar].
	vScrollable := self allowDragScrolling and: [self hasVerticalScrollbar].

	^ hScrollable
		ifTrue: [vScrollable
			ifTrue: [DragScroller freeDragableCursor]
			ifFalse: [ DragScroller horizontalDragableCursor]]
		ifFalse: [vScrollable
			ifTrue: [DragScroller verticalDragableCursor]
			ifFalse: [super cursor]].!

discardBuffer
	"private -- discard the our exisiting buffer, if any.  The buffer will be re-created lazily at need"

	buffer isNil ifFalse:
		[buffer free.
		buffer := nil].
!

doOwnScrolling
	"answer whether we do our own scrolling"

	^ modeFlags allMask: OwnScrollingFlag.!

doOwnScrolling: aBool
	"set whether we do our own scrolling"

	"note that that we don't discard our old scroller when we turn off own
	scrolling, this is so that we can maintain our scroll data, and in particular
	the scrollable extent, separately from whether we are actually using it at
	any given time"

	modeFlags := modeFlags mask: OwnScrollingFlag set: aBool.

	(aBool and: [scroller isNil]) ifTrue: [scroller := self makeDefaultScroller].

	aBool
		ifTrue: [scroller connect: self]
		ifFalse: [scroller ifNotNil: [:it | it disconnect]].!

doSimpleScrolling
	"answer whether we do our own scrolling by calling #invalidate rather than
	attempting to optimise by copynig the existing graphical info."

	^ modeFlags allMask: SimpleScrollingFlag.!

doSimpleScrolling: aBool
	"set whether we do our own scrolling by calling #invalidate rather than
	attempting to optimise by copynig the existing graphical info.
	By default this is false (so the optimised code is used).  The main reason
	to turn it off is that some graphics depend on the scroll position in complicated
	ways.
	This has no effect unless we are in own-scrolling mode."

	modeFlags := modeFlags mask: SimpleScrollingFlag set: aBool.
!

ensureRectangleVisible: aRectangle
	"if we are in #doOwnScrolling mode then ensure that as much as possible of aRectangle is scrolled
	into view, otherwise 	ignore"

	#CUtodo.
		"this will see the rectangle in logical (paint) space if we do our own scolling,
		and will therefore take account of insets, but will be in physical space if we do
		not do our own scrolling"

	self doOwnScrolling
		ifTrue: [scroller ensureRectangleVisible: aRectangle]
		ifFalse: [super ensureRectangleVisible: aRectangle].
!

erase: aCanvas in: aViewportRectangle damage: aDamageRectangle
	"private -- erase aCanvas"

	aCanvas backcolor isNil ifTrue: [^ self].

	self isEraseRequired
		ifTrue: [self eraseAll: aCanvas in: aViewportRectangle damage: aDamageRectangle]
		ifFalse: [self allowPaintInInsets
			ifFalse: [self eraseInsets: aCanvas in: aViewportRectangle damage: aDamageRectangle]].!

eraseAll: aCanvas in: aViewportRectangle damage: aDamageRectangle
	"private -- erase aCanvas.
	The co-odinates are in logical (painting) space"

	| brush |

	brush := Brush color: aCanvas backcolor.
	aCanvas
		fillRectangle: aDamageRectangle
		brush: brush.
	brush release.
!

eraseInsets: aCanvas in: aViewportRectangle damage: aDamageRectangle
	"private -- erase the insets part of aCanvas
	Note that this is called if #isEraseRequired is true; i.e. the #isEraseRequired flag only
	applies to the #paint:-able area of the window.
	Also note that both rectangles are in logical (painting)"

	| brush outer inner paint |

	(insets origin isZero and: [insets corner isZero]) ifTrue: [^ self].

	#CUtodo.  "better to use Rectangle>>areasOutside: ?"

	outer := Region rectangle: (aDamageRectangle intersect: aViewportRectangle).
	inner := Region rectangle: (aViewportRectangle insetBy: insets).
	paint := outer exclude: inner.
	outer release.
	inner release.

	brush := Brush color: aCanvas backcolor.
	aCanvas
		fillRegion: paint
		brush: brush.
	brush release.
	paint release.
!

features
	"answer our features dictionary"

	^ settings features.
!

features: aDictionary
	"set our features map to aDictionary"

	settings features: aDictionary.!

font: aFontOrNil
	"set a new font into the receiver"

	"overriden to add the font to the settings"
	super font: aFontOrNil.
	settings font: aFontOrNil.
!

forecolor
	"answer the foreground colour of the receiver"

	^ settings forecolor.
!

forecolor: aColorOrNil
	"set the foreground colour of the receiver"

	"overriden to add the forecolor to the settings"
	super forecolor: aColorOrNil.
	settings forecolor: aColorOrNil.
!

hasHorizontalScrollbar
	"answer whether we do our own scrolling *and* are small enough that we have
	a horizontal scrollbar"

#CUtodo.	"there is probably a way to ask Windows directly, but this'll do for now"
	^ self paintableRectangle width > self paintViewportExtent x.!

hasVerticalScrollbar
	"answer whether we do our own scrolling *and* are small enough that we have
	a vertical scrollbar"

#CUtodo.	"there is probably a way to ask Windows directly, but this'll do for now"
	^ self paintableRectangle height > self paintViewportExtent y.
!

initialAllowDragScrolling
	"answer whether we should allow scrolling by default"

	^ false.!

initialAllowPaintInInsets
	"answer whether we should allow our painter to paint in the insets border by default"

	^ false.!

initialDoOwnScrolling
	"answer whether we should do our own scrolling by default"

	^ false.!

initialDoSimpleScrolling
	"answer whether we should do our simple scrolling by default"

	^ false.!

initialGraphicsSettings
	"answer the settings we use by default"

	^ GraphicsSettings new.


!

initialInsets
	"answer the insets we use by default"

	^ Rectangle new.


!

initialIsDoubleBuffered
	"answer whether we should use a double-buffer by default"

	^ false.!

initialIsEraseRequired
	"answer whether we should clear the canvas before calling the repaint mechanism
	by default"

	^ true.!

initialize
	"private -- establish a coherent initial state"


	modeFlags := self initialModeFlags.
	settings := self initialGraphicsSettings.
	insets := self initialInsets.
	paintableExtent := self initialPaintableExtent.

	super initialize.

	#CUtodo.  "should we just call #connectObservables"
	settings when: #changed send: #invalidate to: self.
!

initialModeFlags
	"private -- answer an intitial setting for our modeFlags"

	| mode |

	mode := 0.
	self initialIsDoubleBuffered ifTrue: [mode := mode | DoubleBufferedFlag].
	self initialIsEraseRequired ifTrue: [mode := mode | EraseRequiredFlag].
"	self initialDoOwnScrolling ifTrue: [mode := mode | OwnScrollingFlag].		-- does not yet work"	#CUtodo.
	self initialDoSimpleScrolling ifTrue: [mode := mode | SimpleScrollingFlag].
	self initialAllowPaintInInsets ifTrue: [mode := mode | AllowPaintInsetsFlag].
	self initialAllowDragScrolling ifTrue: [mode := mode | AllowDragScrollingFlag].

	^ mode.
!

initialPaintableExtent
	"answer the #paintableExtent we use by default"

	^ nil.


!

insets
	"answer the insets of the painted graphics within our client area"

	^ insets.!

insets: aRectangle
	"set the insets of the painted graphics within our client area.
	Note that if we are in own-scrolling mode then the insets are, as it were,
	applied 'inside' the scrolling, so that there is always a border between
	the scrollbars and the actual graphics.  If own-scrolling is not in use
	(e.g. if we are inside a ScrollingDecorator) then the insets will be applied
	at the edge of the overall window, so the borders will only be visible if
	you scroll to the one of the edges of the graphic"

	insets := aRectangle ifNil: [Rectangle new].
	self invalidateLayout.!

invalidateBufferContents
	"private -- discard the data in our exisiting buffer, if any"

	(self isDoubleBuffered and: [buffer notNil]) ifTrue: [modeFlags := modeFlags mask: RefillBufferFlag set: true].
!

invalidateBufferedExtent
	"discard any data that depends on our extent"

	self
		discardBuffer;
		invalidateScrollPosition.!

invalidateCalculatedPaintableExtent
	"this causes our #paintableExtent to be recalculated if necessary"

	paintableExtent isNil ifTrue: [self invalidateCalculatedExtent].!

invalidateLayout
	"overriden to ensure that we are not holding on to invalid buffer data.
	NB: hooking this also ensures that we discard our buffer and update our scrollbars (if any)
	when our client extent changes"

	self invalidateBufferedExtent.
	super invalidateLayout.!

invalidatePaintRectangle: aRectangle
	"invalidates the specified rectangle in painting co-ordinates"

	^ self invalidateRect: (self mapRectangleFromPaintCoordinates: aRectangle).!

invalidateRect: aRectangle erase: aBool
	"overriden to ensure that we are not holding onto a stale buffer"

	#CUtodo.  "is it worth invalidating only part of the buffer ?"
	self invalidateBufferContents.

	super invalidateRect: aRectangle erase: aBool.
!

invalidateRgn: aRegion erase: aBool
	"overriden to ensure that we are not holding onto a stale buffer"

	#CUtodo.  "is it worth invalidating only part of the buffer ?"
	self invalidateBufferContents.

	super invalidateRgn: aRegion erase: aBool.
!

invalidateScrollPosition
	"update our scrollbars to reflect a new extent.
	This is equivalent to an #invalidate if we are not doing our own scrolling (just as
	a convenience)"

	self doOwnScrolling
		ifTrue: [scroller updateScrollbars]
		ifFalse: [self invalidate].
!

isDoubleBuffered
	"answer whether the receiver is double-buffered"

	^ modeFlags allMask: DoubleBufferedFlag.
!

isDoubleBuffered: aBool
	"set whether the receiver is double-buffered"

	modeFlags := modeFlags mask: DoubleBufferedFlag  set: aBool.
	self isDoubleBuffered ifFalse: [self discardBuffer].!

isEraseRequired
	"answer whether we should clear the view before asking our <Painter> to
	#paint: ourself"

	^ modeFlags allMask: EraseRequiredFlag .

!

isEraseRequired: aBool
	"set whether we should clear the view before asking our <Painter> to
	#paint: ourself"

	modeFlags := modeFlags mask: EraseRequiredFlag  set: aBool.
	self onSettingsChanged.
!

isTextTransparent
	"answer whether we are to draw transparent text"

	^ settings isTextTransparent.
!

isTextTransparent: aBool
	"set whether we are to draw transparent text"

	settings isTextTransparent: aBool.
!

makeBuffer
	"private -- answer a new buffer big enough to back our client extent.
	NB: in some cases the client extent is smaller than our graphics extent"

	| extent bitmap |

	extent := self clientExtent.
	bitmap := Bitmap compatible: self canvas extent: extent.
	bitmap handle = 0 ifTrue:
		[Notification signal: 'Failed to allocate bitmap: ' , extent displayString.
		bitmap := nil].

	^ bitmap.!

makeDefaultScroller
	"private -- answer a new GraphicsScroller that we can use to control our own
	scrolling"

	"NB: this will have a nil #scrollableExtent, so we will still take our size from
	our #layoutExtent"
	^ GraphicsScroller new.!

makePaintRequestFor:aCanvas in: aRectangle viewport: aViewportRectangle damage: aDamageRectangle
	"private -- answer a new PaintRequest configured from our state and the given Canvas and Rectangles"

	^ (PaintRequest canvas: aCanvas rectangle: aRectangle)
		subject: self model;
		features: settings features;
		viewport: aViewportRectangle;
		damage: aDamageRectangle;
		yourself.!

mapPointFromPaintCoordinates: aPoint
	"answer aPoint translated from the coordinate system that our
	<Painter>'s #paint: method will see to our client area coordinate system.
	This takes account of any #insets and (if we do our own scrolling) our scroll offset"

	^ aPoint - self paintOffset.
	!

mapPointToPaintCoordinates: aPoint
	"answer aPoint translated from our client area coordinate system (which is,
	for instance, how mouse positions are reported) to the coordinate system
	that our <Painter>'s #paint: method will see.  This takes account of any
	#insets and (if we do our own scrolling) our scroll offset"

	^ aPoint + self paintOffset.
	!

mapRectangleFromPaintCoordinates: aRectangle
	"answer aRectangle translated from the coordinate system that our
	<Painter>'s #paint: method will see to our client area coordinate system.
	This takes account of any #insets and (if we do our own scrolling) our scroll offset"

	^ aRectangle translateBy: self paintOffset negated.
	!

mapRectangleToPaintCoordinates: aRectangle
	"answer aRectangle translated from our client area coordinate system to
	the coordinate system that our <Painter>'s #paint: method will see. 
	This takes account of any #insets and (if we do our own scrolling) our scroll offset"

	^ aRectangle translateBy: self paintOffset.
	!

onCreated: anEvent
	"we override this, not because we are particularly interested in creation events per se,
	but because we need to ensure that we are watching events from our
	settings and this is as handy a place to do that as any"

	self connectObservables.

	^ super onCreated: anEvent.!

onEraseRequired: aColorEvent
	"called when the system wants us to clear the view"

	"overridden so we can suppress the default painting"
	^ (self isEraseRequired and: [self isDoubleBuffered not])
		ifTrue: [super onEraseRequired: aColorEvent]
		ifFalse: [1].!

onExtentChanged: aPoint
	"the receiver has been resized"

	self invalidateLayout.
!

onHScroll: aScrollEvent 
	"handler for a horizontal scroll event, overridden to pass it to our scroller, if any"

	^ self doOwnScrolling
		ifTrue: [scroller onHScroll: aScrollEvent]
		ifFalse: [super onHScroll: aScrollEvent].!

onLeftButtonPressed: aMouseEvent
	"handler for when the left button is pressed"

	| hScrollable vScrollable |

	self hasFocus ifFalse: [self setFocus].

	"NB: #hasXxxxScrollbar  is false unless #doOwnScrolling is true"
	hScrollable := self allowDragScrolling and: [self hasHorizontalScrollbar].
	vScrollable := self allowDragScrolling and: [self hasVerticalScrollbar].

	(hScrollable or: [vScrollable]) ifFalse:
		[^ super onLeftButtonPressed: aMouseEvent].

	hScrollable
		ifTrue: [vScrollable
			ifTrue: [self startDragScroll]
			ifFalse: [self startHorizontalDragScroll]]
		ifFalse: [vScrollable
			ifTrue: [self startVerticalDragScroll]].

	"answer 0 to suppress default processing"
	!

onMiddleButtonPressed: aMouseEvent

	self hasFocus ifFalse: [self setFocus].

	^ super onMiddleButtonPressed: aMouseEvent.!

onPaintRequired: aPaintEvent
	"a portion of the receiver window has been exposed and needs repainting.
	The supplied aPaintEvent holds details about the exposed area including the
	canvas to use"

	| damaged |

	damaged := aPaintEvent paintStruct rcPaint asRectangle.

	self validateBuffer
		ifTrue:
			[self repaintCanvasFromBuffer: aPaintEvent canvas damage: damaged]
		ifFalse:
			[damaged := self mapRectangleToPaintCoordinates: damaged.
			self repaintCanvasDirectly: aPaintEvent canvas damage: damaged].
!

onPositionChanged: aPositionEvent
	"the receiver has been positioned or resized.
	Overriden because the superclass implementation does an #invalidateLayout
	just for moves, which causes unbearable flickering whilst scrolling"

	aPositionEvent isResize ifTrue: [self onExtentChanged: aPositionEvent extent].
	self presenter trigger: #positionChanged: with: aPositionEvent.!

onRightButtonPressed: aMouseEvent

	self hasFocus ifFalse: [self setFocus].

	^ super onRightButtonPressed: aMouseEvent.!

onScrolledFrom: anOldPoint to: aNewPoint
	"invoked from our scroller (if we #doOwnScrolling) to tell us that our scroll position
	has changed from that given by anOldPoint to that given by aNewPoint"

	"simple case"
	self doSimpleScrolling ifTrue: [^ self simpleScrollFrom: anOldPoint to: aNewPoint].

	"slightly less simple case"
	self isDoubleBuffered ifFalse: [^ self scrollWindowFrom: anOldPoint to: aNewPoint].

	"otherwise we are supposed to be double-buffered"

	"do we have a valid buffer at the minute ?"
	(buffer isNil or: [modeFlags allMask: RefillBufferFlag]) ifTrue: [^ self invalidate].

	"ok, we have a valid buffer, so try to be clever about scrolling it"
	self scrollBufferFrom: anOldPoint to: aNewPoint.

	"buf now we have to tell Windows to refresh our display, but just calling
	#invalidate would discard our cleverly updated buffer and rebuild it from
	scratch, so we have to use a super-send"
	super invalidate.!

onSettingsChanged
	"private -- one of our graphics settings has changed, arrange to be re-drawn"

	self invalidate.
!

onStartup
	"make sure we are not using an out-of-date buffer"

	super onStartup.
	self refreshContents.
!

onVScroll: aScrollEvent 
	"handler for a vertical scroll event, overridden to pass it to our scroller, if any"

	^ self doOwnScrolling
		ifTrue: [scroller onVScroll: aScrollEvent]
		ifFalse: [super onVScroll: aScrollEvent].
!

paint: aPaintRequest
	"we implement this since we act as our own painter unless a subclass overrides
	#painter.  In fact we don't do anything by default (we /are/ abstract after all...)"

#subclassResponsibility.!

paintableExtent
	"answer a Point indicating the preferred size of the rectangle that our <Painter> will be asked to paint in.
	May be nil in which case the size will be determined by our #calculatePaintableExtent method"

	^ paintableExtent.!

paintableExtent: aPointOrNil
	"set the preferred size of the rectangle that our <Painter> will be asked to paint in.
	May be nil in which case the size will be determined by our #calculatePaintableExtent method"

	paintableExtent := aPointOrNil.
	self invalidateLayout.

!

paintableRectangle
	"answer a Rectangle defining the shape of our underlying graphic"

	^ 0@0 extent: self actualPaintableExtent.!

painter
	"answer the <Painter> that will be asked to paint this canvas.
	By default it is ourself"

	^ self.!

paintOffset
	"answer the difference between our client area coordinate system (which is,
	for instance, how mouse positions are reported) to the coordinate system
	that our <Painter>'s #paint: method will see.  It takes account of any
	#insets and (if we do our own scrolling) our scroll offset.
	The answered Point is the amount to subtract from a position in painting coordinates
	to get the corresponding position in client-rectangle coordinates (or the amount
	to subtract to map the other way)"

	^ self scrollOffset - insets origin.
	!

paintOnCanvas: aCanvas
	"private -- render ourself on the given given bit of aCanvas which may be our window canvas or
	our buffer"

	self paintOnCanvas: aCanvas damage: nil.!

paintOnCanvas: aCanvas damage: aRectangleOrNil
	"private -- render ourself on the given given bit of aCanvas which may be our window canvas or
	our buffer.
	Note that the damage Rectangle is in logical (painting) co-ordinates.
	We implement the request by configuring a PaintRequest and forwarding it to our <Painter>
	(which may, in fact, be ourself)"

	self painter isNil ifTrue: [^ self].

	self settings onCanvas: aCanvas do:
		[:canvas || rectangle outer inner damaged offset request |

		"work out what needs to be repainted"
		rectangle := self paintableRectangle.
		outer := self rawPaintViewport.
		inner := self paintViewport.
		damaged := aRectangleOrNil ifNil: [outer].

		"adjust the co-ordinates to allow for scrolling and insets"
		aCanvas windowOrigin: (aCanvas windowOrigin + self paintOffset).

"
Transcript clear; display: 'PaintOnCanvas'; space; display: Time millisecondClockValue; cr.
self clientExtent traceWith: '	client extent'.
rectangle traceWith: '	rectangle'.
outer traceWith: '	outer'.
inner traceWith: '	inner'.
damaged traceWith: '	damaged'.
aCanvas clipBox traceWith: '	clip box'.
aCanvas windowOrigin traceWith: '	Window origin'.
"
		"erase as much as necessary"
		self erase: canvas in: outer damage: damaged.

		"silly little optimisation, but it makes life easier for client code"
		(inner isEmpty or: [damaged isEmpty]) ifTrue: [^ self].

		"enforce no painting in the no-go zone"
		self allowPaintInInsets ifFalse: [aCanvas intersectClipRectangle: inner].

		"and -- finally -- do the real painting"
		request := self makePaintRequestFor: canvas in: rectangle viewport: inner damage: damaged.
		[self painter paint: request]
			on: Error
			do: [:err | err notify]].
!

paintViewport
	"answer visible region that our <Painter> will be asked to #paint:.
	The answer is in logical (painting) coordinates"

	^ self scrollOffset extent: self paintViewportExtent.!

paintViewportExtent
	"answer the size of the visible region that our <Painter> will be asked to #paint:"

	^ self clientExtent - insets origin - insets corner.
	!

pen
	"answer the default Pen of the receiver"

	^ settings pen.
!

pen: aPenOrNil
	"set the default Pen of the receiver"

	settings pen: aPenOrNil.
!

placement: aWINDOWPLACEMENT
	"overriden to update our scroll bars if necessary"

	super placement: aWINDOWPLACEMENT.
	self invalidateLayout.!

preTranslateKeyboardInput: aMSG
	"answer whether the receiver would like to consume the argument aMSG,
	which is a keyboard message. Handle keyboard navigation"

	^ self doOwnScrolling
		ifTrue: [scroller preTranslateKeyboardInput: aMSG]
		ifFalse: [super preTranslateKeyboardInput: aMSG].!

publishedAspects
	"answer a Set of AspectDescriptors that describe the aspects we publish"

	"we 'republish' the aspects of our settings too"
	^ super publishedAspects
		addAll: (settings publishedAspects);
		yourself.
!

queryCommand: aCommandQuery
	"private -- set the enabledness of a command"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd = #toggleDragScrolling ifTrue: [enabled := true. checked := self allowDragScrolling].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

rawPaintViewport
	"answer the visible region that our <Painter> will be asked to #paint: including the insents, if any.
	The answer is in logical (painting) coordinates"

	^ self mapRectangleToPaintCoordinates: self clientRectangle.!

repaintBuffer
	"private -- repaint our buffer"

	| canvas |

	modeFlags := modeFlags mask: RefillBufferFlag set: false.
	canvas := buffer canvas.
	[self paintOnCanvas: canvas]
		ensure: [canvas free].!

repaintCanvasDirectly: aCanvas damage: aRectangle
	"private -- repaint aCanvas directly ignoring the double-buffer, if any.
	Note: the damage rectangle is in logical (painting) coordinates"

	self paintOnCanvas: aCanvas damage: aRectangle.!

repaintCanvasFromBuffer: aCanvas damage: aRectangle
	"private -- repaint aCanvas from our buffer.
	Note: the damage rectangle is in physical coordinates"

	buffer
		drawOn: aCanvas
		at: aRectangle origin
		from: aRectangle origin
		extent: aRectangle extent.
!

resolutionScaledBy: scale
	"private -- I'm still not sure what this does..."

	insets := (insets * scale x) truncated.
	super resolutionScaledBy: scale.

!

scrollableExtent
	"answer how big an area we can be scrolled over.
	This is used by our scroller (if any)"

	^ self actualPaintableExtent.!

scrollBufferFrom: anOldPoint to: aNewPoint
	"private -- implementation of scrolling used when #doSimpleScrolling is false (and
	we are set to do our own scrolling) and we are using double-buffering.
	Note.  the caller will invalidate our window, so all we have to do here is update the
	buffer"

	| canvas paintArea blitFrom blitTo |

	canvas := buffer canvas.
	paintArea := self clientRectangle insetBy: insets.
	blitFrom := (paintArea translateBy: (aNewPoint - anOldPoint)) intersect: paintArea.

	"is there any overlap to preserve ?"
	blitFrom isEmpty ifTrue:
		[self paintOnCanvas: canvas.
		canvas free.
		^ self].

	"copy the stuff that has only been moved by the scrolling"
	blitTo := blitFrom translateBy: anOldPoint - aNewPoint.
	canvas
		bitBlt: canvas
		rectangle: blitFrom
		to: blitTo origin
		rop: SRCCOPY.

	"and refresh the bits (actually only one bit) that have been exposed"
	(paintArea areasOutside: blitTo) do:
		[:each || damaged |
		damaged := self mapRectangleToPaintCoordinates: each.
		self paintOnCanvas: canvas damage: damaged].
	
	canvas free.
!

scrollData
	"answer the data that controls how we do our own scrolling, or nil
	if we don't.
	The scroll data is actually our scroller, but it's just an accident (in a way)
	that it both holds the controlling data, and implements the resulting control"

	^ self scroller.!

scrollData: aGraphicsScrollerOrNil
	"set the data that controls how we do our own scrolling.
	The scroll data is actually our scroller, but it's just an accident (in a way)
	that it both holds the controlling data, and implements the resulting control"

	self scroller: aGraphicsScrollerOrNil.
!

scroller
	"answer the object that controls our scrolling, or nil if we don't do our oiwn scrolling"

	^ self doOwnScrolling
		ifTrue: [scroller]
		ifFalse: [nil].!

scroller: aGraphicsScrollerOrNil
	"set the object that controls our scrolling"

	| previous |

	aGraphicsScrollerOrNil isNil ifTrue: [self doOwnScrolling: false].

	previous := scroller.
	scroller := aGraphicsScrollerOrNil.

	"transfer control ?"
	(self doOwnScrolling and: [aGraphicsScrollerOrNil ~= previous]) ifTrue:
		[previous disconnect.
		scroller connect: self].

!

scrollOffset
	"if we are in #doOwnScrolling mode then answer the current scroll offset, otherwise
	answer 0@0"

	^ self doOwnScrolling
		ifTrue: [scroller scrollOffset]
		ifFalse: [0@0].
!

scrollOffset: aPoint
	"if we are in #doOwnScrolling mode then set the current scroll offset, otherwise
	ignore"

	self doOwnScrolling ifTrue: [scroller scrollTo: aPoint].!

scrollViewport
#Cutodo.
	^ self paintViewport.!

scrollViewportExtent
	"answer how much of our #scrollableExtent is currently visible.
	This is used by our scroller (if any) to determine how big the scroll thumbs should be"

	^ self paintViewportExtent.!

scrollWindowFrom: anOldPoint to: aNewPoint
	"private -- implementation of scrolling used when #doSimpleScrolling is false (and
	we are set to do our own scrolling) and we are not using double-buffering"

	| scrollArea |

	scrollArea := self clientRectangle insetBy: insets.
	self
		scrollBy: (anOldPoint - aNewPoint)
		scroll: scrollArea
		clip: scrollArea.

!

settings
	"answer the RenderingSettings of the receiver"

	^ settings.
!

simpleScrollFrom: anOldPoint to: aNewPoint
	"private -- implementation of scrolling used when #doSimpleScrolling is true (and
	we are set to do our own scrolling)"

	self invalidate!

startDragScroll
	"if we are in own-scrolling mode than start a drag-scrolling operation.
	This is ignored if we are not in that mode"

	self doOwnScrolling ifTrue: [(DragScroller for: self) start].!

startHorizontalDragScroll
	"if we are in own-scrolling mode than start a horizontal drag-scrolling operation.
	This is ignored if we are not in that mode"

	self doOwnScrolling ifTrue: [(DragScroller for: self) startHorizontal].!

startVerticalDragScroll
	"if we are in own-scrolling mode than start a vertical drag-scrolling operation.
	This is ignored if we are not in that mode"

	self doOwnScrolling ifTrue: [(DragScroller for: self) startVertical].!

stbSaveOn: anSTBOutFiler
	"overridden not to save the buffer"

	| saved |

	saved := buffer.
	buffer := nil.
	^ [super stbSaveOn: anSTBOutFiler] ensure: [buffer := saved].!

toggleDragScrolling
	"command -- toggle whether we do drag scrolling"

	self allowDragScrolling: self allowDragScrolling not.
!

validateBuffer
	"private -- ensure that our buffer exists and is up-to-date.
	Answer whether we have a buffer"

	"we may not want a buffer"
	self isDoubleBuffered ifFalse: [^ false].

	"or it may be how we want it already"
	(buffer notNil and: [(modeFlags allMask: RefillBufferFlag) not]) ifTrue: [^ true].

	"try to ensure we have a buffer"
	buffer isNil ifTrue: [buffer := self makeBuffer].

	"but that may have failed"
	buffer isNil ifTrue:
		["revert to non-buffered mode"
		modeFlags := modeFlags mask: DoubleBufferedFlag set: false.
		^ false].

	"fill it in"
	self repaintBuffer.

	^ true.! !
!GraphicsViewAbstract categoriesFor: #actualPaintableExtent!geometry!public! !
!GraphicsViewAbstract categoriesFor: #allowDragScrolling!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #allowDragScrolling:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #allowPaintInInsets!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #allowPaintInInsets:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #backcolor:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #brush!accessing!public! !
!GraphicsViewAbstract categoriesFor: #brush:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #calculateExtent!geometry!public! !
!GraphicsViewAbstract categoriesFor: #calculatePaintableExtent!geometry!public! !
!GraphicsViewAbstract categoriesFor: #connectObservables!event handling!public! !
!GraphicsViewAbstract categoriesFor: #cursor!public!tracking! !
!GraphicsViewAbstract categoriesFor: #discardBuffer!double buffering!private!validating! !
!GraphicsViewAbstract categoriesFor: #doOwnScrolling!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #doOwnScrolling:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #doSimpleScrolling!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #doSimpleScrolling:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #ensureRectangleVisible:!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #erase:in:damage:!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #eraseAll:in:damage:!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #eraseInsets:in:damage:!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #features!accessing!public! !
!GraphicsViewAbstract categoriesFor: #features:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #font:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #forecolor!accessing!public! !
!GraphicsViewAbstract categoriesFor: #forecolor:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #hasHorizontalScrollbar!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #hasVerticalScrollbar!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #initialAllowDragScrolling!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialAllowPaintInInsets!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialDoOwnScrolling!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialDoSimpleScrolling!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialGraphicsSettings!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialInsets!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialIsDoubleBuffered!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialIsEraseRequired!constants!public! !
!GraphicsViewAbstract categoriesFor: #initialize!initializing!private! !
!GraphicsViewAbstract categoriesFor: #initialModeFlags!constants!private! !
!GraphicsViewAbstract categoriesFor: #initialPaintableExtent!constants!public! !
!GraphicsViewAbstract categoriesFor: #insets!accessing!geometry!public! !
!GraphicsViewAbstract categoriesFor: #insets:!accessing!geometry!public! !
!GraphicsViewAbstract categoriesFor: #invalidateBufferContents!double buffering!private!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateBufferedExtent!double buffering!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateCalculatedPaintableExtent!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateLayout!geometry!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidatePaintRectangle:!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateRect:erase:!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateRgn:erase:!public!validating! !
!GraphicsViewAbstract categoriesFor: #invalidateScrollPosition!public!scrolling!validating! !
!GraphicsViewAbstract categoriesFor: #isDoubleBuffered!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #isDoubleBuffered:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #isEraseRequired!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #isEraseRequired:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #isTextTransparent!accessing!public!testing! !
!GraphicsViewAbstract categoriesFor: #isTextTransparent:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #makeBuffer!double buffering!private! !
!GraphicsViewAbstract categoriesFor: #makeDefaultScroller!helpers!private! !
!GraphicsViewAbstract categoriesFor: #makePaintRequestFor:in:viewport:damage:!painting!private! !
!GraphicsViewAbstract categoriesFor: #mapPointFromPaintCoordinates:!geometry!public! !
!GraphicsViewAbstract categoriesFor: #mapPointToPaintCoordinates:!geometry!public! !
!GraphicsViewAbstract categoriesFor: #mapRectangleFromPaintCoordinates:!geometry!public! !
!GraphicsViewAbstract categoriesFor: #mapRectangleToPaintCoordinates:!geometry!public! !
!GraphicsViewAbstract categoriesFor: #onCreated:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onEraseRequired:!event handling!painting!public! !
!GraphicsViewAbstract categoriesFor: #onExtentChanged:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onHScroll:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onLeftButtonPressed:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onMiddleButtonPressed:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onPaintRequired:!event handling!painting!public! !
!GraphicsViewAbstract categoriesFor: #onPositionChanged:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onRightButtonPressed:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onScrolledFrom:to:!event handling!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #onSettingsChanged!event handling!private! !
!GraphicsViewAbstract categoriesFor: #onStartup!event handling!public! !
!GraphicsViewAbstract categoriesFor: #onVScroll:!event handling!public! !
!GraphicsViewAbstract categoriesFor: #paint:!painting!public! !
!GraphicsViewAbstract categoriesFor: #paintableExtent!accessing!geometry!public! !
!GraphicsViewAbstract categoriesFor: #paintableExtent:!accessing!geometry!public! !
!GraphicsViewAbstract categoriesFor: #paintableRectangle!geometry!public! !
!GraphicsViewAbstract categoriesFor: #painter!accessing!public! !
!GraphicsViewAbstract categoriesFor: #paintOffset!geometry!public! !
!GraphicsViewAbstract categoriesFor: #paintOnCanvas:!painting!private! !
!GraphicsViewAbstract categoriesFor: #paintOnCanvas:damage:!painting!private! !
!GraphicsViewAbstract categoriesFor: #paintViewport!geometry!public! !
!GraphicsViewAbstract categoriesFor: #paintViewportExtent!geometry!public! !
!GraphicsViewAbstract categoriesFor: #pen!accessing!public! !
!GraphicsViewAbstract categoriesFor: #pen:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #placement:!geometry!public! !
!GraphicsViewAbstract categoriesFor: #preTranslateKeyboardInput:!dispatching!public! !
!GraphicsViewAbstract categoriesFor: #publishedAspects!constants!must strip!public! !
!GraphicsViewAbstract categoriesFor: #queryCommand:!commands!public! !
!GraphicsViewAbstract categoriesFor: #rawPaintViewport!geometry!public! !
!GraphicsViewAbstract categoriesFor: #repaintBuffer!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #repaintCanvasDirectly:damage:!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #repaintCanvasFromBuffer:damage:!double buffering!painting!private! !
!GraphicsViewAbstract categoriesFor: #resolutionScaledBy:!geometry!private! !
!GraphicsViewAbstract categoriesFor: #scrollableExtent!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollBufferFrom:to:!painting!private!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollData!accessing!public! !
!GraphicsViewAbstract categoriesFor: #scrollData:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #scroller!accessing!public! !
!GraphicsViewAbstract categoriesFor: #scroller:!accessing!public! !
!GraphicsViewAbstract categoriesFor: #scrollOffset!geometry!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollOffset:!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollViewport!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollViewportExtent!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #scrollWindowFrom:to:!painting!private!scrolling! !
!GraphicsViewAbstract categoriesFor: #settings!accessing!public! !
!GraphicsViewAbstract categoriesFor: #simpleScrollFrom:to:!painting!private!scrolling! !
!GraphicsViewAbstract categoriesFor: #startDragScroll!operations!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #startHorizontalDragScroll!operations!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #startVerticalDragScroll!operations!public!scrolling! !
!GraphicsViewAbstract categoriesFor: #stbSaveOn:!binary filing!public! !
!GraphicsViewAbstract categoriesFor: #toggleDragScrolling!commands!public! !
!GraphicsViewAbstract categoriesFor: #validateBuffer!double buffering!private!validating! !

GraphicsViewAbstract methodProtocol: #Painter attributes: #() selectors: #(#paint:)!

!GraphicsViewAbstract class methodsFor!

convertFromVersion10: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 10 to version 11"

	| mode doubleBufferingMode isEraseRequired |

	"we replaced doubleBufferingMode and isEraseRequired by bits in a bitset"
	doubleBufferingMode := anArray at: 15.
	isEraseRequired := anArray at: 16.

	mode := 0.
	doubleBufferingMode = #NoBuffer ifFalse: [mode := mode | DoubleBufferedFlag].
	isEraseRequired ifTrue: [mode := mode | EraseRequiredFlag].
	anArray at: 15 put: mode.
	anArray at: 16 put: nil.

	^ anArray.!

convertFromVersion11: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 11 to version 12"

	"actually, no conversion is required"
	^ anArray.!

convertFromVersion12: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 12 to version 13"

	| new |

	"we replaced the last of the dummy vars with paintableExtent and added three new dummies"
	new := OrderedCollection withAll: anArray.
	new at: 18 put: nil.	"dont copy the dummy"
	3 timesRepeat: [new add: nil afterIndex: 18].

	^ new asArray.!

convertFromVersion9: anArray
	"private -- answer an array of instvars derived from that given and reflecting the changes from version 9 to version 10"

	| mode |

	"we replaced the boolean isDoubleBuffered by a symbol mode"
	mode := (anArray at: 15)
			ifTrue: [#FitClientExtent]
			ifFalse: [#NoBuffer].
	anArray at: 15 put: mode.

	"and replaced one of the dummy vars with insets"
	anArray at: 17 put: Rectangle new.

	^ anArray.!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	| bit |

	"NB: these values are used in stored View resources -- don't change them"
	bit := -1.
	DoubleBufferedFlag  := 1 << (bit := bit+1).
	EraseRequiredFlag := 1 << (bit := bit+1).
	RefillBufferFlag:= 1 << (bit := bit+1).
	OwnScrollingFlag := 1 << (bit := bit+1).
	AllowPaintInsetsFlag := 1 << (bit := bit+1).
	SimpleScrollingFlag := 1 << (bit := bit+1).
	AllowDragScrollingFlag := 1 << (bit := bit+1).
!

publishedAspectsOfInstances
	"answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	^ super publishedAspectsOfInstances

		add: (Aspect name: #insets) beImmutable;
		add: (Aspect name: #scrollData) beImmutable;
		add: (Aspect name: #paintableExtent) beImmutable;
		add: ((Aspect boolean: #isDoubleBuffered) isNullable: false; yourself);
		add: ((Aspect boolean: #isEraseRequired) isNullable: false; yourself);
		add: ((Aspect boolean: #doOwnScrolling) isNullable: false; yourself);
		add: ((Aspect boolean: #doSimpleScrolling) isNullable: false; yourself);
		add: ((Aspect boolean: #allowPaintInInsets) isNullable: false; yourself);
		add: ((Aspect boolean: #allowDragScrolling) isNullable: false; yourself);

		yourself.
!

stbConvert: anArray fromVersion: anInteger
	"private -- convert from earlier version by updating and answering the array of instance
	variables' values"

	| slots |

	slots := super stbConvert: anArray fromVersion: anInteger.

	anInteger <= 9 ifTrue: [slots := self convertFromVersion9: slots].
	anInteger <= 10 ifTrue: [slots := self convertFromVersion10: slots].
	anInteger <= 11 ifTrue: [slots := self convertFromVersion11: slots].
	anInteger <= 12 ifTrue: [slots := self convertFromVersion12: slots].

	^ slots.!

stbVersion
	"
	version 9 was the intial one (number inherited from View)
	version 10 replaced the boolean isDoubleBuffer with doubleBuifferingMode and added insets.
	version 11 removed doubleBufferingMode and replaced it and isEraseRequired with a bitset.
	version 12 replaced one of the dummy vars with scroller.
	version 13 replaced the last dummy vars with paintableExtent, and added three new dummy vars.
	"

	^ 13.! !
!GraphicsViewAbstract class categoriesFor: #convertFromVersion10:!binary filing!private! !
!GraphicsViewAbstract class categoriesFor: #convertFromVersion11:!binary filing!private! !
!GraphicsViewAbstract class categoriesFor: #convertFromVersion12:!binary filing!private! !
!GraphicsViewAbstract class categoriesFor: #convertFromVersion9:!binary filing!private! !
!GraphicsViewAbstract class categoriesFor: #initialize!initializing!private! !
!GraphicsViewAbstract class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!GraphicsViewAbstract class categoriesFor: #stbConvert:fromVersion:!binary filing!private! !
!GraphicsViewAbstract class categoriesFor: #stbVersion!binary filing!public! !

PaintableView guid: (GUID fromString: '{F1A84895-0C9A-4E0E-BB56-0F040CE2CFE2}')!
PaintableView comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

Simple view that implements <Painter> by asking its Presenter to paint on it.

Note that instances do *not* make use of their model.  The purpose of these things is to be drawn on by their Presenter, which should be a PaintingPresenter (of which you observe the #paintRequired: event in order to do the necessary painting) or a subclass that overrides #paint: to do it directly.'!
!PaintableView categoriesForClass!Unclassified! !
!PaintableView methodsFor!

paint: aPaintRequest
	"implement <Painter> triggering #paintRequired: off our Presenter.
	Note, we'll only actually get here if we are acting as our own Presenter,
	or if the Presenter has forwarded #paint: back to us (which PaintingPresenters
	do by default)"

	self presenter trigger: #paintRequired: with: aPaintRequest.!

painter
	"answer the <Painter> that will be asked to paint this canvas.
	We expect to be painted on by our Presenter, so we answer that"

	^ self presenter.! !
!PaintableView categoriesFor: #paint:!painting!public! !
!PaintableView categoriesFor: #painter!accessing!public! !

!PaintableView class methodsFor!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	PaintingPresenter addView: self asResource: 'Default view'.
!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by our instances.
	NB: the events are triggered off the instances' #presenters"

	^ (super publishedEventsOfInstances)
		add: #paintCanvasRequired:in:;
		yourself.
! !
!PaintableView class categoriesFor: #installViewResources!development!helpers!must strip!private! !
!PaintableView class categoriesFor: #publishedEventsOfInstances!constants!development!events!public! !

PaintingView guid: (GUID fromString: '{11BA4F6A-F0C6-4798-ACC6-BDF3AE6E316F}')!
PaintingView comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Simple view that implements <Painter> by holding a reference to a <Painter> and asking to #paint the Model on the canvas.

An instance configured with a SubjectPainter will end up asking its Model to #paint: itself.

An instance configured with a TextPainter will end up painting the Model''s #displayString, and will thus act much like a StaticText.
'!
!PaintingView categoriesForClass!Unclassified! !
!PaintingView methodsFor!

connectModel
	"connect the receiver to its model, wiring events, etc. Overridden as a reminder that
	we ought to be doing something..."

	super connectModel.
!

initialize
	"private -- establish a coherent initial state"

	painter := self initialPainter.

	^ super initialize.
!

initialPainter
	"answer the <Painter> to use by default"

	^ TextPainter new.!

paint: aPaintRequest
	"implement <Painter> triggering #paintRequired: off our Presenter.
	We just foreward the request to the <Painter> we have been configured
	with"

	"the request is already configured with our model as its #subject"
	painter ifNotNil: [:it | it paint: aPaintRequest].!

painter
	"answer the <Painter> that will paint this view"

	^ painter.
!

painter: aPainter
	"set the <Painter> that will paing this view"

	painter := aPainter.
	self invalidate.! !
!PaintingView categoriesFor: #connectModel!initializing!models!public! !
!PaintingView categoriesFor: #initialize!initializing!private! !
!PaintingView categoriesFor: #initialPainter!constants!public! !
!PaintingView categoriesFor: #paint:!painting!public! !
!PaintingView categoriesFor: #painter!accessing!public! !
!PaintingView categoriesFor: #painter:!accessing!public! !

!PaintingView class methodsFor!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	| ri v extent |

	extent := 400@300.

	ri := ResourceIdentifier class: GraphicsPresenter name: 'Default view'.
	ri owningClass addView: self asResource: ri name.
	v := ri load.
	v
		isDoubleBuffered: true;
		extent: extent.
	ri save: v.

	ri := ResourceIdentifier class: GraphicsPresenter name: 'TextPainter view'.
	ri owningClass addView: self asResource: ri name.
	v := ri load.
	v
		painter: TextPainter new;
		isDoubleBuffered: true;
		extent: extent.
	ri save: v.

	ri := ResourceIdentifier class: GraphicsPresenter name: 'SubjectPainter view'.
	ri owningClass addView: self asResource: ri name.
	v := ri load.
	v
		painter: SubjectPainter new;
		isDoubleBuffered: true;
		extent: extent.
	ri save: v.
!

publishedAspectsOfInstances
	"answer a Set of AspectDescriptors that describe the aspects published
	by  instances of the receiver"

	| choices |

	"it'd be better, formally, to scan for classes implementing <Painter> but that would pick up too many irrelevant classes"
	choices := Painter allSubclasses asSortedCollection collect: [:each | 'new ' , each name].

	^ super publishedAspectsOfInstances
		add: (Aspect name: #painter chooseFrom: choices);
		yourself.
! !
!PaintingView class categoriesFor: #installViewResources!development!helpers!must strip!private! !
!PaintingView class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: GraphicsPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAJYCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAENVIEdyYXBoaWNzIEJhc2VSAAAADAAAAFBhaW50aW5nVmlld2IAAAAW
AAAAAAAAAAAAAABiAAAAAgAAAAEAAAgBAAAAoAEAAAAAAAAAAAAAAAAAAAUAAAAAAAAAAAAAAAAA
AACgAQAARggQAAMAAABHcmFwaGljc1NldHRpbmdzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAgAAAAAAAAAAcAAAAAAAAABgIJAFJlY3RhbmdsZQAAAAAGAgUAUG9pbnQAAAAAAQAAAAEA
AABCAgAAAAAAAAEAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAYCCwBUZXh0UGFpbnRlcgAAAABwAgAA
RgYRAAEAAABHcmFwaGljc1RleHRTdHlsZQAAAAAAAAAAAAAAAAAAAAAAAAAAIgIAAAAAAABCAgAA
AAAAAAEAAAABAAAAQgIAAAAAAAABAAAAAQAAAKEQAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVh
dGVBdDpleHRlbnQ6YgAAAAIAAABCAgAAAAAAAAEAAAABAAAAQgIAAAAAAAAhAwAAWQIAAKABAAAG
AQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////////
/wAAAAAAAAAAkAEAACwBAADKAAAAAAAAANAAAABiAAAAAAAAAEICAAAAAAAAwQAAAMEAAAAAAAAA
GwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAA
AAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAA
AABSAAAABwAAAGN1cnJlbnRSAAAACAAAAFZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VM
aWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: GraphicsPresenter name: 'SubjectPainter view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAADwCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAENVIEdyYXBoaWNzIEJhc2VSAAAADAAAAFBhaW50aW5nVmlld2IAAAAW
AAAAAAAAAAAAAABiAAAAAgAAAAEAAAgBAAAAoAEAAAAAAAAAAAAAAAAAAAUAAAAAAAAAAAAAAAAA
AACgAQAARggQAAMAAABHcmFwaGljc1NldHRpbmdzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAgAAAAAAAAAAcAAAAAAAAABgIJAFJlY3RhbmdsZQAAAAAGAgUAUG9pbnQAAAAAAQAAAAEA
AABCAgAAAAAAAAEAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAYADgBTdWJqZWN0UGFpbnRlcgAAAAAG
AQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5k
AAAAALoAAAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRlbnQ6YgAAAAIAAABCAgAAAAAAAAEAAAAB
AAAAQgIAAAAAAAAhAwAAWQIAAKABAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAA
AAAAAAAAAAAA/////////////////////wAAAAAAAAAAkAEAACwBAADKAAAAAAAAANAAAABiAAAA
AAAAAEICAAAAAAAAwQAAAMEAAAAAAAAAGwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBT
VEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VS
ZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAACAAAAFZpZXcuaWNv
DgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIw
MDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: GraphicsPresenter name: 'TextPainter view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAJYCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAENVIEdyYXBoaWNzIEJhc2VSAAAADAAAAFBhaW50aW5nVmlld2IAAAAW
AAAAAAAAAAAAAABiAAAAAgAAAAEAAAgBAAAAoAEAAAAAAAAAAAAAAAAAAAUAAAAAAAAAAAAAAAAA
AACgAQAARggQAAMAAABHcmFwaGljc1NldHRpbmdzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAgAAAAAAAAAAcAAAAAAAAABgIJAFJlY3RhbmdsZQAAAAAGAgUAUG9pbnQAAAAAAQAAAAEA
AABCAgAAAAAAAAEAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAYCCwBUZXh0UGFpbnRlcgAAAABwAgAA
RgYRAAEAAABHcmFwaGljc1RleHRTdHlsZQAAAAAAAAAAAAAAAAAAAAAAAAAAIgIAAAAAAABCAgAA
AAAAAAEAAAABAAAAQgIAAAAAAAABAAAAAQAAAKEQAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVh
dGVBdDpleHRlbnQ6YgAAAAIAAABCAgAAAAAAAAEAAAABAAAAQgIAAAAAAAAhAwAAWQIAAKABAAAG
AQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////////
/wAAAAAAAAAAkAEAACwBAADKAAAAAAAAANAAAABiAAAAAAAAAEICAAAAAAAAwQAAAMEAAAAAAAAA
GwAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAA
AAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAA
AABSAAAABwAAAGN1cnJlbnRSAAAACAAAAFZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VM
aWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: PaintingPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAACMCAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAENVIEdyYXBoaWNzIEJhc2VSAAAADQAAAFBhaW50YWJsZVZpZXdiAAAA
EgAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAAAoAEAAAAAAAAAAAAAAAAAAAUAAAAA
AAAAAAAAAAAAAACgAQAARggQAAMAAABHcmFwaGljc1NldHRpbmdzAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAgAAAAAAAAAAUAAAAAAAAABgIJAFJlY3RhbmdsZQAAAAAGAgUAUG9pbnQA
AAAAAQAAAAEAAABSAgAAAAAAAAEAAAABAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAAygAA
AAAAAADQAAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNyZWF0
ZUF0OmV4dGVudDpiAAAAAgAAAFICAAAAAAAAAQAAAAEAAABSAgAAAAAAAMkAAADJAAAAoAEAAAYB
DwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////////////
AAAAAAAAAABkAAAAZAAAAMoAAAAAAAAA0AAAAGIAAAAAAAAAUgIAAAAAAADBAAAAwQAAAAAAAAAZ
AAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAA
AAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAA
AFIAAAAHAAAAY3VycmVudFIAAAAIAAAAVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxp
YnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

