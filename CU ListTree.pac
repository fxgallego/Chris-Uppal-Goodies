| package |
package := Package name: 'CU ListTree'.
package paxVersion: 0;
	basicComment: 'Copyright � Chris Uppal, 2002-2005.
chris.uppal@metagnostic.org

ListTree adds a new kind of View that is a hybrid of ListView and TreeView; it allows you to use ListViewColumns in a TreeView-like presentation.  See the class comment on ListTreeView for some examples and more details.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

I make an exception for Object-Arts -- I''d be very pleased if they were to fold this (or, better, something based on this) into the main image.

Please let me know when you find any problems.

	-- chris


Known Bugs:
-	#Node{Added/Expanded}: notifications are wrongly suppressed during #onItem:movedToParent:
-	Using non-virtual ListTrees with column images causes Windows errors (apparently fixed in Dolphin 5.0.3).
-	Windows always leaves space for an icon in the tree column, even if we don''t want one.


History:

9.00
-	Now use PackageResourceLocator for icons, etc.

8.00
-	Minor tweak to #onItem:removedFromParent:.
-	Minor tweak to #onTreeChanged:.

7.00
-	Fix for #recreate loosing connection to treeModel (seems to have been introduced in pl3 or 4).
-	Minor fix to ListTreePresenter>>up
-	Fixed rare bug in #onTreeChanged that could use model''s list object as the "roots" instead of making a copy.
-	Performace tweak closing big trees.
-	Now do an updateStates in smart refresh.
-	Changes to supprt EditableListTreeView.

6.00
-	Fixed bug handling #treeChanged: notification with virtual tree models.
-	Added #useSmartUpdate aspect and #refreshFromModel[Below:].

5.00
-	Minor cosmetic enhancements.
-	Added limited ability to configure open/closed images.
		See #stateImageBlock and #useIconsLikeWinXP.
-	Removed workaround for bug in Dolphin ListView pre 5.0.3.
-	Removed methods deprecated in v3.00.

4.00
-	Changes to examples for D5 pl3 (sort blocks).
-	Minor cosmetic improvement (*very* minor!!).

3.00
-	Uses the Tree/ListModel''s search policy instead of identity comparisons.
		My thanks to Pieter Emmelot for the suggestion.
-	The ''hidden-expanded'' set is no longer Weak (required by the above).
-	Slightly improved consistancy with [Moen]TreeViews'' public protocols.
-	Better emulation of {Tree/List}Views'' #selectionChanging: events.
-	Deprecated:
		TreeListView>>ensureDisplayed:
		TreeListView>>toggle{Expanded/ExpandAll}:
		TreeListView class>>default{List/Tree}ModelClass:

2.00
-	Bugfixes including several suggested by Steve Waring (Thanks Steve!!).
-	Speeded up expanding nodes with lots of children.
-	Completely reimplemented #onItem:movedToParent:.
-	Completely reimplemented #onItem:removedFromParent:.

1.00
-	First release.
'.

package basicPackageVersion: '9.00'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU ListTree'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\ListTreeIconsForXP.bmp''
	).
!!'.
package basicScriptAt: #preinstall put: '(Smalltalk at: #CommCtrlConstants)
"	at: ''CDRF_NOTIFYPOSTPAINT''	put: 16r00000010;
	at: ''CDRF_NOTIFYPOSTERASE''	put: 16r00000040;
	at: ''CDDS_PREPAINT''		put: 16r00000001;
	at: ''CDDS_POSTPAINT''		put: 16r00000002;
	at: ''CDDS_PREERASE''		put: 16r00000003;
	at: ''CDDS_POSTERASE''		put: 16r00000004;
	at: ''CDDS_ITEM''			put: 16r00010000;
	at: ''CDDS_ITEMPREPAINT''	put: 16r00010001;
	at: ''CDDS_ITEMPOSTPAINT''	put: 16r00010002;
	at: ''CDDS_ITEMPREERASE''	put: 16r00010003;
	at: ''CDDS_ITEMPOSTERASE''	put: 16r00010004;
	at: ''CDDS_SUBITEM''		put: 16r00020000;
	at: ''LVS_EX_UNDERLINEHOT''	put: 16r00000800;
	at: ''LVS_EX_UNDERLINECOLD''	put: 16r00001000;
"	yourself.
!!'.

package classNames
	add: #ListTreePresenter;
	add: #ListTreeView;
	add: #MutipleSelectionListTreeView;
	add: #VirtualTreeModelWithSearchPolicy;
	yourself.

package methodNames
	add: #ListView -> #lvmGetCallbackMask;
	add: #ListView -> #lvmSetCallbackMask:;
	add: #LVHITTESTINFO -> #isAboveClientArea;
	add: #LVHITTESTINFO -> #isBelowClientArea;
	add: #LVHITTESTINFO -> #isNowhere;
	add: #LVHITTESTINFO -> #isOnItem;
	add: #LVHITTESTINFO -> #isOnItemIcon;
	add: #LVHITTESTINFO -> #isOnItemLabel;
	add: #LVHITTESTINFO -> #isOnItemStateIcon;
	add: #LVHITTESTINFO -> #isToLeft;
	add: #LVHITTESTINFO -> #isToRight;
	add: #LVITEM -> #stateImageIndex;
	add: #LVITEM -> #stateImageIndex:;
	yourself.

package globalNames
	add: #ListTreeConstants;
	yourself.

package resourceNames
	add: #ListPresenter -> 'ListTree view';
	add: #ListPresenter -> 'Multi-selection ListTree view';
	add: #TreePresenter -> 'ListTree view';
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ListPresenter -> 'ListTree view';
	add: #ListPresenter -> 'Multi-selection ListTree view';
	add: #ListTreePresenter -> 'Default view';
	add: #ListTreePresenter -> 'Multi-selection view';
	add: #TreePresenter -> 'ListTree view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Package-relative File Locator';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	yourself).

package!

"Class Definitions"!

VirtualTreeModel subclass: #VirtualTreeModelWithSearchPolicy
	instanceVariableNames: 'searchPolicy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListPresenter subclass: #ListTreePresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListView subclass: #ListTreeView
	instanceVariableNames: 'hasExplicitTreeModel treeModel dummy3 expanded hiddenExpanded options indentSeparation getChildrenBlock hasChildrenBlock getParentBlock sortBlock doSortChildrenBlock dummy2 dummy1'
	classVariableNames: ''
	poolDictionaries: 'ListTreeConstants'
	classInstanceVariableNames: 'stateImageManager stateImageBlock'!
MultipleSelectionListView subclass: #MutipleSelectionListTreeView
	instanceVariableNames: 'hasExplicitTreeModel treeModel dummy3 expanded hiddenExpanded options indentSeparation getChildrenBlock hasChildrenBlock getParentBlock sortBlock doSortChildrenBlock dummy2 dummy1'
	classVariableNames: ''
	poolDictionaries: 'ListTreeConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ListView methodsFor!

lvmGetCallbackMask
	"Private - Answers the callback mask of the underlying control"

#CUadded.
	^self sendMessage: LVM_GETCALLBACKMASK.
!

lvmSetCallbackMask: anInteger
	"Private - Answers the callback mask of the underlying control"

#CUadded.
	^self sendMessage: LVM_SETCALLBACKMASK wParam: anInteger.
! !
!ListView categoriesFor: #lvmGetCallbackMask!accessing!private! !
!ListView categoriesFor: #lvmSetCallbackMask:!accessing!private! !

!LVHITTESTINFO methodsFor!

isAboveClientArea
	"Answers whether the position is above the control's client area."
#CUadded.
	^ self flags allMask: LVHT_ABOVE.!

isBelowClientArea
	"Answers whether the position is below the control's client area."
#CUadded.
	^ self flags allMask: LVHT_BELOW.!

isNowhere
	"Answers whether the position is in the control's client area but not over an item."
#CUadded.
	^ self flags allMask: LVHT_NOWHERE.!

isOnItem
	"Answers whether the position is over an item's label, icon, or state icon."
#CUadded.
	^ self flags anyMask: LVHT_ONITEM.!

isOnItemIcon
	"Answers whether the position is over an item's icon."

#CUadded.

	"if the hit is on the same line as the item, but not over any of its formal
	elements, then Window's seems to set all the LVHT_ISONITEM* flags"
	^ (self flags bitAnd: LVHT_ONITEM) = LVHT_ONITEMICON.!

isOnItemLabel
	"Answers whether the position is over an item's label."

#CUadded.

	"if the hit is on the same line as the item, but not over any of its formal
	elements, then Window's seems to set all the LVHT_ISONITEM* flags"
	^ (self flags bitAnd: LVHT_ONITEM) = LVHT_ONITEMLABEL.!

isOnItemStateIcon
	"Answers whether the position is over an item's state icon."

#CUadded.

	"if the hit is on the same line as the item, but not over any of its formal
	elements, then Window's seems to set all the LVHT_ISONITEM* flags"
	^ (self flags bitAnd: LVHT_ONITEM) = LVHT_ONITEMSTATEICON.!

isToLeft
	"Answers whether the position is to the left of the control's client area."
#CUadded.
	^ self flags allMask: LVHT_TOLEFT.!

isToRight
	"Answers whether the position is to the right of the control's client area."
#CUadded.
	^ self flags allMask: LVHT_TORIGHT.! !
!LVHITTESTINFO categoriesFor: #isAboveClientArea!public!testing! !
!LVHITTESTINFO categoriesFor: #isBelowClientArea!public!testing! !
!LVHITTESTINFO categoriesFor: #isNowhere!public!testing! !
!LVHITTESTINFO categoriesFor: #isOnItem!public!testing! !
!LVHITTESTINFO categoriesFor: #isOnItemIcon!public!testing! !
!LVHITTESTINFO categoriesFor: #isOnItemLabel!public!testing! !
!LVHITTESTINFO categoriesFor: #isOnItemStateIcon!public!testing! !
!LVHITTESTINFO categoriesFor: #isToLeft!public!testing! !
!LVHITTESTINFO categoriesFor: #isToRight!public!testing! !

!LVITEM methodsFor!

stateImageIndex
	"Answer contents' state image index."
#CUadded.

	self dwState bitShift: -12.!

stateImageIndex: anInteger
	"Set contents' state image index to anInteger."
#CUadded.

	self
		dwState: ((anInteger bitShift: 12) bitOr: self dwState);
		maskIn: self stateValidMask;
		stateMask: (LVIS_STATEIMAGEMASK bitOr: self stateMask).! !
!LVITEM categoriesFor: #stateImageIndex!accessing!public! !
!LVITEM categoriesFor: #stateImageIndex:!accessing!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #ListTreeConstants put: (PoolConstantsDictionary named: #ListTreeConstants)!
ListTreeConstants at: 'DisableExpandAllMask' put: 16r1!
ListTreeConstants at: 'DoSortRootsMask' put: 16r10!
ListTreeConstants at: 'HasButtonsMask' put: 16r2!
ListTreeConstants at: 'HasLinesAtRootMask' put: 16r4!
ListTreeConstants at: 'HasLinesMask' put: 16r8!
ListTreeConstants at: 'UseSmartRefreshMask' put: 16r20!
ListTreeConstants shrink!

"Classes"!

VirtualTreeModelWithSearchPolicy guid: (GUID fromString: '{4E93F4D0-0656-41BB-AE75-B86AF62BE94D}')!
VirtualTreeModelWithSearchPolicy comment: 'Copyright � Chris Uppal, 2003.
chris.uppal@metagnostic.org

The *only* point of this class is to make the search policy pluggable, which is needed by ListTreeView.'!
!VirtualTreeModelWithSearchPolicy categoriesForClass!Unclassified! !
!VirtualTreeModelWithSearchPolicy methodsFor!

searchPolicy
	"Answer the <searchPolicy> used to compare and search for elements by the receiver."

	^searchPolicy!

searchPolicy: aSearchPolicy
	"private -- set the search policy that we will use.  Actually the search policy is
	only used to implement #keyOfNode:in:ifAbsent, but it is also used by TreeListView
	(which is why this class exists at all)"

	searchPolicy := aSearchPolicy.! !
!VirtualTreeModelWithSearchPolicy categoriesFor: #searchPolicy!accessing!public! !
!VirtualTreeModelWithSearchPolicy categoriesFor: #searchPolicy:!initializing!private! !

!VirtualTreeModelWithSearchPolicy class methodsFor!

defaultSearchPolicy
	"answer the default SearchPolicy> used by our instances"

	^ SearchPolicy identity.!

withRoots: aCollection 
	"overriden to answer an instance with the default search policy (identity)"

	^ (super withRoots: aCollection)
		searchPolicy: self defaultSearchPolicy;
		yourself.!

withRoots: aCollection searchPolicy: aSearchPolicy
	"answer a new instance with the given roots, and search policy"

	^ (super withRoots: aCollection)
		searchPolicy: aSearchPolicy;
		yourself.! !
!VirtualTreeModelWithSearchPolicy class categoriesFor: #defaultSearchPolicy!constants!public! !
!VirtualTreeModelWithSearchPolicy class categoriesFor: #withRoots:!instance creation!public! !
!VirtualTreeModelWithSearchPolicy class categoriesFor: #withRoots:searchPolicy:!instance creation!public! !

ListTreePresenter guid: (GUID fromString: '{B4F22156-DF92-493F-81C9-6E74FD4FDCCE}')!
ListTreePresenter comment: 'Copyright � Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

These presenters are intended to pair with ListTreeViews, however in almost all cases they are not needed since it suffices to clip a ListPresenter or TreePresenter to the TreeView.  One case where that wouldn''t work is where you need a TreePresenter that understands that the selection may be multiple (which TreePresenter doesn''t) *and* knows about trees (which ListPresenter) doesn''t.

Here''s an example:

	ListTreeView example2c	"do it"

See the class comment of ListTreeView for more information.

	-- chris'!
!ListTreePresenter categoriesForClass!MVP-Presenters! !
!ListTreePresenter methodsFor!

beNotSorted
	"remove any sorting of the model. This is almost a null-op since we
	don't acually use a sorted collection"

	self sortBlock: nil.!

beSorted
	"change the receiver's model to be a SortedCollection 
	using a default sort block"

	self beSorted: self defaultSortBlock.!

beSorted: aSortBlock
	"change the order in which the roots of the model are displayed to that implied by aSortBlock,
	and recursively to each list of children under any parent.  Note that this doesn't (unlike the
	superclass implementation) change the class of the model -- in fact it doesn't change the
	model at all..."

	self sortBlock: aSortBlock.
	self view sortBlock: aSortBlock.
!

collapse: anObject
	"ensure that all immediate children of anObject are not displayed in our view"

	self view collapse: anObject.!

expand: anObject
	"ensure that all immediate children of anObject are displayed in our view"

	self view expand: anObject.!

expandAll: anObject
	"ensure that all (recursive) children of anObject are displayed in our view.
	If anObject is nil then it expands all the roots"

	self view expandAll: anObject.!

hasMultipleSelection
	"answer whether our view supports multiple selection"

	"hack"
	^ self view isKindOf: MultipleSelectionListView.!

list
	"answer the contents of the receiver.
	Overriden since our own 'model' is not the one used
	by our view, and the list should be regarded as readonly"

	"we can probably do better than this, but it'll do for now"
	self view model list shallowCopy.
!

list: aSequenceableCollection
	"set the contents of the receiver.
	Overriden since our own 'model' is not the one used
	by our view"

	"we can probably do better than this, but it'll do for now"
	self shouldNotImplement.!

model: anObject
	"set the model of the receiver to be anObject. Overriden to
	pick up the View's sortBlock if any"

	super model: anObject.
	self sortBlock: self view sortBlock.!

treeModel
	"answer the real tree model used by our view"

	^ self view treeModel.!

up
	"ask our view to move the selection up one generation.
	Note that this can have rather odd effects if the view is
	multiselection"

	| sels |

	sels := self view selectionsAsSet.
	sels := sels collect: [:each | self view parentOf: each].
	self view selections: sels asOrderedCollection.! !
!ListTreePresenter categoriesFor: #beNotSorted!accessing!public!sorting! !
!ListTreePresenter categoriesFor: #beSorted!accessing!public!sorting! !
!ListTreePresenter categoriesFor: #beSorted:!accessing!public!sorting! !
!ListTreePresenter categoriesFor: #collapse:!operations!public! !
!ListTreePresenter categoriesFor: #expand:!operations!public! !
!ListTreePresenter categoriesFor: #expandAll:!operations!public! !
!ListTreePresenter categoriesFor: #hasMultipleSelection!public!testing! !
!ListTreePresenter categoriesFor: #list!accessing!public! !
!ListTreePresenter categoriesFor: #list:!accessing!public! !
!ListTreePresenter categoriesFor: #model:!accessing!public! !
!ListTreePresenter categoriesFor: #treeModel!accessing!operations!public! !
!ListTreePresenter categoriesFor: #up!operations!public! !

ListTreePresenter methodProtocol: #treePresenter attributes: #(#readOnly) selectors: #(#collapse: #ensureVisible #expand: #expandAll: #onAboutToDisplayMenu: #onActionPerformed #onCloseRequested #onIdleEntered #onInputMaskRequired: #onKeyPressed: #onKeyReleased: #onKeyTyped: #onKillFocus #onLeftButtonDoubleClicked: #onLeftButtonPressed: #onLeftButtonReleased: #onMouseMoved: #onPositionChanged: #onPositionChanging: #onRightButtonDoubleClicked: #onRightButtonPressed: #onRightButtonReleased: #onSetFocus #onTipTextRequired: #onViewClosed #onViewOpened #parentPresenter #performAction #setFocus #show #topShell #up #view)!

!ListTreePresenter class methodsFor!

defaultModel
	"answer a default model to be assigned to the receiver when it
	is initialized."

	^ TreeModel new.! !
!ListTreePresenter class categoriesFor: #defaultModel!models!public! !

ListTreeView guid: (GUID fromString: '{F2D6215C-2ADC-4032-AACB-8C4DA10950AB}')!
ListTreeView comment: 'Copyright � Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

An implementation of a kind of TreeView that supports multiple columns.  It uses the same technique as (I guess) M$ do for Outlook Express''s message folder.  I.e. it''s really a ListView that indents the primary column''s items according to how deep in the tree they are, and uses the rows ''state'' images to show open/closed indicators.  There is also a version of this class that supports multiple-selection, but is otherwise identical (see MultipleSelectionListTreeView).

There are some examples of use in the class-side category ''examples'', but to save you the effort of going to look for them:
	self example1.	"do it"
	self example2a.	"do it"
	self example2b.	"do it"
	self example2c.	"do it"

Instances can be used with TreePresenters (e.g. TreePresenter.ListTree view), ListPresenters (e.g. ListPresenter.ListTree view), or with the specialised presenter class, TreeViewPresenter, which knows more about ListTreeView than the pre-packaged Presenter classes do.  

The multi-select version, MultipleSelectionListTreeView, can be used with ListPresenters (e.g. ListPresente.Multi-select ListTree view) and with ProperTreeViewPresenter.


General Notes on Usage:
----------------------------------------

Just like any other View really.  Nothing too special.  This package adds pre-packaged ListTreeViews to TreePresenter and TreeView.

As far as possible these act pretty much like TreeViews or ListViews.  However, I''ve added three attributes for controlling sorting.  These are #sortBlock, the <diadicValuable> to use to sort the tree (items are sorted *within* the list of their siblings); and then two attributes that restrict the use of the sort block: #doSortRoots, a boolean that can be used to turn off sorting the roots; and #doSortChildren, a <monadicValuable> that is used to selectively enable sorting of each item''s children.

Instances trigger four additional events off the Presenter, #nodeAdded:, #nodeRemoved:, #nodeExpanded:, and #nodeCollapsed: which can be useful for controling the resources used for backing trees.  (This is something I''ve wanted in the standard TreeView for some time.)


Bugs
--------

I have found that using column images with a non-virtual instance will cause Windows errors.  I can''t find anything that I''m doing wrong to cause this, and none of the workarounds that I''ve tried fix it, so I''ll just have to leave a warning that it doesn''t work.  Sorry!!   (Stop press: a quick look suggests that this may now work in Dolphin 5.0.3, which has had changes to the way non-virtual ListViews work.)

For some reason Window''s always leaves space for the primary row''s icon, even if we''ve told it that we don''t want one.  I suspect that I''m missing something obvious, but I can''t find what I''m doing wrong.  Programming Windows, perhaps...


Differences Between ListTreeView and ListView
-----------------------------------------------------------------------------

The major difference is that the model maintained by the ListTreeView is a TreeModel.  If you clip it to a ListPresenter and give it a ListModel, then the ListTreeView will generate an internal VirtualTreeModel from its three block-valued aspects, #hasChildrenBlock, #getChildrenBlock, and #getParentBlock.  The ListModel used by the presenter is used only to populate the TreeModel''s roots, and is thereafter ignored.  The big effect of this on TreePresenter is that the #list should be treated as readonly, since it is being maintained by the ListTreeView.

You can also supply a TreeModel to a ListPresenter/ListTreeView pair.  This seems to work OK, but of course there may be problems.  The reason you might want to do that is that you want the multi-selection awareness of a ListPresenter, but need to manipulate an externally defined TreeModel.  Example 2b does this.

A minor difference is that sorting is not dispatched via the ListPresenter, since the ListPresenter attempts to manipulate the #list directly.  If a ListTreeView is connected to a TreePresenter or ListPresenter then it does its own sorting without consulting its presenter.  If it is connected to a proper ListTreePresenter (or subclass) then it dispatches via the presenter in the normal way.

The #hasCheckBoxes aspect is not supported, since that (internally to Windows) uses the same ''state'' images as we want to use for our open/closed indicators.


Differences between ListTreeView and TreeView
-----------------------------------------------------------------------------

The biggest deficiency (that I know about so far) is that ListTreeView does not draw tree ''lines''.  Since M$ don''t do this either (in Outlook Express) I take it that this is OK.  If you find it too much of a failing, then don''t use this software... ;-)

The big gains over a TreeView is that a ListTreeView supports additional ListViewColumns and that MultipleSelectionListTreeView supports multiple selection (duh).  As far as I know things like per-column images, owner-draw, etc all work, but I haven''t tried everything.

One or two minor aspects of TreeView, like #isMonoExpandable and #hasHotTracking, are not implemented or are simulated badly.

As with ListPresener, sorting is not dispatched via the TreePresenter but handled internally, and the #hasCheckBoxes aspect is not supported.

Since the tree navigation key bindings are not quite consistant across M$ products, I''ve tried to find and implement a sensible compromise among them.  Please let me know if I''ve missed any.  I''ve also tried to come up with sensible implementations for the case where there are multiple selections.  Sometimes they will seem odd, but I don''t think that''s avoidable.  One touch I like is that expanding any selected item while holding down the <shift> key will automatically select its children too.

Instances can be set not to keep a record of which nodes were expanded when their parents are closed.  This, again, can save resources, but it''s mainly because the default Window''s tree behaviour irritates me.  It makes no difference for small trees, but is a bloody nuisance for large ones.  This is controlled by the #retainExpanded aspect; standard Windows behaviour is the default.

Instances can be set not to collapse the entire subtree below a node in response to #onTreeChanged: notification.  The default is to collapse the sub-tree because that''s what TreeView does.  This is controlled by the #useSmartUpdate aspect.

	-- chris'!
!ListTreeView categoriesForClass!MVP-Views! !
!ListTreeView methodsFor!

addAll: aCollection afterIndex: anIndex
	"private -- add all the objects in aCollection after the given index in the displayed list"

	| index |

	index := anIndex.
	aCollection do: [:each |
		self addItem: each afterIndex: index.
		index := index + 1].!

addExpanded: anObject
	"private -- ensure that anObject is on our list of expanded
	nodes"

	expanded add: anObject.!

addHiddenExpanded: anObject
	"private -- ensure that anObject is on our hidden-expanded list, if we have one"

	self retainExpanded ifTrue: [hiddenExpanded add: anObject].

!

addItem: anObject afterIndex: anIndex
	"private -- add aChildObject to our displayed list after the given index in the displayed list"

	"add the object to the 'real' model"
	self model add: anObject afterIndex: anIndex.

	"NB: this would be unecessary, but the superclass's handling of raw element insertion is too much of
	a minefield for me to mess with it.  See ListView>>addNonVirtualItems:atIndex: for why I'd rather not try
	to override it to ensure that the state is set directly as a side-effect of adding the item"
	self setItem: anObject openState: ((self hasChildren: anObject) ifTrue: [self closedState] ifFalse: [#Leaf]).

	self notifyNodeAdded: anObject.
!

additem: aChildObject toParent: aParentObject
	"private -- add aChildObject to our displayed list under aParentObject, and in the
	correct position w.r.t. any sibling objects.
	It is assumed that the parent either is nil (the child is a root) or that it is visible and
	expanded"

	| siblings |

	siblings := aParentObject isNil
			ifTrue: [self roots]
			ifFalse: [self childrenOf: aParentObject].
	self
		additem: aChildObject
		toParent: aParentObject
		withSiblings: siblings.

!

additem: aChildObject toParent: aParentObject withSiblings: aCollection
	"private -- add aChildObject to our displayed list under aParentObject, and in the
	correct position w.r.t. the collection of sibling objects (which may not yet be displayed
	themselves)"

	| index sibling |

	"try to find the correct position to insert it -- initial assumption is just after the parent (if any)"
	index := self displayIndexOf: aParentObject ifAbsent: [0].

	"or better, just after the previous displayed sibling (if any) and its children (if any)"
	sibling := self previousDisplayedSiblingOf: aChildObject in: aCollection.
	sibling isNil ifFalse:
		[index := (self displayIndexOf: sibling) + (self countDisplayedChildrenOf: sibling)].

	"add the object to the 'real' model"
	self addItem: aChildObject afterIndex: index.!

allDisplayedItems
	"answer an OrderedCollection of all the items that are currently displayed.
	(i.e. they are either a root or their parent is expanded).
	They are listed in the order that they are currently displayed"

	^ OrderedCollection withAll: self model.!

allExpandedItems
	"answer an OrderedCollection of all the items that are currently expanded"

	^ OrderedCollection withAll: expanded.!

allHiddenExpandedItems
	"answer an OrderedCollection of all the items that are currently hidden-expanded; that
	is the ones that we will automatically expand as soon as they are visible, because they
	were expanded when their parent was closed"

	| all |

	all := OrderedCollection new.
	self retainExpanded ifTrue: [all addAll: hiddenExpanded].

	^ all.!

applyImageLists
	"private -- set the receiver's image lists from the image managers.
	Overridden to do the state Image list too"

	| images |

	super applyImageLists.

	"note that we don't use the state image list instvar; there's no need since the
	image list is always the same systemwide, and this avoids having to mess with
	STBing images"
	images := self hasButtons
			ifTrue: [self class stateImageList]
			ifFalse: [nil].
	self stateImageList: images.!

basicCollapse: anObject
	"private -- collapse the displayed hierarchy below anObject, and refresh its display.
	NB: assumes none of the children of anObject are selected.
	NB: assumes anObject is expanded"

	"collapse the node and collapse and remove any children.
	Someday I may activate the redraw-suppression code configurably"
		"self disableRedraw. [ Cursor wait showWhile: ["
	self veryBasicCollapse: anObject.
		"] ] ensure: [self enableRedraw]."

	"as with #expand: we allow the tree model to say that an object has children even if
	the actual set thereof is zero-sized, hence we display a #Closed image even if there
	were no actual children to remove"
	self setItem: anObject openState: (self closedState).
	self invalidateItem: anObject.!

basicOnItem: aChildObject movedToParent: aParentObject
	"private -- implementation of #onItem:movedToParent:  This is factored
	out so that the code can be executed without triggering the normal
	events or worrying about handling selection change events.  Note
	that we rely on the caller reseting all the status icons, since otherwise
	there's no way to ensure that the *old* parent's icon will be correct"

	| itemDisplayed itemExpanded parentDisplayed parentExpanded |

	"Warning: sin is prettier than this horribly fragile logic"

	"work out the status quo for the child object"
	(self isItemDisplayed: aChildObject)
		ifTrue: [itemDisplayed := true. itemExpanded := self isItemExpanded: aChildObject]
		ifFalse: [itemDisplayed := itemExpanded := false].

	"ditto for the new parent.  We treat a nil parent (child is a root) as 'expanded' to simplify (ha!!) the logic"
	aParentObject notNil
		ifTrue: [(self isItemDisplayed: aParentObject)
			ifTrue: [parentDisplayed := true. parentExpanded := self isItemExpanded: aParentObject]
			ifFalse: [parentDisplayed := parentExpanded := false]]
		ifFalse: [parentDisplayed := parentExpanded := true].

	"if the item wasn't visible before, then we can early-out, just ensuring that it's added to the new
	parent if necessary.  Remember that the caller will update the status icons for us"
	itemDisplayed ifFalse:
		[parentExpanded ifTrue: [self additem: aChildObject toParent: aParentObject].
		^ self].

	"otherwise the item must be visible, so must ensure that the new parent is expanded, but if the
	parent is not already expanded, then expanding it will create a duplicate of the existing row, so we
	have to remove the old row first in that case.  (BTW, we don't need to worry about the hidden-expanded
	stuff causing aChildObject to be expanded when the new parent is, since aChildObject is displayed
	currently and therefore not on the hidden expanded list)"
	parentExpanded ifFalse:
		[self removeFromDisplay: aChildObject].
	aParentObject isNil ifFalse:
		[self
			ensureItemDisplayed: aParentObject;
			expand: aParentObject].

	"now we know that the item is visible; if it had been visible before, and the new parent was already
	expanded then we'll need to update the display order. The same applies if the item itself was already
	expanded since any children will still be in their old positions"
	(parentExpanded or: [itemExpanded]) ifTrue: [self updateDisplayOrder].

	"and lastly ensure that the indentations are all OK.  This step is unecessary if we're virtual,
	and in fact causes a common control error, so we only do it if we're not"
	self isVirtual ifFalse: [self unorderedTreeOf: aChildObject do: [:each | self setItemIndentation: each]].
!

beNoIcons
	"supplied for consistancy with TreeView"

	self viewMode: #noIcons.!

beSorted: aSortBlock
	"implemented so that we can implement the protocol of our Presenters"

	self sortBlock: aSortBlock.
!

checkChangeSelectionFrom: anOldCollection to: aNewCollection because: aSymbol
	"private -- trigger a #selectionChanging: notification and answer whether the
	Observers thereof were happy for the selection change to go ahead"

	| event |

	event := (SelectionChangingEvent forSource: self)
			oldSelections: anOldCollection;
			newSelections: aNewCollection;
			cause: aSymbol;
			yourself.

	self presenter onSelectionChanging: event.

	^ event value.
!

checkSelectAll: aCollection because: aSymbol
	"private -- trigger a #selectionChanging: notification and answer whether the
	Observers thereof were happy for the selection change to go ahead"

	^ self
		checkChangeSelectionFrom: self selections
		to: aCollection
		because: aSymbol.!

childrenOf: anObject
	"answer our TreeModel's idea of anObject's immediate children in the order
	specified by our sortblock, etc"

	^ self
		inOrder: (self unorderedChildrenOf: anObject)
		parent: anObject.!

closedState
	"private -- answer the Symbol name of the 'closed' state"

	^ self hasIcons ifTrue: [#Closed] ifFalse: [#SmallClosed].
!

collapse
	"collapse the tree view to ensure that the immediate children of the current selection,
	if any, are not displayed.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events and will add anObject to the
	'hidden-expanded' list"

	self selections do: [:each | self collapse: each].!

collapse: anObject
	"collapse the displayed hierarchy below anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events but does add anObject to the
	'hidden-expanded' list"

	| newSelection |

	(self isItemExpanded: anObject) ifFalse: [^ self].

	"we have to change the selection first, or else it'll change implicitly as selected
	children are closed.  That causes unwanted selectionChanged events to be
	generated.
	Note that we *don't* attempt to send a #selectionChanging: notification -- that is
	only sent for user-initiated actions (e.g. see #collapse:because:)"
	newSelection := self selectionAfterCollapseList: (Array with: anObject).
	newSelection isNil ifFalse: [self selections: newSelection].

	"now that we've done that, on to part two!!"
	self basicCollapse: anObject.
!

collapse: anObject because: aSymbol
	"private -- collapse the given Object's node.
	This is (part of) the implementation of double-clicking on the item and of left clicking the open/close box"

	self collapseList: (Array with: anObject) because: aSymbol.!

collapseAll
	"collapse the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events add will add all currently expanded items
	to the hidden-expanded list"

	self selectionRootsOrRoots do: [:each | self collapse: each].
!

collapseList: aCollection because: aSymbol
	"private -- collapse the tree view to ensure that the immediate children the items in aCollection
	are not displayed.
	Since this is user-initiated we need send the vetoable #onSelectionChanging:
	event before making any changes.
	For the same reason, the directly affected nodes are not made hidden-expanded
	since the user has set their preferred status"

	| oldSelections collapse newSelections |

	collapse := aCollection select: [:each | self isItemExpanded: each].
	collapse isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterCollapseList: collapse.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"we have to change the selection first, or else it'll change implicitly as selected
	children are closed.  That causes unwanted selectionChanged events to be
	generated"
	newSelections notNil ifTrue: [self selections: newSelections].

	"this little bit of code is what all the fuss was about..."
	collapse do: [:each | self basicCollapse: each].

	"collapsing the nodes will have made them 'hidden-expanded' so we undo that here"
	collapse do: [:each | self removeHiddenExpanded: each]
!

collapseOrSelectParentBecause: aSymbol
	"private -- if the current selection (if any) is expanded then collapse it, otherwise
	select its parent.
	This is the implementation of the left-arrow-key shortcut"

	| sel |

	sel := self selections at: 1 ifAbsent: [^ self].

	(self isItemExpanded: sel)
		ifTrue: [self collapse: sel because: aSymbol]
		ifFalse: [self selectParentOf: sel because: aSymbol].!

collapseSelectedBecause: aSymbol
	"private -- collapse the tree view to ensure that the immediate children of the current selection,
	if any, are not displayed.
	This is the implementation of the numeric-minus keyboard shortcut"

	self collapseList: self selectionRoots because: aSymbol.!

connectModel
	"connect the receiver to its model, wiring events, etc. Overridden so as also to
	connect to our internal tree model"

	super connectModel.

	self connectTreeModel.
!

connectTreeModel
	"private -- set up the connections to our internal tree model"

	treeModel
		when: #treeChanged: send: #onTreeChanged: to: self;
		when: #item:addedInParent: send: #onItem:addedInParent: to: self;
		when: #item:removedFromParent: send: #onItem:removedFromParent: to: self;
		when: #item:movedToParent: send: #onItem:movedToParent: to: self;
		when: #itemUpdated: send: #onItemUpdated: to: self.

!

countDisplayedChildrenOf: anObject
	"private -- answer how many displayed chilren (recursively) anObject has"

	| answer |

	answer := 0.
	self displayedTreeBelow: anObject do: [:each | answer := answer + 1].

	^ answer.!

defaultIndentSeparation
	"private -- answer the default pixel identation of child items"

	^ 0.!

depthOf: anObject
	"answer our TreeModel's idea of how many generations anObject is descended
	from a root"

	| depth it |

	"why recurse when we can iterate, eh?"
	depth := 0.
	it := anObject.
	[(it := self parentOf: it) notNil] whileTrue: [depth := depth + 1].

	^ depth.!

disableExpandAll
	"answer whether the abiliity to expand the whole tree with one keypress is enabled"

	^ options allMask: DisableExpandAllMask.!

disableExpandAll: aBool
	"set whether the abiliity to expand the whole tree with one keypress is enabled"

	options := options mask: DisableExpandAllMask set: aBool.
!

discardAllExpanded
	"private -- discard any record of which items are expanded or hidden-expanded"

	self
		discardExpanded;
		discardHiddenExpanded.
!

discardExpanded
	"private -- discard any record of which items are expanded"

	expanded := self makeSet.!

discardHiddenExpanded
	"discard any record we may be keeping of which items were
	expanded when they were last displayed.  This only discards
	the current record, it does not stop us from retaining such records
	in the future"

	self retainExpanded ifTrue: [hiddenExpanded := self makeSet].!

disconnectFromModel
	"remove all event registrations lodged with the current model. Overridden so as also to
	disconnect from our tree model"

	super disconnectFromModel.

	treeModel notNil ifTrue: [treeModel removeEventsTriggeredFor: self].
!

displayedContents
	"supplied for consistancy with TreeView"

	^ self allDisplayedItems.!

displayedOrHiddenTreeBelow: aParent do: a1Block
	"private -- traverse the displayed or hidden-expanded part of our tree in pre-order starting at, but
	not including, aParent, evaluating a1Block for each discovered node"

	(self unorderedChildrenOf: aParent) do:
		[:each |
		a1Block value: each.
		((self isItemExpanded: each) or: [self isItemHiddenExpanded: each]) ifTrue:
			[self displayedOrHiddenTreeBelow: each do: a1Block]].!

displayedTreeBelow: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at, but not including, aParent, evaluating
	a1Block for each discovered node"

	(self isItemExpanded: aParent) ifTrue:
		[(self childrenOf: aParent) do: [:each | self displayedTreeOf: each do: a1Block]].!

displayedTreeDo: a1Block
	"traverse the displayed part of our tree in pre-order evaluating
	a1Block for each discovered node"

	self roots do: [:each | self displayedTreeOf: each do: a1Block].!

displayedTreeOf: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at aParent, evaluating
	a1Block for each discovered node"

	a1Block value: aParent.
	self displayedTreeBelow: aParent do: a1Block.!

displayIndexOf: anObject
	"private -- answer the index of anObject in our list display"

	^ self model indexOf: anObject.!

displayIndexOf: anObject ifAbsent: a0Block
	"private -- answer the index of anObject in our list display, or, if it is
	not displayed, the result of evaluating a0Block"

	^ self model indexOf: anObject ifAbsent: a0Block.!

doSortChildrenBlock
	"answer the <monadicValuable> that, if it is not nil, will be evaluted with a parent
	node as its parameter in order to decide whether to use the #sortBlock when sorting
	the children.  This is primarily intended to allow for the case where a tree has 'synthetic'
	nodes that shouldn't be sorted since their layout was set by the UI designer.
	See also #doSortRoots"

	^ doSortChildrenBlock.
!

doSortChildrenBlock: a1Block
	"set the <monadicValuable> that, if it is not nil, will be evaluted with a parent
	node as its parameter in order to decide whether to use the #sortBlock when sorting
	the children.
	See also #doSortRoots:"

	doSortChildrenBlock = a1Block
		ifTrue: [doSortChildrenBlock := a1Block]
		ifFalse: [doSortChildrenBlock := a1Block. self updateDisplayOrder].


!

doSortRoots
	"answer whether our #sortBlock, if any, will be used to sort the roots of the tree.
	See also #doSortChildrenBlock"

	^ options allMask: DoSortRootsMask.!

doSortRoots: aBool
	"set whether our #sortBlock, if any, will be used to sort the roots of the tree.
	See also #doSortChildrenBlock:"

	aBool = self doSortRoots ifTrue: [^ self].

	options := options mask: DoSortRootsMask set: aBool.
	self updateDisplayOrder
!

ensureItemDisplayed: anObject
	"ensure that all parents of anObject are expanded.  Has no effect for root objects,
	or objects that are not children (recursively) of a root.
	Answers whether the object is actually displayed"

	^ (self isItemDisplayed: anObject)
		ifTrue: [true]
		ifFalse: [(self parentOf: anObject)
			ifNil: [false]
			ifNotNil: [:parent | (self ensureItemDisplayed: parent) and: [self expand: parent. true]]].!

expand
	"expand the tree view to ensure that the immediate children of the current
	selection, if any, are displayed.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self selections do: [:each | self expand: each].
!

expand: anObject
	"expand the tree view to ensure that the immediate children of anObject are displayed.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	| children |

	(self isItemExpandable: anObject) ifFalse: [^ self].

	"ensure it is actually displayed, but remove any hidden-expanded record first, or we'll
	end up in an infinite recursion (eek!!)"
	self
		removeHiddenExpanded: anObject;
		ensureItemDisplayed: anObject.

	"note that this may cause an object with no children to show an open flag; that's by design
	so that TreeModels that distinguish between having a zero-sized set of children and not having
	children will display properly"
	self setItem: anObject openState: (self openState).
	self invalidateItem: anObject.

	"add any children"
	children := self childrenOf: anObject.
	self addAll: children afterIndex: (self displayIndexOf: anObject).
	self addExpanded: anObject.

	"apply our record of which children were expanded when this node was last closed"
	hiddenExpanded notNil ifTrue:
		[children do: [:each | (hiddenExpanded includes: each) ifTrue: [self expand: each]]].

	self notifyNodeExpanded: anObject.
!

expand: anObject because: aSymbol
	"private -- expand/collapse the given Object's node.
	This (part of) is the implementation of double-clicking on the item and of left clicking the
	open/close box"

	self expandList: (Array with: anObject) because: aSymbol.
!

expandAll
	"expand the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self selectionOrRoots do: [:each | self expandAll: each].!

expandAll: anObject
	"expand the tree view to ensure that all the (recursive) children of anObject are displayed.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self expand: anObject.
	(self unorderedChildrenOf: anObject) do: [:each | self expandAll: each].
!

expandAllList: aCollection because: aSymbol
	"private -- expand the tree view to ensure that all (recursive) children of the items
	in aCollection are displayed.
	Since this is user-initiated we must send the vetoable #onSelectionChanging:
	event before making any changes"

	| expand newSelections |

	expand := aCollection select: [:each | self isItemExpandable: each].
	expand isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterExpandAllList: expand.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"all OK, do it"
	"Cursor wait showWhile: ["
	expand do: [:each | self expandAll: each].
	"]."

	"ensure that the selection has changed in the way we wanted it to"
	newSelections notNil ifTrue: [self selections: newSelections].
!

expandAllOrCollapseSelectedBecause: aSymbol
	"private -- expand/collapse the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is the implementation of the number-pad multiply keyboard shortcut"

	(self selectionOrRoots anySatisfy: [:each | self isItemExpanded: each])
		ifTrue: [self collapseList: self selectionRootsOrRoots because: aSymbol]
		ifFalse: [self expandAllList: self selectionOrRoots because: aSymbol].!

expandList: aCollection because: aSymbol
	"private -- expand the tree view to ensure that the immediate children of the items
	in aCollection are displayed.
	Since this is user-initiated we must send the vetoable #onSelectionChanging:
	event before making any changes"

	| expand newSelections |

	expand := aCollection select: [:each | self isItemExpandable: each].
	expand isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterExpandList: expand.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"all OK, do it"
	expand do: [:each | self expand: each].

	"ensure that the selection has changed in the way we wanted it to"
	newSelections notNil ifTrue: [self selections: newSelections].!

expandOrCollapse: anObject because: aSymbol
	"private -- expand/collapse the given Object's node.
	This is the implementation of double-clicking on the item and of left clicking
	the open/close box"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject because: aSymbol]
		ifFalse: [self expand: anObject because: aSymbol].
!

expandOrSelectFirstChildBecause: aSymbol
	"private -- if the current selection (if any) is expanded then select its first child, otherwise
	expand it.
	This is the implementation of the right-arrow-key shortcut"

	| sel |

	sel := self selections at: 1 ifAbsent: [^ self].

	(self isItemExpanded: sel)
		ifTrue: [self selectFirstChildOf: sel because: aSymbol]
		ifFalse: [self expand: sel because: aSymbol].!

expandSelectedBecause: aSymbol
	"private -- expand the tree view to ensure that the immediate children of the current
	selection, if any, are displayed.
	This is the implementation of the numeric-plus keyboard shortcut"

	self expandList: self selections because: aSymbol.!

getChildrenBlock
	"answer the <monadicValuable> that we will use to find the children of
	an Object if we have not been configured with an explicit TreeModel"

	^ getChildrenBlock.
!

getChildrenBlock: a1Block
	"set the <monadicValuable> that we will use to find the children of
	an Object if we have not been configured with an explicit TreeModel"

	getChildrenBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel getChildrenBlock: a1Block].!

getParentBlock
	"answer the <monadicValuable> that we will use to determine the parent of
	an Object if we have not been configured with an explicit TreeModel"

	^ getParentBlock.!

getParentBlock: a1Block
	"set the <monadicValuable> that we will use to determine the parent of
	an Object if we have not been configured with an explicit TreeModel"

	getParentBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel getParentBlock: a1Block].
!

hasButtons
	"answer whether the receiver has expand/contract buttons"

	^ options allMask: HasButtonsMask.!

hasButtons: aBool
	"set whether the receiver has expand/contract buttons"

	options := options mask: HasButtonsMask set: aBool.
	self applyImageLists.
!

hasCheckBoxes
	"answer whether the receiver has check-boxes.
	Overridden because the check box stuff uses the same 'state' images as we need
	for our open/closed indicators.  Always answers false"

	^ false.!

hasCheckBoxes: aBool
	"set whether the receiver has check-boxes.
	Overridden because the check box stuff uses the same 'state' images as we need
	for our open/closed indicators.  Always ignored (for consistancy with TreeView)"!

hasChildren: anObject
	"answer our TreeModel's idea of whether anObject has children"

	^ treeModel hasChildren: anObject.!

hasChildrenBlock
	"answer the <monadicValuable> that we will use to determine whether
	an Object has children if we have not been configured with an explicit
	TreeModel"

	^ hasChildrenBlock.!

hasChildrenBlock: a1Block
	"set the <monadicValuable> that we will use to determine whether
	an Object has children if we have not been configured with an explicit
	TreeModel"

	hasChildrenBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel hasChildrenBlock: a1Block].!

hasHotTracking
	"answer whether the receiver has the hot-tracking style"

	"this seems to be the nearest equivalent"
	^ self listViewStyleAllMask: LVS_EX_TWOCLICKACTIVATE.!

hasHotTracking: aBool
	"set whether the receiver has the hot-tracking style"

	"MSDN seems to imply that a TreeView's hot tracking is the same thing as a list view's
	track select.  But they ain't the same beast at all.
	This seems to be the nearest equivalent"
	self listViewStyleMask: LVS_EX_TWOCLICKACTIVATE set: aBool.!

hasIcons
	"supplied for consistancy with TreeView"

	^ self viewMode ~~ #list and: [self getImageBlock notNil or: [self hasColumnImages]].!

hasLines
	"answer whether the receiver has lines connecting its nodes.
	Unfortunately we do not support lines, at least not yet"

	^ false.!

hasLines: aBool
	"set whether the receiver has lines connecting its nodes.
	Unfortunately we do not support lines, so this is ignored
	but retained for compatability with TreeView"
!

hasLinesAtRoot
	"answer whether the receiver has lines and buttons for its root entries.
	Actually this only affects the buttons.  Note also that it will have no
	effect if we do not have buttons at all"

	^ options allMask: HasLinesAtRootMask.!

hasLinesAtRoot: aBoolean
	"set whether the receiver has lines and buttons for its root entries.
	Actually this only affects the buttons.  Note also that it will have no
	effect if we do not have buttons at all"

	options := options mask: HasLinesAtRootMask set: aBoolean.

	#CUtodo.  "should this be #updateAll instead ?"
	self refreshContents.!

indentationFor: anObject
	"private -- answer the number of image widths to indent the row by.
	We use this to hack an approximation to a tree"

	| depth |

	depth := self depthOf: anObject.

	"try to adjust to take #indentSeparation into account"
	depth := depth * (1 + (indentSeparation / self class stateImageWidth) rounded).

	"if we have buttons then Windows'll add indent for the (possibly blank) state icon too,
	so if we don't want #linesAtRoot (really #buttonsAtRoot) then reduce the depth by one
	to hide any buttons in the first column"
	(self hasButtons and: [self hasLinesAtRoot not]) ifTrue: [depth := depth - 1].

	^ depth.!

indentFromRow: anObject
	"private -- answer the number of image widths to indent the row by.
	We use this to hack an approximation to a tree"

	^ self indentationFor: anObject.!

indentSeparation
	"answer the pixel identation of child items from their parents excluding
	the width of the current set of icons being shown"

	^ indentSeparation.!

indentSeparation: indentation
	"set the pixel identation of child items from their parents excluding
	the width of the current set of icons being shown.
	Note that the actual indentation used will be rounded to the nearest multiple
	of the size of the open/close image.  Also the indentation is not applied in
	quite the same way as a real TreeView does it"

	indentSeparation := indentation.
	self invalidate.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.	"will set our model, etc, as side-effect"

	indentSeparation := self defaultIndentSeparation.
	self viewMode == #report ifFalse: [self viewMode: #report].
	self retainExpanded: true.
	options := self class defaultOptions.
!

inOrder: aCollection parent: anObject
	"private -- answer aCollection of children of anObject as ordered by our sort settings"

	"the test against true is in case the doSortChildrenBlock answers nil or self"
	^ (sortBlock isNil or: [doSortChildrenBlock notNil and: [(doSortChildrenBlock value: anObject) ~= true]])
		ifTrue: [aCollection]
		ifFalse: [(aCollection asSortedCollection: sortBlock) asOrderedCollection].!

inRootOrder: aCollection
	"private -- answer aCollection ordered by our sort settings for root objects"

	^ (sortBlock isNil or: [self doSortRoots not])
		ifTrue: [aCollection]
		ifFalse: [(aCollection asSortedCollection: sortBlock) asOrderedCollection].!

invalidateItem: anObject
	"private -- invalidate the rectangle of the item for anObject"

	| index |

	index := self displayIndexOf: anObject ifAbsent: [^ self].
	self invalidateItemIndex: index.!

invalidateItemIndex: anInteger
	"private -- envalidate the item with the 1-based integer index"

	self invalidateRect: (self itemRect: anInteger textOnly: false).
!

isItemDisplayed: anObject
	"answer whether anObject is currently displayed (i.e. either it is a root or
	its parent has been expanded to show its children -- there is no implication
	that it is actually *visible*, at least without scrolling)"

	^ self model includes: anObject.!

isItemExpandable: anObject
	"answer whether it is possble to expand anObject -- i.e. that it has children,
	but hasn't been expanded yet"

	^ (self hasChildren: anObject) and: [(self isItemExpanded: anObject) not].!

isItemExpanded: anObject
	"answer whether anObject is currently expanded (this doens't imply that
	it has any children, just that the #expand: operation has been performed
	on it)"

	^ expanded includes: anObject.!

isItemHiddenExpanded: anObject
	"answer whether anObject is hidden-expanded; that is, that its parent isn't
	displayed, but if/when it is, anObject will be expanded automatically"

	^ self retainExpanded and: [hiddenExpanded includes: anObject].!

isLargeIcons
	"supplied for conistancy with TreeView.  I'm not entirely sure what the implementation
	should be here, since we'd look Very Odd Indeed in any mode than #report.  However..."

	^ self viewMode == #largeIcons.!

isMonoExpandable
	"answer whether the receiver has the single-expand style.
	Supplied for consistancy with TreeView; always answers false"

	^ false.!

isMonoExpandable: aBool
	"set whether the receiver has the single-expand style.
	Supplied for consistancy with TreeView; currently ignored"!

isSelected: anObject
	"supplied for consistancy with TreeView; answer whether anObject is currently
	selected"

	| comparator |

	comparator := self searchPolicy.
	self selections do: [:each | (comparator compare: each with: anObject) ifTrue: [^ true]].
	^ false.
!

isSmallIcons
	"supplied for conistancy with TreeView.  I'm not entirely sure what the implementation
	should be here, since we'd look Very Odd Indeed in any mode than #report, but that's
	probably the bset we can do"

	^ self viewMode == #report.
!

isVirtual: aBoolean
	"overridden to request the state update requests"

	| mask |

	mask := self lvmGetCallbackMask.
	mask := mask mask: LVIS_STATEIMAGEMASK set: aBoolean.
	self lvmSetCallbackMask: mask.

	^ super isVirtual: aBoolean.!

listModel: aListModel
	"private -- set the ListModel for us to use.  This is invoked indirectly from #model: if the supplied
	model is a ListModel.  We generate a VirtualTreeModel and initialise its pluggable blocks
	from our *Block aspects to use as our TreeModel, and create an internal ListModel to use
	as our #model (the contents of which will be changed as the display changes).
	Actually we use the trivial subclass, VirtualTreeModelWithSearchPolicy, since that allows us to
	preserver the search policy of the list model too.
	Note that once we have initialised ourself from the supplied list model, we take no further interest
	in it"

	| roots newTreeModel newListModel |

	roots := OrderedCollection withAll: aListModel.
	newTreeModel := self makeInternalTreeModel: roots searchPolicy: aListModel searchPolicy.
	newListModel := self makeInternalListModel: roots searchPolicy: aListModel searchPolicy.

	hasExplicitTreeModel := false.
	self treeModel: newTreeModel listModel: newListModel.!

makeInternalListModel: aCollection searchPolicy: aSearchPolicy
	"private -- answer a new ListModel configured with the given initial contents and search policy.
	This will be used  to hold the actual displayed items, and will be passed to 'super' as its model"

	^ self class internalListModelClass
				on: (OrderedCollection withAll: aCollection)
				searchPolicy: aSearchPolicy.
!

makeInternalTreeModel: aCollection searchPolicy: aSearchPolicy
	"private -- answer a new TreeModel configured the given roots, aCollection,
	search policy, and our various blocks"

	| newTreeModel |

	newTreeModel := self class internalTreeModelClass
					withRoots: aCollection
					searchPolicy: aSearchPolicy.

	getParentBlock notNil ifTrue: [newTreeModel getParentBlock: getParentBlock].
	getChildrenBlock notNil ifTrue: [newTreeModel getChildrenBlock: getChildrenBlock].
	hasChildrenBlock notNil ifTrue: [newTreeModel hasChildrenBlock: hasChildrenBlock].

	^ newTreeModel.!

makeSet
	"private -- answer a new Set using our current search policy"

	^ self class setClass searchPolicy: self searchPolicy.
!

makeSetFrom: aCollection
	"private -- answer a new Set which uses our current search policy and contains the
	elements of aCollection"

	^ (self makeSet)
		addAll: aCollection;
		yourself.!

minimumIndent
	"just for consistancy with TreeView"

	^ 0.!

notifyNodeAdded: anObject
	"private -- notify 'our' Observers that a node corresponding to anObject
	has been added.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeAdded: with: anObject.
!

notifyNodeCollapsed: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been collapsed.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeCollapsed: with: anObject.
!

notifyNodeExpanded: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been expanded.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeExpanded: with: anObject.
!

notifyNodeRemoved: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been removed.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeRemoved: with: anObject.
!

onDestroyed
	"called when the View is destroyed (whatever that actually means).  Overridden
	to unlink ourselves from our real tree model's change notifications too"

	self isStateRestoring ifFalse:		"only when we are not in #recreate"
		[treeModel notNil ifTrue: [treeModel removeEventsTriggeredFor: self].
		self discardAllExpanded].

	^ super onDestroyed.!

onDisplayDetailsRequired: lvitem
	"private -- Windows wants the display details for the item identified by lvitem.
	Overriden to handle LVIF_STATE too"

	| object mask |

	object := self objectFromHandle: lvitem handle ifAbsent: [^ nil].
	mask := lvitem mask.

	(self systemDrawsStateImages
		and: [lvitem iSubItem isZero
			and: [lvitem mask allMask: LVIF_STATE]])
		ifTrue: [lvitem stateImageIndex: (self stateIndexFromRow: object)].

	^ super onDisplayDetailsRequired: lvitem.!

onFullyCreated
	"our window has been created. Ensure that it reflects the state we think
	it should have"

	super onFullyCreated.

	self isVirtual ifTrue: [self lvmSetCallbackMask: LVIS_STATEIMAGEMASK].!

onItem: aChildObject addedInParent: aParentObject
	"notification received when aChildObject has been added to the list of
	children of aParentObject.  We assume that aChildObject is not already
	present in the tree as a child of some other object"

	"if the child is currently displayed then ignore it"
	(self isItemDisplayed: aChildObject) ifTrue: [^ self].

	"if the parent, if any, is not currently expanded then ignore it except for (possibly) updating
	the state image, otherwise insert the new child amongst its brothers and sisters"
	(aParentObject notNil and: [(self isItemExpanded: aParentObject) not])
		ifTrue: [self setItemState: aParentObject]
		ifFalse: [self additem: aChildObject toParent: aParentObject].
!

onItem: aChildObject movedToParent: aParentObject
	"notification received when aChildObject has been moved to the list of 	children of aParentObject.
	NB: unlike the TreeView we preserve 'expandedness' of the moved child, rather than just preserving
	the selection and closing all other subtrees"

	| selections |

	"this will prevent selection change events reaching the Presenter."
#CUtodo.  "unfortunately it also prevents #nodeExpanded: and nodeAdded: notifications too -- fix this!!"
	selections := self selections.
	self presenter noEventsDo:
		[self
			basicOnItem: aChildObject movedToParent: aParentObject;
			selections: selections ifAbsent: [];
			updateExpanded;
			updateStates;	
			invalidate].
!

onItem: aChildObject removedFromParent: aParentObject
	"notification received when aChildObject has been removed from the list of
	children of aParentObject."

	(self isItemDisplayed: aChildObject) ifTrue: [self removeZombies].
	aParentObject isNil ifFalse: [self setItemState: aParentObject].
!

onItemUpdated: anObject
	"notification received when one of our elements has been changed"

	self onItemUpdatedAtIndex: (self displayIndexOf: anObject ifAbsent: [^ self]).
	self setItemState: anObject.
!

onKeyPressed: aKeyEvent
	"handler for key press events.
	We want to emulate:
		- the numeric pad's multiply key fully expands/collapses the current selection or the whole tree
		- left-arrow expands children or, if already expanded, selects first
		- right-arrow collapses children, or if not expanded, selects parent
		- numeric+ expands selection
		- numeric- collapses selection"

	| code |

	"God's Teeth, but this is gross!!"

	"early-out with default ListView handling if we're not interested"
	code := aKeyEvent code.
	(code == VK_RIGHT
		or: [code == VK_LEFT
			or: [(code == VK_MULTIPLY and: [self disableExpandAll not])
				or: [code == VK_ADD
					or: [code == VK_SUBTRACT]]]])
		ifFalse: [^ super onKeyPressed: aKeyEvent].

	"I *hate* this selection-changing-cause stuff!!"
	(code == VK_RIGHT) ifTrue: [self expandOrSelectFirstChildBecause: #keyboard].
	(code == VK_LEFT) ifTrue: [self collapseOrSelectParentBecause: #keyboard].
	(code == VK_MULTIPLY and: [self disableExpandAll not]) ifTrue: [self expandAllOrCollapseSelectedBecause: #keyboard].
	(code == VK_ADD) ifTrue: [self expandSelectedBecause: #keyboard].
	(code == VK_SUBTRACT) ifTrue: [self collapseSelectedBecause: #keyboard].

	"remember to do this since we are supressing the superclass implementation"
	self presenter trigger: #keyPressed: with: aKeyEvent.

	"answer 0 to suppress Windows' default handling"
	^ 0.!

onLeftButtonDoubleClicked: aMouseEvent
	"handle a mouse left-button event.  We need to implement opening
	items by double-clicking"

	| hit target |

	#CUtodo.	"what happens if mouse buttons are reversed ?"

	"if it's not over an item then we can just leave it to the superclass"
	hit := self basicItemFromPoint: aMouseEvent position.
	((target := self model at: (hit iItem + 1) ifAbsent: [nil]) notNil)
		ifFalse: [^ super onLeftButtonDoubleClicked: aMouseEvent].

	"toggle the indicated item's expanded state. This does not directly change the
	selection but it may do so implicitly if the effect is to close an ancestor of
	the current selection.  Hence we need to bugger about with the selection cause
	stuff"
	self expandOrCollapse: target because: #mouse.

	"in this case we want to pass the event to the default handling so that any selection
	changes and the default 'action' processing will also be applied"
	^ super onLeftButtonDoubleClicked: aMouseEvent.!

onLeftButtonPressed: aMouseEvent
	"handle a mouse left-button event.  We need to implement opening
	items by clicking their status box"

	| hit target |

	#CUtodo.	"what happens if mouse buttons are reversed ?"

	"see if it's a click on a state icon; if not then let the default list view processing
	take over"
	hit := self basicItemFromPoint: aMouseEvent position.
	(hit isOnItemStateIcon and: [(target := self model at: (hit iItem + 1) ifAbsent: [nil]) notNil])
		ifFalse: [^ super onLeftButtonPressed: aMouseEvent].

	"toggle the indicated item's expanded state. This does not directly change the
	selection, unless shift is down, but it may do so implicitly if the effect is to close an
	ancestor of the current selection.  Hence we need to bugger about with the
	selection cause stuff"
	self expandOrCollapse: target because: #mouse.

	"remember to do this since we are avoiding the superclass implementation"
	self presenter trigger: #leftButtonPressed: with: aMouseEvent.

	"answer 0 to suppress Window's default handling"
	^ 0.!

onTreeChanged: anObject
	"notification received when the tree at and below anObject has been changed.  If
	anObject is nil then it amounts to a notification that *something* (unknown, or
	perhaps the set of roots) has changed.
	Note this is similar in meaning to #refreshFromModelBelow: -- the difference is that
	this (unless #useSmartRefresh is on, in which case it just forwards to
	#refreshFromModelBelow:) collapses the tree below anObject (like the standard
	Dolphin TreeView).  NB: because we need to #removeZombies, this collapsing
	implementation is not significantly cheaper than #refreshFromModelBelow: since
	it still requires a scan of the whole displayed part of the tree"

	self useSmartRefresh ifTrue: [^ self refreshFromModelBelow: anObject].

	"if any part of the tree has changed, then there may be nodes (now removed) that are
	still on our hidden-expanded list; there is no way that we can check for that case
	so just we discard the whole hidden-expanded list"
	self discardHiddenExpanded.

	"easy implementation if the whole tree is potentially invalid"
	anObject isNil ifTrue: [^ self resetTreeToRoots].

	"if the object is not currently displayed, then we've done everything 	we have to"
	(self isItemDisplayed: anObject) ifFalse: [^ self].

	"we collapse the tree at the indicated point"
	self collapse: anObject.

	"but then we *still* have to rescan the displayed tree since anObject may
	have had children which have just vanished (without anyone bothering to
	tell us about them specifically)"
	self removeZombies.

	"finally..."
	self onItemUpdated: anObject.!

openState
	"private -- answer the Symbol name of the 'open' state"

	^ self hasIcons ifTrue: [#Open] ifFalse: [#SmallOpen].
!

parentOf: anObject
	"answer our TreeModel's idea of anObject's parent"

	^ treeModel parentOf: anObject.!

parentsOf: anObject includesAny: aSet
	"private -- answer whether any of the (recursive) parents of anObject are in
	aSet"

	| parent |

	parent := self parentOf: anObject.
	parent isNil ifTrue: [^ false].
	(aSet includes: parent) ifTrue: [^ true].
	^ self parentsOf: parent includesAny: aSet.!

previousDisplayedSiblingOf: anObject in: aCollection
	"private -- given a collection of siblings of anObject (not all of which are necessarily
	displayed yet), answer the last sibling in the collection that is displayed, and which comes
	before anObject.  May be nil if there is no such sibling"

	| sibling comparator |

	sibling := nil.
	comparator := self searchPolicy.
	aCollection do:
		[:each |
		(comparator compare: each with: anObject) ifTrue: [^ sibling].
		(self isItemDisplayed: each) ifTrue: [sibling := each]].

	^ sibling.!

refreshContents
	"our display needs to be refreshed.  Overridden to force state images
	back to Windows, and to throw away the (expensive) record of what
	used to be expanded but is no longer displayed"

	super refreshContents.
	self
		updateStates;
		discardHiddenExpanded.!

refreshFromModel
	"can be called when it is suspected that the current display differs from what the tree model
	would expect.  Normally this only occurs if the tree model is actually virtual (so that changes
	can happen that we haven't been told about in detail).
	NB: I don't know to what extent this is the *intended* semantics of #refreshContents,
	since the Dolphin implementations don't work as well as I would wish with virtual tree
	models that change arbitrarily, so I don't have anything to compare against"

	self refreshFromModelBelow: nil.!

refreshFromModelBelow: anObject
	"can be called when it is suspected that the current display differs from what the tree model
	would expect.  Normally this only occurs if the tree model is actually virtual (so that changes
	can happen that we haven't been told about in detail).  If anObject is nil then refreshes the
	whole tree"

	"NB1: the current implementation does not make full use of anObject, but will
	usually re-scan the whole tree.

	NB2: I really wish I'd decided to use an additional helper tree model to keep track of
	what the *display* thinks is included and what the *displayed* parent-child relationships
	are"

	| oldSels newOrder legit newSels comparator |

	"there may be nodes (now removed) that are still on our hidden-expanded list; there is no
	way that we can check for that case so just we discard the whole hidden-expanded list"
	self discardHiddenExpanded.

	"if the object is not nil, and is not currently displayed, then there's no need to do more"
	(anObject notNil and: [(self isItemDisplayed: anObject) not])
		ifTrue: [^ self].

	"we'll need to allow for selection changes"
	oldSels := self selections.

	"work out what *should* be displayed, and in what order.
	This scan is based on the *tree model's* data, so it will only find the items
	that are legitimately displayed"
	newOrder := OrderedCollection new.
	self displayedTreeDo: [:each | newOrder addLast: each].
	legit := self makeSetFrom: newOrder.

	"remove items that are not legitimately included"
	self removeAllExcept: legit.

	"and filter the selection list too"
	newSels := oldSels select: [:each | legit includes: each].

	"now we have to add any 'new' items.  We get lazy, just add them at the
	end then update the display order"
	self model do: [:each | legit remove: each ifAbsent: []].
	legit do: [:each | self addItem: each afterIndex: self model size].
	self updateDisplayOrderFrom: self allDisplayedItems to: newOrder.

	"and ensure the state images are correct too"
	self updateStates.

	"we may as well make sure..."
	anObject isNil ifFalse: [self onItemUpdated: anObject].

	"and finally, put the selection back to what it should be.
	We have to be carefull here, if the selection has genuinely changed
	then we have to send a selection changed notification, but if the
	selection has not changed, then we *still* have to set the selection
	(since the Window's selection won't have been updated to reflect
	any changes to the order of items) but we have to ensure that the
	*don't* send spurious selection changing events in that case.
	Sigh..."
	newSels size = oldSels size
		ifTrue: [self presenter noEventsDo: [self selections: newSels]]
		ifFalse: [self selections: newSels].!

refreshNonVirtual
	"private -- overridden to refresh the 'state' too"

	super refreshNonVirtual.

	"NB: this should be unecessary, but the superclasses handling of raw element insertion is too
	much of a minefield for me to mess with it.  See ListView>>addNonVirtualItems:atIndex: for
	why I'd rather not try to override it to set the state directly"
	self unorderedRoots do: [:each | self setItemState: each].

!

removeAllExcept: aSet
	"private -- remove all the items that are not in aSet. The set is assumed to have the
	correct search policy, and to contain only legit items -- i.e. ones that are both
	displayed *and* still included in our tree model"

	self model size to: 1 by: -1 do:
		[:i || each |
		each := self model at: i.
		(aSet includes: each) ifFalse:
			[self model removeAtIndex: i.
			self
				removeExpanded: each;
				removeHiddenExpanded: each;
				notifyNodeRemoved: each]].
!

removeExpanded: anObject
	"private -- ensure that anObject is not on our list of expanded
	nodes"

	expanded remove: anObject ifAbsent: [].!

removeFromDisplay: anObject
	"private -- ensure that anObject is not displayed"

	self model removeAtIndex: (self displayIndexOf: anObject ifAbsent: [^ self]).

	self notifyNodeRemoved: anObject.

!

removeHiddenExpanded: anObject
	"private -- ensure that anObject is not on our hidden expanded list (if we have one). 
	This is called in three circumstances:
	-	when anObject has been expanded (since it is no longer *hidden*)
	-	when the user has explicitly expanded *or* collapsed a node
	-	when we find that anObject is no longer part of the tree"

	self retainExpanded ifTrue: [hiddenExpanded remove: anObject ifAbsent: []].!

removeZombies
	"private -- when an object is removed, we need to remove any displayed children too;
	but this is a problem: our tree model is no longer willing to admit that the removed object
	*has* any children, or indeed that it exists at all.  This is a fundamental architectural
	problem with MVC/MVP-type frameworks where the View needs to respond to changes
	*after* they have happened, and so the data needed to 'unwind' the display has already
	been trashed.  All we can do is scan our displayed items removing any that the model is
	no longer willing to countenance"

	| legit |

	"this scan is based on the *tree model's* data, so it will only find the items
	that are legitimately displayed"
	legit := self makeSet.
	self unorderedTreeDo: [:each | legit add: each].

	"remove any that are not in that set"
	self removeAllExcept: legit.

	#CUtodo.  "should we do an #updateStates here ?"!

resetTreeToRoots
	"private -- go back to our TreeModel and ensure that our tree displays
	all and only its current roots"

	self discardAllExpanded.	
	self model list: (OrderedCollection withAll: self roots).	"be sure to make a copy"!

resolutionScaledBy: scale
	"private -- I'm not sure what this does, but it's copied from TreeView"

	indentSeparation := (indentSeparation * scale x) truncated.
	super resolutionScaledBy: scale


!

retainExpanded
	"answer whether we retain a record of which items were expanded even
	when they are not displayed, this is the default (even though it is expensive
	and irritating) 'cos that's the way that TreeView does it"

	^ hiddenExpanded notNil.
!

retainExpanded: aBool
	"set whether we retain a record of which items were expanded even
	when they are not displayed, this is the default"

	aBool = self retainExpanded ifFalse:
		[hiddenExpanded := aBool ifTrue: [self makeSet] ifFalse: [nil]].!

roots
	"answer our TreeModel's idea of its roots in the order specified by our
	sortblock, etc"

	^ self inRootOrder: self unorderedRoots.!

searchPolicy
	"answer the SearchPolicy that is in use.
	This is implicit in our model, hence there is no corresponding
	setter"

	"we could use:
		^ self model searchPolicy.
	which would give the same answer"
	^ treeModel searchPolicy.!

selectFirstChildOf: anObject because: aSymbol
	"private -- see if it's acceptable to select anObject's first child (it it has one), and if so then
	do it.
	This is part of the implementation of the right-arrow-key shortcut"

	| children child all |

	children := self childrenOf: anObject.
	child := children at: 1 ifAbsent: [^ self].
	all := Array with: child.
	(self checkSelectAll: all because: aSymbol) ifTrue: [self selections: all].!

selection: anObject ifAbsent: a0Block
	"set our selection to anObject answering anObject.  If it is not present in our tree, then answer the result
	of evaluating the <nildadicValuable>, a0Block.
	NB: unlike all the other instance-side methods, this is not duplicated in MutlipleSelectionListTreeView
	(mainly because the argument is of a different type)"

	"ensure that the proposed new selection is actually displayed before we
	allow the superclass definition to take over"
	#CUtodo.  "if the TreeModel throws an exception from #ensureItemDisplayed: then we should
			evaluate a0Block, not allow the error to propogate outwards"
	self ensureItemDisplayed: anObject.
	^ super selection: anObject ifAbsent: a0Block.
!

selectionAfterCollapseList: aCollection
	"private -- answer what the selection list *would* be if we were to collapse each
	item in aCollection.
	If no change is implied then answer nil"

	| changes newSelections |

	changes := false.
	newSelections := self selectionsAsSet.
	aCollection do:
		[:each |
		self unorderedTreeBelow: each do:
			[:child | (newSelections includes: child) ifTrue:
					[newSelections remove: child.
					newSelections add: each.
					changes := true]]].
	^ changes
		ifTrue: [newSelections asOrderedCollection]
		ifFalse: [nil].
!

selectionAfterExpandAllList: aCollection
	"private -- answer what the selection list *would* be if we were to 'expand all'
	each item in aCollection.
	If no change is implied then answer nil.
	NB: we assume that each item in aCollection is expandable"

	| expand newSelection |

	"there'll be no change unless we want autoselection (the user holding down the
	shift key and we support multiple selection)"
	self wantExtendedSelection ifFalse: [^ nil].

	"the only items that can have any effect are the ones that are selected"
	newSelection := self selectionsAsSet.
	expand := aCollection select: [:each | newSelection includes: each].
	expand isEmpty ifTrue: [^ nil].

	"but all their (recursive) children will get selected"
	newSelection := self selection asOrderedCollection.
	expand do: [:each | newSelection addAll: (treeModel withAllChildren: each)].

	^ newSelection.!

selectionAfterExpandList: aCollection
	"private -- answer what the selection list *would* be if we were to expand each
	item in aCollection.
	If no change is implied then answer nil.
	NB: we assume that each item in aCollection is expandable"

	| expand newSelection |

	"there'll be no change unless we want autoselection (the user holding down the
	shift key and we support multiple selection)"
	self wantExtendedSelection ifFalse: [^ nil].

	"the only items that can have any effect are the ones that are selected"
	newSelection := self selectionsAsSet.
	expand := aCollection select: [:each | newSelection includes: each].
	expand isEmpty ifTrue: [^ nil].

	"but all their children will get selected, and *their* children too if they are hidden-expanded"
	newSelection := self selection asOrderedCollection.
	expand do: [:each | self displayedOrHiddenTreeBelow: each do: [:child | newSelection addLast: child]].

	^ newSelection.!

selectionOrRoots
	"private -- answer the current selection or, if there is no selection then
	our current roots"

	| list |

	list := self selections.
	^ list isEmpty
		ifTrue: [self unorderedRoots]
		ifFalse: [list].!

selectionRoots
	"private -- answer a collection of the current selection with all items removed
	that are children of other items in the collection"

	| list set |

	list := self selections.
	list size <= 1ifTrue: [^ list].

	set := self makeSetFrom: list.
	^ list reject: [:each | self parentsOf: each includesAny: set].
	!

selectionRootsOrRoots
	"private -- answer the current selection (with all items removed that are
	children of other items in the collection) or, if there is no selection then
	our current roots"

	| list |

	list := self selectionRoots.
	^ list isEmpty
		ifTrue: [self unorderedRoots]
		ifFalse: [list].!

selectionsAsSet
	"answer our current selection as a Set that uses our current
	seach policy"

	^ self makeSetFrom: self selections.!

selectParentOf: anObject because: aSymbol
	"private -- see if it's acceptable to select anObject's parent (it it has one), and if so then
	do it.
	This is part of the implementation of the left-arrow-key shortcut"

	| parent all |

	parent := (self parentOf: anObject) ifNil: [^ self].
	all := Array with: parent.
	(self checkSelectAll: all because: aSymbol) ifTrue: [self selections: all].!

setItem: anObject indentation: anInteger
	"private -- update the indentation for anObject.
	Note: this causes a Windows error if our control is virtual"

	| objectIndex anLvItem |

	objectIndex := self displayIndexOf: anObject ifAbsent: [^ self].

	anLvItem := (LVITEM new)
			iItem: objectIndex - 1;
			indent: anInteger;
			yourself.

	self lvmSetItem: anLvItem.

	self invalidateItemIndex: objectIndex .
!

setItem: anObject openState: aSymbol
	"private -- update the 'state' image for anObject"

	| objectIndex stateIndex anLvItem |

	self systemDrawsStateImages ifFalse: [^ self].

	objectIndex := self displayIndexOf: anObject ifAbsent: [^ self].
	stateIndex := self stateImageIndex: aSymbol.

	anLvItem := (LVITEM new)
			iItem: objectIndex - 1;
			stateImageIndex: stateIndex;		"a 1-based index in Windows !!"
			yourself.

	self lvmSetItem: objectIndex - 1 state: anLvItem.

	self invalidateItemIndex: objectIndex .


!

setItemIndentation: anObject
	"private -- update the indent for anObject.
	Note: this causes a Windows error if our control is virtual"

	self
		setItem: anObject
		indentation: (self indentationFor: anObject).!

setItemState: anObject
	"private -- update the 'state' image for anObject"

	self
		setItem: anObject
		openState: (self stateFor: anObject).!

setModel: aListOrTreeModel
	"private -- called to set our model.  Since we play games with the model (the one we hold may
	not be the one supplied), we override to dispatch to the 'real' model setters"

	"this will actually (probably) end up calling super>>setModel: with a different model" 
	^ (aListOrTreeModel respondsTo: #roots)
		ifTrue: [self treeModel: aListOrTreeModel]
		ifFalse: [self listModel: aListOrTreeModel].!

sortBlock
	"answer the <diadicValuable> that we use to sort the roots, and the children of
	any parent.  May be nil"

	^ sortBlock.!

sortBlock: a2Block
	"set the <diadicValuable> that we will use to sort the roots, and the children of
	any parent.  May be nil, in which case we use the natural sort order as provided
	by our TreeModel"

	sortBlock = a2Block
		ifTrue: [sortBlock := a2Block]
		ifFalse: [sortBlock := a2Block. self updateDisplayOrder].
!

sortOnColumn: aListViewColumn
	"sort the receiver according to the sort block in aListViewColumn"

	| sorter |

	aListViewColumn isSortable ifFalse: [^ self].

	sorter := self presenter.

	"ugly hack: don't go back to vanilla presenters -- they don't understand how to sort trees"
	#CUtodo.  "make this more flexible"
	(sorter == self or: [sorter isKindOf: ListTreePresenter])
		ifFalse: [sorter := self].

	Cursor wait showWhile: [sorter beSorted: aListViewColumn rowSortBlock].!

stateFor: anObject
	"private -- answer the 'state' identifier for anObject"

	^ (self isItemExpanded: anObject)
			ifTrue: [self openState]
			ifFalse: [(self hasChildren: anObject)
				ifTrue: [self closedState]
				ifFalse: [#Leaf]].!

stateImageIndex: aSymbol
	"private -- answer the state image index corresponding to aSymbol"

	^ self class stateNames indexOf: aSymbol ifAbsent: [0].

!

stateIndexFromRow: anObject
	"private -- answer the index of the state image that is appropriate for anObject"

	^ self stateImageIndex: (self stateFor: anObject).!

stbSaveOn: anSTBOutFiler
	"save a binary representation of the receiver to anSTBOutFiler.
	Overriden not to save the contents of the hiddenExpanded collection too"

	| saved |

	saved := hiddenExpanded.
	self discardHiddenExpanded.	"don't set it to nil or the resurected version will never use hiddenExpanded"

	^ [super stbSaveOn: anSTBOutFiler]
		ensure: [hiddenExpanded := saved].!

systemDrawsStateImages
	"private -- answer whether we can let Windows draw our state image (normally we can)"

	"in point of fact, the only case where Window's doesn't get it right is
	when we are a multi-line EditableListTreeView"
	^ true.!

toggleExpandAll: anObject
	"expand/collapse the entire tree below anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject]
		ifFalse: [self expandAll: anObject].!

toggleExpanded: anObject
	"expand/collapse the tree view under anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject]
		ifFalse: [self expand: anObject].!

treeModel
	"answer our TreeModel.  If the receiver's model as originally supplied was a ListModel,
	then the TreeModel will be an internally generated VirtualTreeModel using our pluggable
	blocks from the #hasChildrenBlock: #childrenOfBlock: and #parentOfBlock: aspects"

	^ treeModel.!

treeModel: aTreeModel
	"private -- set the TreeModel for us to use.  This is invoked indirectly from #model: if the supplied
	model is a TreeModel.  We create an internal ListModel to use as our #model that will
	be changed as the display changes"

	| newListModel |

	"make an internal ListModel to hold the actual display, initialising it with the roots
	and search policy of the tree model, this will be passed to our superclass as
	our 'real' model"
	newListModel := self
				makeInternalListModel: aTreeModel roots
				searchPolicy: aTreeModel searchPolicy.

	hasExplicitTreeModel := true.
	self treeModel: aTreeModel listModel: newListModel.!

treeModel: aTreeModel listModel: aListModel
	"private -- set the Models for us to use.  The TreeModel is only used internally, the ListModel (which
	*must not* be the same as the ListModel of our Presenter if that happens to use one) is passed
	to our inherited implementation"

	treeModel := aTreeModel.
	super setModel: aListModel.
	self discardAllExpanded.
!

unorderedChildrenOf: anObject
	"answer our TreeModel's idea of anObject's immediate children without applying
	our sortblock"

	^ treeModel childrenOf: anObject!

unorderedRoots
	"answer our TreeModel's idea of its roots without applying our sortblock"

	^ treeModel roots.!

unorderedTreeBelow: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at, but not including, aParent,
	evaluating a1Block for each discovered node.  This is a faster version of
	#displayedTreeBelow:do that ignores the ordering implied by our sortblocks"

	(self isItemExpanded: aParent) ifTrue:
		[(self unorderedChildrenOf: aParent) do: [:each | self unorderedTreeOf: each do: a1Block]].!

unorderedTreeDo: a1Block
	"traverse the displayed part of our tree in pre-order evaluating
	a1Block for each discovered node.  This is a faster version of
	#displayedTreeDo: that ignores the ordering implied by our
	sortblocks"

	self unorderedRoots do: [:each | self unorderedTreeOf: each do: a1Block].!

unorderedTreeOf: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at aParent, evaluating
	a1Block for each discovered node.  This is a faster version of
	#displayedTreeOf:do: that ignores the ordering implied by our
	sortblocks"

	a1Block value: aParent.
	self unorderedTreeBelow: aParent do: a1Block.!

updateDisplayOrder
	"private -- ensure that the 'real' list model is showing items in the same order as we expect"

	| oldOrder newOrder comparator firstDiff lastDiff |

	"work out what order we are in"
	oldOrder := self allDisplayedItems.

	"work out what order we *should* be in"
	newOrder := OrderedCollection new: self model size.
	self displayedTreeDo: [:each | newOrder addLast: each].

	"they *should* be the same size, but in case not...
	NB: apart from this one bit of defensive coding we assume that the two versions are
	re-orderings of the *same* elements"
	(newOrder size = oldOrder size) ifFalse: [^ self refreshFromModel].

	self updateDisplayOrderFrom: oldOrder to: newOrder.
!

updateDisplayOrderFrom: anOldOrder to: aNewOrder
	"private -- change the currently displayed order of items from that given
	by the <sequenceableCollection> anOldOrder to that given by the
	<sequenceableCollection> aNewOrder.
	NB: assumes that anOldOrder is a correct reflection of the current
	status, and that the two collections contain the same items and
	are the same size"

	| comparator firstDiff lastDiff |

	"compare"
	comparator := self searchPolicy.
	firstDiff := (1 to: aNewOrder size)
			detect: [:i | (comparator compare: (anOldOrder at: i)
			with: (aNewOrder at: i)) not] ifNone: [^ self].
	lastDiff := (aNewOrder size to: 1 by: -1)
			detect: [:i | (comparator compare: (anOldOrder at: i)
			with: (aNewOrder at: i)) not].

	"and fixup"
	self model
		replaceFrom: firstDiff
		to: lastDiff
		with: aNewOrder
		startingAt: firstDiff.
!

updateExpanded
	"private -- ensure that our idea of what is 'expanded' is still correct; this called when we suspect
	that an item that used to have children no longer does.  NB currently there is no need for checking
	for items that have *gained* children"

	expanded removeAll: (expanded reject: [:each | treeModel hasChildren: each]).
!

updateMode
	"supplied for consistancy with TreeView>>updateMode.  Actually we always
	reply #dynamic (mainly because Windows keeps asking for the indentation even
	after it has been told it; otherwise we would be #dynamic or #static according
	as we are #isVirtual)"

	^ #dynamic.!

updateMode: aSymbol
	"supplied for consistancy with TreeView>>viewMode.  Actually we ignore this"!

updateStates
	"private -- update the 'state' images all objects"

	self model do: [:each | self setItemState: each].!

useSmartRefresh
	"set whether we attempt to preserve open subbranches of the tree when we
	are told that the model has changed below some node"

	^ options allMask: UseSmartRefreshMask.
!

useSmartRefresh: aBool
	"set whether we attempt to preserve open subbranches of the tree when we
	are told that the model has changed below some node"

	options := options mask: UseSmartRefreshMask set: aBool.
!

veryBasicCollapse: anObject
	"private -- recursively collapse all expanded children of anObject, and
	remove them from the display.
	NB: assumes none of the children of anObject are selected.
	NB: assumes anObject is expanded"

	"kill off any children (he he).
	NB: do it in reverse of display order to reduce the amount of data that
	is refreshed for each deletion (it'd be nice if ListModel had optimised forms
	of bulk removal)"
	(self childrenOf: anObject) reverseDo:
		[:each |
		(self isItemExpanded: each) ifTrue:
			[self veryBasicCollapse: each].
		self removeFromDisplay: each].

	"remember that anObject was expanded, but isn't anymore"
	self
		addHiddenExpanded: anObject;
		removeExpanded: anObject.

	self notifyNodeCollapsed: anObject.!

viewMode: aSymbol
	"sets the view mode of the receiver to aSymbol.
	Overridden to understand the TreeView modes #noIcons and #smallIcons"

	(aSymbol == #noIcons or: [aSymbol == #smallIcons])
		ifFalse: [^ super viewMode: aSymbol].

	"this isn't quite right, since we really don't take proper account of the various image blocks,
	but it'll do for its main purpose which is to make mutating TreeViews (in the View Composer)
	work better"
	(aSymbol == #noIcons) ifTrue:
		[self getImageBlock: nil.
		self hasColumnImages: false].
	^ super viewMode: #report.
!

wantExtendedSelection
	"private -- answer whether we should interpret the current user gesture as a request to
	extend the selection"

	"this is silly"
	^ (self isKindOf: MultipleSelectionListView) and: [Keyboard default isShiftDown].! !
!ListTreeView categoriesFor: #addAll:afterIndex:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #addExpanded:!do copy!helpers!private! !
!ListTreeView categoriesFor: #addHiddenExpanded:!do copy!helpers!private! !
!ListTreeView categoriesFor: #addItem:afterIndex:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #additem:toParent:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #additem:toParent:withSiblings:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #allDisplayedItems!accessing!do copy!public! !
!ListTreeView categoriesFor: #allExpandedItems!accessing!do copy!public! !
!ListTreeView categoriesFor: #allHiddenExpandedItems!accessing!do copy!public! !
!ListTreeView categoriesFor: #applyImageLists!do copy!image management!private! !
!ListTreeView categoriesFor: #basicCollapse:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #basicOnItem:movedToParent:!do copy!event handling!private! !
!ListTreeView categoriesFor: #beNoIcons!do copy!modes!public! !
!ListTreeView categoriesFor: #beSorted:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #checkChangeSelectionFrom:to:because:!do copy!events!helpers!private! !
!ListTreeView categoriesFor: #checkSelectAll:because:!do copy!helpers!private! !
!ListTreeView categoriesFor: #childrenOf:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #closedState!do copy!private!states! !
!ListTreeView categoriesFor: #collapse!commands!do copy!public! !
!ListTreeView categoriesFor: #collapse:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #collapse:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #collapseAll!commands!do copy!public! !
!ListTreeView categoriesFor: #collapseList:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #collapseOrSelectParentBecause:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #collapseSelectedBecause:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #connectModel!do not copy!initializing!models!public! !
!ListTreeView categoriesFor: #connectTreeModel!do copy!initializing!models!private! !
!ListTreeView categoriesFor: #countDisplayedChildrenOf:!do copy!helpers!private! !
!ListTreeView categoriesFor: #defaultIndentSeparation!constants!do copy!private! !
!ListTreeView categoriesFor: #depthOf:!accessing!do copy!public! !
!ListTreeView categoriesFor: #disableExpandAll!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #disableExpandAll:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #discardAllExpanded!do copy!helpers!private! !
!ListTreeView categoriesFor: #discardExpanded!do copy!helpers!private! !
!ListTreeView categoriesFor: #discardHiddenExpanded!do copy!helpers!operations!public! !
!ListTreeView categoriesFor: #disconnectFromModel!do copy!initializing!models!public! !
!ListTreeView categoriesFor: #displayedContents!accessing!do copy!public! !
!ListTreeView categoriesFor: #displayedOrHiddenTreeBelow:do:!do copy!enumerating!private! !
!ListTreeView categoriesFor: #displayedTreeBelow:do:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #displayedTreeDo:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #displayedTreeOf:do:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #displayIndexOf:!do copy!helpers!private! !
!ListTreeView categoriesFor: #displayIndexOf:ifAbsent:!do copy!helpers!private! !
!ListTreeView categoriesFor: #doSortChildrenBlock!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #doSortChildrenBlock:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #doSortRoots!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #doSortRoots:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #ensureItemDisplayed:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #expand!commands!do copy!public! !
!ListTreeView categoriesFor: #expand:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #expand:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandAll!commands!do copy!public! !
!ListTreeView categoriesFor: #expandAll:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #expandAllList:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandAllOrCollapseSelectedBecause:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandList:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandOrCollapse:because:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandOrSelectFirstChildBecause:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #expandSelectedBecause:!do copy!private!user initiated actions! !
!ListTreeView categoriesFor: #getChildrenBlock!accessing!do copy!public! !
!ListTreeView categoriesFor: #getChildrenBlock:!accessing!do copy!public! !
!ListTreeView categoriesFor: #getParentBlock!accessing!do copy!public! !
!ListTreeView categoriesFor: #getParentBlock:!accessing!do copy!public! !
!ListTreeView categoriesFor: #hasButtons!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #hasButtons:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #hasCheckBoxes!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #hasCheckBoxes:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #hasChildren:!do copy!public!testing! !
!ListTreeView categoriesFor: #hasChildrenBlock!accessing!do copy!public! !
!ListTreeView categoriesFor: #hasChildrenBlock:!accessing!do copy!public! !
!ListTreeView categoriesFor: #hasHotTracking!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #hasHotTracking:!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #hasIcons!do copy!public!testing! !
!ListTreeView categoriesFor: #hasLines!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #hasLines:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #hasLinesAtRoot!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #hasLinesAtRoot:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #indentationFor:!do copy!private!states! !
!ListTreeView categoriesFor: #indentFromRow:!adapters!do copy!private! !
!ListTreeView categoriesFor: #indentSeparation!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #indentSeparation:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #initialize!do copy!initializing!private! !
!ListTreeView categoriesFor: #inOrder:parent:!do copy!private!sorting! !
!ListTreeView categoriesFor: #inRootOrder:!do copy!private!sorting! !
!ListTreeView categoriesFor: #invalidateItem:!do copy!helpers!private! !
!ListTreeView categoriesFor: #invalidateItemIndex:!do copy!helpers!private! !
!ListTreeView categoriesFor: #isItemDisplayed:!do copy!public!testing! !
!ListTreeView categoriesFor: #isItemExpandable:!do copy!public!testing! !
!ListTreeView categoriesFor: #isItemExpanded:!do copy!public!testing! !
!ListTreeView categoriesFor: #isItemHiddenExpanded:!do copy!public!testing! !
!ListTreeView categoriesFor: #isLargeIcons!do copy!public!testing! !
!ListTreeView categoriesFor: #isMonoExpandable!accessing-styles!do copy!public!testing! !
!ListTreeView categoriesFor: #isMonoExpandable:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #isSelected:!do copy!public!testing! !
!ListTreeView categoriesFor: #isSmallIcons!do copy!public!testing! !
!ListTreeView categoriesFor: #isVirtual:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #listModel:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #makeInternalListModel:searchPolicy:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #makeInternalTreeModel:searchPolicy:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #makeSet!do copy!helpers!private! !
!ListTreeView categoriesFor: #makeSetFrom:!do copy!helpers!private! !
!ListTreeView categoriesFor: #minimumIndent!constants!do copy!public! !
!ListTreeView categoriesFor: #notifyNodeAdded:!do copy!events!private! !
!ListTreeView categoriesFor: #notifyNodeCollapsed:!do copy!events!private! !
!ListTreeView categoriesFor: #notifyNodeExpanded:!do copy!events!private! !
!ListTreeView categoriesFor: #notifyNodeRemoved:!do copy!events!private! !
!ListTreeView categoriesFor: #onDestroyed!do copy!event handling!models!public! !
!ListTreeView categoriesFor: #onDisplayDetailsRequired:!do copy!event handling!private! !
!ListTreeView categoriesFor: #onFullyCreated!do copy!event handling!public! !
!ListTreeView categoriesFor: #onItem:addedInParent:!do copy!event handling!public! !
!ListTreeView categoriesFor: #onItem:movedToParent:!do copy!event handling!public! !
!ListTreeView categoriesFor: #onItem:removedFromParent:!do copy!event handling!public! !
!ListTreeView categoriesFor: #onItemUpdated:!do copy!event handling!public! !
!ListTreeView categoriesFor: #onKeyPressed:!do copy!event handling!public!user initiated actions! !
!ListTreeView categoriesFor: #onLeftButtonDoubleClicked:!do copy!event handling!public!user initiated actions! !
!ListTreeView categoriesFor: #onLeftButtonPressed:!do copy!event handling!public!user initiated actions! !
!ListTreeView categoriesFor: #onTreeChanged:!do copy!event handling!public! !
!ListTreeView categoriesFor: #openState!do copy!private!states! !
!ListTreeView categoriesFor: #parentOf:!accessing!do copy!public! !
!ListTreeView categoriesFor: #parentsOf:includesAny:!do copy!helpers!private! !
!ListTreeView categoriesFor: #previousDisplayedSiblingOf:in:!do copy!helpers!private! !
!ListTreeView categoriesFor: #refreshContents!do copy!public!updating! !
!ListTreeView categoriesFor: #refreshFromModel!do copy!public!updating! !
!ListTreeView categoriesFor: #refreshFromModelBelow:!do copy!public!updating! !
!ListTreeView categoriesFor: #refreshNonVirtual!do copy!private!updating! !
!ListTreeView categoriesFor: #removeAllExcept:!do copy!expanding/collapsing!private!updating! !
!ListTreeView categoriesFor: #removeExpanded:!do copy!helpers!private! !
!ListTreeView categoriesFor: #removeFromDisplay:!do copy!private! !
!ListTreeView categoriesFor: #removeHiddenExpanded:!do copy!helpers!private! !
!ListTreeView categoriesFor: #removeZombies!do copy!private!updating! !
!ListTreeView categoriesFor: #resetTreeToRoots!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #resolutionScaledBy:!do copy!geometry!private! !
!ListTreeView categoriesFor: #retainExpanded!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #retainExpanded:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #roots!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #searchPolicy!accessing!do copy!public! !
!ListTreeView categoriesFor: #selectFirstChildOf:because:!do copy!private!selection!user initiated actions! !
!ListTreeView categoriesFor: #selection:ifAbsent:!do not copy!public!selection! !
!ListTreeView categoriesFor: #selectionAfterCollapseList:!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionAfterExpandAllList:!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionAfterExpandList:!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionOrRoots!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionRoots!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionRootsOrRoots!do copy!private!selection! !
!ListTreeView categoriesFor: #selectionsAsSet!accessing!do copy!public!selection! !
!ListTreeView categoriesFor: #selectParentOf:because:!do copy!private!selection!user initiated actions! !
!ListTreeView categoriesFor: #setItem:indentation:!do copy!private!states! !
!ListTreeView categoriesFor: #setItem:openState:!do copy!private!states! !
!ListTreeView categoriesFor: #setItemIndentation:!do copy!private!states! !
!ListTreeView categoriesFor: #setItemState:!do copy!private!states! !
!ListTreeView categoriesFor: #setModel:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #sortBlock!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #sortBlock:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #sortOnColumn:!columns!do copy!public!sorting! !
!ListTreeView categoriesFor: #stateFor:!do copy!private!states! !
!ListTreeView categoriesFor: #stateImageIndex:!do copy!private!states! !
!ListTreeView categoriesFor: #stateIndexFromRow:!adapters!do copy!private! !
!ListTreeView categoriesFor: #stbSaveOn:!binary filing!do copy!public! !
!ListTreeView categoriesFor: #systemDrawsStateImages!do not copy!private!states!testing! !
!ListTreeView categoriesFor: #toggleExpandAll:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #toggleExpanded:!do copy!expanding/collapsing!public! !
!ListTreeView categoriesFor: #treeModel!accessing!do copy!models!public! !
!ListTreeView categoriesFor: #treeModel:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #treeModel:listModel:!accessing!do copy!models!private! !
!ListTreeView categoriesFor: #unorderedChildrenOf:!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #unorderedRoots!accessing!do copy!public!sorting! !
!ListTreeView categoriesFor: #unorderedTreeBelow:do:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #unorderedTreeDo:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #unorderedTreeOf:do:!do copy!enumerating!public! !
!ListTreeView categoriesFor: #updateDisplayOrder!do copy!private!updating! !
!ListTreeView categoriesFor: #updateDisplayOrderFrom:to:!do copy!private!updating! !
!ListTreeView categoriesFor: #updateExpanded!do copy!private!updating! !
!ListTreeView categoriesFor: #updateMode!accessing!do copy!public! !
!ListTreeView categoriesFor: #updateMode:!accessing!do copy!public! !
!ListTreeView categoriesFor: #updateStates!do copy!private!states!updating! !
!ListTreeView categoriesFor: #useSmartRefresh!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #useSmartRefresh:!accessing-styles!do copy!public! !
!ListTreeView categoriesFor: #veryBasicCollapse:!do copy!expanding/collapsing!private! !
!ListTreeView categoriesFor: #viewMode:!accessing!do copy!public! !
!ListTreeView categoriesFor: #wantExtendedSelection!do copy!private!testing! !

ListTreeView methodProtocol: #treeView attributes: #(#readOnly) selectors: #(#collapse: #expand: #expandAll: #onItem:addedInParent: #onItem:movedToParent: #onItem:removedFromParent: #onItemUpdated: #onTreeChanged:)!

!ListTreeView class methodsFor!

configureExampleView: aListTreeView
	"private -- helper for the examples"

	aListTreeView
		addColumn: ListViewColumn new;
		addColumn: ListViewColumn new;
		addColumn: ListViewColumn new;
		yourself.

	(aListTreeView allColumns at: 1)
		text: 'Name';
		width: 250;
		getImageBlock: IconicListAbstract.

	(aListTreeView allColumns at: 2)
		text: 'Changed';
		width: 20;
		getContentsBlock: [:it | it owningPackage ifNil: [false] ifNotNil: [:pg | pg isChanged]];
		getTextBlock: nil;
		getImageBlock: [:it | it ifTrue: [Package changedIcon imageIndex]];
		sortBlock: [:e1 :e2 | e1 or: [e2 not]].

	(aListTreeView allColumns at: 3)
		text: 'Package';
		width: 200;
		getContentsBlock: (Message selector: #owningPackage);
		getImageBlock: IconicListAbstract;
		sortBlock: [:e1 :e2 | e1 displayString <= e2 displayString].

	"this will be the first column in the actual display"
	(aListTreeView allColumns at: 4)
		text: 'Changed';
		width: 20;
		getContentsBlock: [:it | it notNil and: [it isChanged]];
		getTextBlock: nil;
		getImageBlock: [:it | it ifTrue: [Package changedIcon imageIndex]];
		sortBlock: [:e1 :e2 | e1 or: [e2 not]].

	aListTreeView
		hasLinesAtRoot: true;
		hasFullRowSelect: false;
		hasColumnImages: true;
		columnOrder: #(4 1 2 3);
		yourself.!

convertToLocalVersion0: anArray from: anInteger
	"private -- answer a version of anArray that represents at least version 0 of our instances'
	layout"

	"the recursion grounds here"
	^ anArray.!

convertToLocalVersion1: anArray from: anInteger
	"private -- answer a version of anArray that represents at least version 1 of our instances'
	layout"

	| array index |

	anInteger >= 1 ifTrue: [^ anArray].

	array := self convertToLocalVersion0: anArray from: anInteger.

	"going from 0 to 1 changed the types of expanded and hiddenExpanded to PluggableSet"
	index := self indexOfInstVar: 'expanded'.
	array at: index put: (self setClass withAll: (array at: index)).
	index := self indexOfInstVar: 'hiddenExpanded'.
	(array at: index) isNil ifFalse: [array at: index put: (self setClass withAll: (array at: index))].

	"and 'dummy3' (nee 'selChangeCause') is no longer used"
	index := self indexOfInstVar: 'dummy3'.
	array at: index put: nil.

	^ array.!

defaultOptions
	"answer the bitmap of options that instances have by default"

	^ HasButtonsMask | DoSortRootsMask.!

example1
	"an example of using a ListTreeView to display classes with packages and change indicators.

		self example1.
	"


	| presenter |

	"we use a ListTreeView coupled with a TreePresenter to show a ClassHierarchyModel"
	presenter := TreePresenter show: 'ListTree view' on: ClassHierarchyModel withAllClasses.
	presenter topShell caption: 'Class tree with packages'.

	"add some columns to the view since we want to show them off"
	self configureExampleView: presenter view.

	^ presenter.
!

example2a
	"an example of using a MultipleSelectionListTreeView to display classes with packages and change indicators.

		self example2a.
	"

	| presenter |

	"we use a MultipleSelectionListTreeView coupled with a ListPresenter to show a ClassHierarchyModel.
	We can't use a Treepresenter because we want to have multi-select, so we use a ListPresenter.
	This is a little iffy because we are passing a model of the wrong type to the ListModel.  It works, but..."
	presenter := ListPresenter show: 'Multi-selection ListTree view' on: ClassHierarchyModel withAllClasses.

	"add some columns to the view since we want to show them off"
	self configureExampleView: presenter view.
	presenter topShell caption: 'Multi-select class tree with packages'.

	^ presenter.
!

example2b
	"an example of using a MultipleSelectionListTreeView to display classes with packages and change indicators.

		self example2b.
	"

	| presenter |

	"we use a MultipleSelectionListTreeView coupled with a ListPresenter to show a class hierarchy.
	We can't use a Treepresenter because we want to have multi-select, so we use a ListPresenter.
	In this case we don't use a TreeModel directly, so we have to tell the ListTreeView what blocks to
	use to get the children/parent of a class (this is only necessary because a default VirtualTreeModel
	doesn't know about classes).
	Since we have not used a ClassHierarchyModel (or equivalent), the new window will *not* reflect
	ongoing changes to the class hierarchy.
	The window is actually opened before the view is configured, so we have to postpone setting
	a model till after that -- this wouldn't be a problem if the MVP triad were being assembled and
	displayed in the normal way"
	presenter := ListPresenter show: 'Multi-selection ListTree view'.
	presenter topShell caption: 'Multi-select class tree with packages'.
	(presenter view)
		getChildrenBlock: (Message selector: #subclasses);
		getParentBlock: (Message selector:#superclass).
	presenter model: (ListModel on: Class allRoots).

	"add some columns to the view since we want to show them off"
	self configureExampleView: presenter view.

	^ presenter.
!

example2c
	"an example of using a MultipleSelectionListTreeView to display classes with packages and change indicators.

		self example2c.
	"

	| presenter |

	"we use a MultipleSelectionListTreeView coupled with a ListTreePresenter to show a ClassHierarchyModel.
	We can't use a TreePresenter because we want to have multi-select, so we use a ListTreePresenter.
	This avoids the (potential) problems in examples 2a and 2b"
	presenter := ListTreePresenter show: 'Multi-selection ListTree view' on: ClassHierarchyModel withAllClasses.
	presenter topShell caption: 'Multi-select class tree with packages'.

	"add some columns to the view since we want to show them off"
	self configureExampleView: presenter view.

	^ presenter.
!

example3
	"an example of using a ListTreeView to display an infinite tree of Strings.
	This illustrates the use of SearchPolicies other than Identity.

		self example3.
	"

	| model presenter view |

	"this is just a handy way of making a non-identity tree model"
	model := (VirtualTreeModelWithSearchPolicy withRoots: #('') searchPolicy: SearchPolicy caseInsensitive)
			hasChildrenBlock: [:it | true];
			getChildrenBlock: [:it | #($A $B $C $D $E $F)  collect: [:each | it copyWith: each]];
			getParentBlock: [:it | it allButLast];
			yourself.

	presenter := ListTreePresenter show: 'Default view' on: model.
	presenter topShell caption: 'Infinite tree of case-insensitive Strings'.

	(presenter view)
		hasLinesAtRoot: true;
		disableExpandAll: true;	"its an infinite tree!!"
"		retainExpanded: false;
"		addColumn: ((ListViewColumn new) getContentsBlock: [:it | it size]; yourself);
		layout;
		yourself.

	"also we can set the selection, etc, to elements of the tree that are only #equal: to some
	supplied value.
	Note, however that the 'discovered' elements follow the peculiar pattern of case of this String
	rather than being all uppercase.  This is a natural consequence of the way that a virtual tree
	must work if it is not using identity"
	presenter selection: 'bAcE'.

	^ presenter.
!

fillStateImageManager
	"private -- [re]fill in the shared ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	| extent box smallBox hLineF hLineT vLineF vLineT boxPen linePen image |

	"there *just has* to be a better way than this grubbing around"

	"I should factor this stuff out"
	extent := self stateImageExtent.
	box := 4@4 corner: 13@13.
	smallBox := 5@5 corner: 12@12.
	hLineF := 6@8. hLineT := 11@8.
	vLineF := 8@6. vLineT := 8@11.
	boxPen := Pen color: Color darkGray.
	linePen := Pen color: Color black.

	"NB: the order of insertion of images *must* match the output of:
		self stateNames.
	"
	stateImageManager maskcolor: Color white.

	"image for state #Leaf"
	image := (Bitmap displayCompatibleWithExtent: extent).
	(image canvas)
		erase;
		free.
	stateImageManager addImage: image.

	"image for state #Open"
	image := (Bitmap displayCompatibleWithExtent: extent).
	(image canvas)
		erase;
		pen: boxPen;
		rectangle: box;
		pen: linePen;
		lineFrom: hLineF to: hLineT;
		free.
	stateImageManager addImage: image.

	"image for state #Closed"
	image := (Bitmap displayCompatibleWithExtent: extent).
	(image canvas)
		erase;
		pen: boxPen;
		rectangle: box;
		pen: linePen;
		lineFrom: hLineF to: hLineT;
		lineFrom: vLineF to: vLineT;
		free.
	stateImageManager addImage: image.

	"image for state #SmallOpen"
	image := (Bitmap displayCompatibleWithExtent: extent).
	(image canvas)
		erase;
		pen: boxPen;
		rectangle: smallBox;
		pen: linePen;
		lineFrom: hLineF to: hLineT;
		free.
	stateImageManager addImage: image.

	"image for state #SmallClosed"
	image := (Bitmap displayCompatibleWithExtent: extent).
	(image canvas)
		erase;
		pen: boxPen;
		rectangle: smallBox;
		pen: linePen;
		lineFrom: hLineF to: hLineT;
		lineFrom: vLineF to: vLineT;
		free.
	stateImageManager addImage: image.

	boxPen free.
	linePen free.!

fillStateImageManagerFrom: anImage
	"private -- [re]fill in the shared ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	| zero extent srcOrigin image |

	stateImageManager maskcolor: Color white.

	extent := self stateImageExtent.
	zero := 0@0.
	srcOrigin := 0@0.

	"NB: the order of insertion of images *must* match the output of:
		self stateNames.
	"
	1 to: 5 do:
		[:i |
		image := (Bitmap displayCompatibleWithExtent: extent).
		(image canvas)
			drawBitmap: anImage at: zero extent: extent from: srcOrigin extent: extent rop: SRCCOPY;
			free.
		stateImageManager addImage: image.
		srcOrigin x: (srcOrigin x + extent x)].
!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	ListTreePresenter addView: self asResource: 'Default view'.
	ListPresenter addView: self asResource: 'ListTree view'.			"NB: don't forget to repackage this"
	TreePresenter addView: self asResource: 'ListTree view'.			"or this"
!

internalListModelClass
	"answer the factory object to use for our instances' internally
	created list models"

	^ ListModel.!

internalTreeModelClass
	"answer the factory object to use for our instances' internal tree
	models (i.e. when one has not been set explicitly)"

	"we use this, rather than VirtualTreeModel, since it supports pluggable
	search policies (hence the name!!)"
	^ VirtualTreeModelWithSearchPolicy.!

localStbVersion
	"private -- answer the 'real' binary filer version number for instances of the receiver.
	This is allowed to vary independenty of our superclass's #stbVersion"

	^ 1.!

onStartup
	"called as the system starts up, ensure that we don't hold onto images from the
	old session"

	stateImageManager := nil.!

publishedAspectsOfInstances
    	"answer the <Aspect>s published by instances of the receiver"
    
	^ (super publishedAspectsOfInstances)

		removeKey: #hasCheckBoxes ifAbsent: [];			"	-- uses the same 'state' stuff as we need for 'buttons'"

		add: (Aspect boolean: #hasButtons);
"		add: (Aspect boolean: #hasLines);					-- dummied out, we don't have any lines :-( "
		add: (Aspect boolean: #hasLinesAtRoot);			"	-- really means #hasButtonsAtRoot"
		add: (Aspect boolean: #hasHotTracking);
"		add: (Aspect boolean: #isMonoExpandable);		-- dummied out, too much effort for no value"
		add: (Aspect integer: #indentSeparation);
		add: (Aspect boolean: #disableExpandAll);
"		add: (Aspect choice: #updateMode from: TreeView updateModes);
											-- dummied out, not meaningful for lists"
		add: (Aspect boolean: #retainExpanded);
		add: (Aspect boolean: #useSmartRefresh);
		add: (Aspect block: #hasChildrenBlock);
		add: (Aspect block: #getChildrenBlock);
		add: (Aspect block: #getParentBlock);

		add: (Aspect block: #sortBlock);
		add: (Aspect boolean: #doSortRoots);
		add: (Aspect block: #doSortChildrenBlock);

		yourself.!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by instances of the receiver.
	ListTreeViews generate a few extra events that would be useful coming
	off TreeViews too (as with most views, the events are actually triggered
	off our presenter)"

	^ (super publishedEventsOfInstances)
		add: #nodeAdded:;
		add: #nodeRemoved:;
		add: #nodeExpanded:;
		add: #nodeCollapsed:;
		yourself.!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.  We use a constants dict rather than class vars so that
	we can share them with MultipleSelectionListTreeView.

		self rebuildPoolConstants.
	"

	(Smalltalk at: #ListTreeConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'DisableExpandAllMask'		put: 16r0001;
		at: 'HasButtonsMask'		put: 16r0002;
		at: 'HasLinesAtRootMask'		put: 16r0004;
		at: 'HasLinesMask'			put: 16r0008;
		at: 'DoSortRootsMask'		put: 16r0010;
		at: 'UseSmartRefreshMask'		put: 16r0020;

		shrink.
!

setClass
	"answer the factory object to use to create instances' lists of
	which objects satisfy some condition"

	^ PluggableSet.!

stateImageBlock: a0Block
	"set the <niladicValueable> that can optionally be used to supply bitmaps for
	the open/closed icons in the tree.  It should answer an Image of size 80@16
	and be made up of 5 sub-images (each 16@16) that correspond, in order, to
	the states identified by #stateNames.  If this is nil (the default) then we generate icons
	that match those in Win2K.  To supply icons like those in a default installation
	of XP, try:

		ListTreeView stateImageBlock: [Bitmap
							fromFile: 'Resources\ListTreeIconsForXP.bmp'
							usingLocator: (PackageRelativeFileLocator packageNamed: 'CU ListTree')].

	which will take effect next time you start the image.  To turn it off again:

		ListTreeView stateImageBlock: nil.

	This isn't the best way of setting thjs kind of configuration, but -- in the absence
	of any Windows-defined place where these icons live, its the best I can be
	bothered with..."

	stateImageBlock := a0Block.!

stateImageExtent
	"answer a Point indicating the size we use for state images.
	Note that the state images don't visibly fill the extent"

	^ self stateImageWidth @ self stateImageHeight.!

stateImageHeight
	"answer the height in pixels of our 'state' images.
	Note that the state images don't visibly fill the extent"

	^ 16.!

stateImageList
	"answer an ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	^ self stateImageManager imageListWithExtent: self stateImageExtent.
!

stateImageManager
	"answer the shared ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	stateImageManager isNil ifTrue: [stateImageManager := ImageManager new].

	stateImageManager size = 0 ifTrue:
		[| image |
		[stateImageBlock isNil ifFalse: [image := stateImageBlock value]]
			on: Exception
			do: [:ex | ex notify. ex return: nil].
		image isNil
			ifTrue: [self fillStateImageManager]
			ifFalse: [self fillStateImageManagerFrom: image]].

	^ stateImageManager.!

stateImageWidth
	"answer the width in pixels of our 'state' images.
	Note that the state images don't visibly fill the extent"

	^ 16.!

stateNames
	"answer an Array of Symbol names of states, these are in the same order
	as the corresponding images in the stateImageList"

	"#SmallOpen and #SmallClosed are not actually used yet"
	^ #(#Leaf #Open #Closed #SmallOpen #SmallClosed).!

stbConvert: anArray fromLocalVersion: anInteger
	"private -- answer a version of anArray that represents the current version of our instances'
	layout.  Note that the version number is our *local* version that varies independently of
	our superclass's version"

	^ self convertToLocalVersion1: anArray from: anInteger.!

stbConvert: anArray fromVersion: anInteger
	"private -- convert from an earlier version by updating and answering the array of instance
	variables.  We have to be tricksy here, since we need to allow for both changes to *our*
	encoding and changes (independantly) made in the super class.  See #stbVersion"

	| localVersion superVersion |

	"decode the two version numbers packed into anInteger"
	localVersion := anInteger quo: self stbEncodingPrime.
	superVersion := anInteger rem: self stbEncodingPrime.

	^ self
		stbConvert: (super stbConvert: anArray fromVersion: superVersion)
		fromLocalVersion: localVersion.!

stbEncodingPrime
	"private -- the prime number that we use to pack two stb versions into one
	integer"

	"should be big enough<pray/> -- allows our superclass to get up to version
	996 before anything breaks"
	^ 997.!

stbVersion
	"answer the current binary filer version number for instances of the receiver.
	Since we need to allow the superclass's version number to increment without
	messing up our own version number, we use an arithmetic coding that allows
	us to 'pack' two version numbers into one integer"

	"we do it by multiplying one by a sufficently large prime, that gives us
	unamgiguity over the range [0, thePrime).
	Note that we multiply *our* local version by the prime, this is necessary
	to allow for older instances to appear to have a local version of 0"
	^ self stbEncodingPrime * self localStbVersion + super stbVersion.!

useIconsLikeWinXP
	"set the open/closed icons to 'resemble' those in a default installation
	of XP.

		self useIconsLikeWinXP.

	which will take effect next time you start the image.  To turn it off again:

		ListTreeView stateImageBlock: nil.

	"

	| bitmap |

	bitmap := Bitmap
			fromFile: 'ListTreeIconsForXP.bmp'
			usingLocator: (PackageResourceLocator packageNamed: 'CU ListTree').

	self stateImageBlock: [bitmap].! !
!ListTreeView class categoriesFor: #configureExampleView:!do not copy!examples!must strip!private! !
!ListTreeView class categoriesFor: #convertToLocalVersion0:from:!binary filing!do copy!private! !
!ListTreeView class categoriesFor: #convertToLocalVersion1:from:!binary filing!do copy!private! !
!ListTreeView class categoriesFor: #defaultOptions!constants!do copy!public! !
!ListTreeView class categoriesFor: #example1!do not copy!examples!must strip!public! !
!ListTreeView class categoriesFor: #example2a!do not copy!examples!must strip!public! !
!ListTreeView class categoriesFor: #example2b!do not copy!examples!must strip!public! !
!ListTreeView class categoriesFor: #example2c!do not copy!examples!must strip!public! !
!ListTreeView class categoriesFor: #example3!do not copy!examples!must strip!public! !
!ListTreeView class categoriesFor: #fillStateImageManager!do not copy!initializing!private! !
!ListTreeView class categoriesFor: #fillStateImageManagerFrom:!do not copy!initializing!private! !
!ListTreeView class categoriesFor: #installViewResources!development!do not copy!must strip!private! !
!ListTreeView class categoriesFor: #internalListModelClass!constants!do not copy!public! !
!ListTreeView class categoriesFor: #internalTreeModelClass!constants!do copy!public! !
!ListTreeView class categoriesFor: #localStbVersion!binary filing!constants!do copy!private! !
!ListTreeView class categoriesFor: #onStartup!do not copy!events!public! !
!ListTreeView class categoriesFor: #publishedAspectsOfInstances!constants!development!do copy!must strip!public! !
!ListTreeView class categoriesFor: #publishedEventsOfInstances!constants!do copy!events!public! !
!ListTreeView class categoriesFor: #rebuildPoolConstants!constants!development!do not copy!initializing!must strip!private! !
!ListTreeView class categoriesFor: #setClass!constants!do copy!public! !
!ListTreeView class categoriesFor: #stateImageBlock:!accessing!do not copy!public! !
!ListTreeView class categoriesFor: #stateImageExtent!constants!do copy!public! !
!ListTreeView class categoriesFor: #stateImageHeight!constants!do copy!public! !
!ListTreeView class categoriesFor: #stateImageList!accessing!do copy!public! !
!ListTreeView class categoriesFor: #stateImageManager!accessing!do not copy!public! !
!ListTreeView class categoriesFor: #stateImageWidth!constants!do copy!public! !
!ListTreeView class categoriesFor: #stateNames!constants!do copy!public! !
!ListTreeView class categoriesFor: #stbConvert:fromLocalVersion:!binary filing!do copy!private! !
!ListTreeView class categoriesFor: #stbConvert:fromVersion:!binary filing!do copy!private! !
!ListTreeView class categoriesFor: #stbEncodingPrime!binary filing!constants!do copy!private! !
!ListTreeView class categoriesFor: #stbVersion!binary filing!do copy!public! !
!ListTreeView class categoriesFor: #useIconsLikeWinXP!do not copy!initializing!public! !

MutipleSelectionListTreeView guid: (GUID fromString: '{E23B321A-F53C-4722-892B-002293AB38E7}')!
MutipleSelectionListTreeView comment: 'Copyright � Chris Uppal, 2002, 2005.
chris.uppal@metagnostic.org

IMPORTANT NOTE:  ALL THE METHODS THAT ARE NOT IN THE ''do not copy'' CATEGORY ARE INTENDED TO BE EXACT COPIES OF THOSE IN ListTreeView.

The point of this class is really that ListView and MultipleSelectionListView should be two modes of one class, not two separate classes.  It has no other purpose than to supply the same functionality as ListTreeView in a context where multiple selection is expected and used by both the user and the client code.

See the ListTreeView class comment for examples and more information.

	-- chris'!
!MutipleSelectionListTreeView categoriesForClass!MVP-Views! !
!MutipleSelectionListTreeView methodsFor!

addAll: aCollection afterIndex: anIndex
	"private -- add all the objects in aCollection after the given index in the displayed list"

	| index |

	index := anIndex.
	aCollection do: [:each |
		self addItem: each afterIndex: index.
		index := index + 1].!

addExpanded: anObject
	"private -- ensure that anObject is on our list of expanded
	nodes"

	expanded add: anObject.!

addHiddenExpanded: anObject
	"private -- ensure that anObject is on our hidden-expanded list, if we have one"

	self retainExpanded ifTrue: [hiddenExpanded add: anObject].

!

addItem: anObject afterIndex: anIndex
	"private -- add aChildObject to our displayed list after the given index in the displayed list"

	"add the object to the 'real' model"
	self model add: anObject afterIndex: anIndex.

	"NB: this would be unecessary, but the superclass's handling of raw element insertion is too much of
	a minefield for me to mess with it.  See ListView>>addNonVirtualItems:atIndex: for why I'd rather not try
	to override it to ensure that the state is set directly as a side-effect of adding the item"
	self setItem: anObject openState: ((self hasChildren: anObject) ifTrue: [self closedState] ifFalse: [#Leaf]).

	self notifyNodeAdded: anObject.
!

additem: aChildObject toParent: aParentObject
	"private -- add aChildObject to our displayed list under aParentObject, and in the
	correct position w.r.t. any sibling objects.
	It is assumed that the parent either is nil (the child is a root) or that it is visible and
	expanded"

	| siblings |

	siblings := aParentObject isNil
			ifTrue: [self roots]
			ifFalse: [self childrenOf: aParentObject].
	self
		additem: aChildObject
		toParent: aParentObject
		withSiblings: siblings.

!

additem: aChildObject toParent: aParentObject withSiblings: aCollection
	"private -- add aChildObject to our displayed list under aParentObject, and in the
	correct position w.r.t. the collection of sibling objects (which may not yet be displayed
	themselves)"

	| index sibling |

	"try to find the correct position to insert it -- initial assumption is just after the parent (if any)"
	index := self displayIndexOf: aParentObject ifAbsent: [0].

	"or better, just after the previous displayed sibling (if any) and its children (if any)"
	sibling := self previousDisplayedSiblingOf: aChildObject in: aCollection.
	sibling isNil ifFalse:
		[index := (self displayIndexOf: sibling) + (self countDisplayedChildrenOf: sibling)].

	"add the object to the 'real' model"
	self addItem: aChildObject afterIndex: index.!

allDisplayedItems
	"answer an OrderedCollection of all the items that are currently displayed.
	(i.e. they are either a root or their parent is expanded).
	They are listed in the order that they are currently displayed"

	^ OrderedCollection withAll: self model.!

allExpandedItems
	"answer an OrderedCollection of all the items that are currently expanded"

	^ OrderedCollection withAll: expanded.!

allHiddenExpandedItems
	"answer an OrderedCollection of all the items that are currently hidden-expanded; that
	is the ones that we will automatically expand as soon as they are visible, because they
	were expanded when their parent was closed"

	| all |

	all := OrderedCollection new.
	self retainExpanded ifTrue: [all addAll: hiddenExpanded].

	^ all.!

applyImageLists
	"private -- set the receiver's image lists from the image managers.
	Overridden to do the state Image list too"

	| images |

	super applyImageLists.

	"note that we don't use the state image list instvar; there's no need since the
	image list is always the same systemwide, and this avoids having to mess with
	STBing images"
	images := self hasButtons
			ifTrue: [self class stateImageList]
			ifFalse: [nil].
	self stateImageList: images.!

basicCollapse: anObject
	"private -- collapse the displayed hierarchy below anObject, and refresh its display.
	NB: assumes none of the children of anObject are selected.
	NB: assumes anObject is expanded"

	"collapse the node and collapse and remove any children.
	Someday I may activate the redraw-suppression code configurably"
		"self disableRedraw. [ Cursor wait showWhile: ["
	self veryBasicCollapse: anObject.
		"] ] ensure: [self enableRedraw]."

	"as with #expand: we allow the tree model to say that an object has children even if
	the actual set thereof is zero-sized, hence we display a #Closed image even if there
	were no actual children to remove"
	self setItem: anObject openState: (self closedState).
	self invalidateItem: anObject.!

basicOnItem: aChildObject movedToParent: aParentObject
	"private -- implementation of #onItem:movedToParent:  This is factored
	out so that the code can be executed without triggering the normal
	events or worrying about handling selection change events.  Note
	that we rely on the caller reseting all the status icons, since otherwise
	there's no way to ensure that the *old* parent's icon will be correct"

	| itemDisplayed itemExpanded parentDisplayed parentExpanded |

	"Warning: sin is prettier than this horribly fragile logic"

	"work out the status quo for the child object"
	(self isItemDisplayed: aChildObject)
		ifTrue: [itemDisplayed := true. itemExpanded := self isItemExpanded: aChildObject]
		ifFalse: [itemDisplayed := itemExpanded := false].

	"ditto for the new parent.  We treat a nil parent (child is a root) as 'expanded' to simplify (ha!!) the logic"
	aParentObject notNil
		ifTrue: [(self isItemDisplayed: aParentObject)
			ifTrue: [parentDisplayed := true. parentExpanded := self isItemExpanded: aParentObject]
			ifFalse: [parentDisplayed := parentExpanded := false]]
		ifFalse: [parentDisplayed := parentExpanded := true].

	"if the item wasn't visible before, then we can early-out, just ensuring that it's added to the new
	parent if necessary.  Remember that the caller will update the status icons for us"
	itemDisplayed ifFalse:
		[parentExpanded ifTrue: [self additem: aChildObject toParent: aParentObject].
		^ self].

	"otherwise the item must be visible, so must ensure that the new parent is expanded, but if the
	parent is not already expanded, then expanding it will create a duplicate of the existing row, so we
	have to remove the old row first in that case.  (BTW, we don't need to worry about the hidden-expanded
	stuff causing aChildObject to be expanded when the new parent is, since aChildObject is displayed
	currently and therefore not on the hidden expanded list)"
	parentExpanded ifFalse:
		[self removeFromDisplay: aChildObject].
	aParentObject isNil ifFalse:
		[self
			ensureItemDisplayed: aParentObject;
			expand: aParentObject].

	"now we know that the item is visible; if it had been visible before, and the new parent was already
	expanded then we'll need to update the display order. The same applies if the item itself was already
	expanded since any children will still be in their old positions"
	(parentExpanded or: [itemExpanded]) ifTrue: [self updateDisplayOrder].

	"and lastly ensure that the indentations are all OK.  This step is unecessary if we're virtual,
	and in fact causes a common control error, so we only do it if we're not"
	self isVirtual ifFalse: [self unorderedTreeOf: aChildObject do: [:each | self setItemIndentation: each]].
!

beNoIcons
	"supplied for consistancy with TreeView"

	self viewMode: #noIcons.!

beSorted: aSortBlock
	"implemented so that we can implement the protocol of our Presenters"

	self sortBlock: aSortBlock.
!

checkChangeSelectionFrom: anOldCollection to: aNewCollection because: aSymbol
	"private -- trigger a #selectionChanging: notification and answer whether the
	Observers thereof were happy for the selection change to go ahead"

	| event |

	event := (SelectionChangingEvent forSource: self)
			oldSelections: anOldCollection;
			newSelections: aNewCollection;
			cause: aSymbol;
			yourself.

	self presenter onSelectionChanging: event.

	^ event value.
!

checkSelectAll: aCollection because: aSymbol
	"private -- trigger a #selectionChanging: notification and answer whether the
	Observers thereof were happy for the selection change to go ahead"

	^ self
		checkChangeSelectionFrom: self selections
		to: aCollection
		because: aSymbol.!

childrenOf: anObject
	"answer our TreeModel's idea of anObject's immediate children in the order
	specified by our sortblock, etc"

	^ self
		inOrder: (self unorderedChildrenOf: anObject)
		parent: anObject.!

closedState
	"private -- answer the Symbol name of the 'closed' state"

	^ self hasIcons ifTrue: [#Closed] ifFalse: [#SmallClosed].
!

collapse
	"collapse the tree view to ensure that the immediate children of the current selection,
	if any, are not displayed.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events and will add anObject to the
	'hidden-expanded' list"

	self selections do: [:each | self collapse: each].!

collapse: anObject
	"collapse the displayed hierarchy below anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events but does add anObject to the
	'hidden-expanded' list"

	| newSelection |

	(self isItemExpanded: anObject) ifFalse: [^ self].

	"we have to change the selection first, or else it'll change implicitly as selected
	children are closed.  That causes unwanted selectionChanged events to be
	generated.
	Note that we *don't* attempt to send a #selectionChanging: notification -- that is
	only sent for user-initiated actions (e.g. see #collapse:because:)"
	newSelection := self selectionAfterCollapseList: (Array with: anObject).
	newSelection isNil ifFalse: [self selections: newSelection].

	"now that we've done that, on to part two!!"
	self basicCollapse: anObject.
!

collapse: anObject because: aSymbol
	"private -- collapse the given Object's node.
	This is (part of) the implementation of double-clicking on the item and of left clicking the open/close box"

	self collapseList: (Array with: anObject) because: aSymbol.!

collapseAll
	"collapse the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events add will add all currently expanded items
	to the hidden-expanded list"

	self selectionRootsOrRoots do: [:each | self collapse: each].
!

collapseList: aCollection because: aSymbol
	"private -- collapse the tree view to ensure that the immediate children the items in aCollection
	are not displayed.
	Since this is user-initiated we need send the vetoable #onSelectionChanging:
	event before making any changes.
	For the same reason, the directly affected nodes are not made hidden-expanded
	since the user has set their preferred status"

	| oldSelections collapse newSelections |

	collapse := aCollection select: [:each | self isItemExpanded: each].
	collapse isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterCollapseList: collapse.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"we have to change the selection first, or else it'll change implicitly as selected
	children are closed.  That causes unwanted selectionChanged events to be
	generated"
	newSelections notNil ifTrue: [self selections: newSelections].

	"this little bit of code is what all the fuss was about..."
	collapse do: [:each | self basicCollapse: each].

	"collapsing the nodes will have made them 'hidden-expanded' so we undo that here"
	collapse do: [:each | self removeHiddenExpanded: each]
!

collapseOrSelectParentBecause: aSymbol
	"private -- if the current selection (if any) is expanded then collapse it, otherwise
	select its parent.
	This is the implementation of the left-arrow-key shortcut"

	| sel |

	sel := self selections at: 1 ifAbsent: [^ self].

	(self isItemExpanded: sel)
		ifTrue: [self collapse: sel because: aSymbol]
		ifFalse: [self selectParentOf: sel because: aSymbol].!

collapseSelectedBecause: aSymbol
	"private -- collapse the tree view to ensure that the immediate children of the current selection,
	if any, are not displayed.
	This is the implementation of the numeric-minus keyboard shortcut"

	self collapseList: self selectionRoots because: aSymbol.!

connectModel
	"connect the receiver to its model, wiring events, etc. Overridden so as also to
	connect to our internal tree model"

	super connectModel.

	self connectTreeModel.
!

connectTreeModel
	"private -- set up the connections to our internal tree model"

	treeModel
		when: #treeChanged: send: #onTreeChanged: to: self;
		when: #item:addedInParent: send: #onItem:addedInParent: to: self;
		when: #item:removedFromParent: send: #onItem:removedFromParent: to: self;
		when: #item:movedToParent: send: #onItem:movedToParent: to: self;
		when: #itemUpdated: send: #onItemUpdated: to: self.

!

countDisplayedChildrenOf: anObject
	"private -- answer how many displayed chilren (recursively) anObject has"

	| answer |

	answer := 0.
	self displayedTreeBelow: anObject do: [:each | answer := answer + 1].

	^ answer.!

defaultIndentSeparation
	"private -- answer the default pixel identation of child items"

	^ 0.!

depthOf: anObject
	"answer our TreeModel's idea of how many generations anObject is descended
	from a root"

	| depth it |

	"why recurse when we can iterate, eh?"
	depth := 0.
	it := anObject.
	[(it := self parentOf: it) notNil] whileTrue: [depth := depth + 1].

	^ depth.!

disableExpandAll
	"answer whether the abiliity to expand the whole tree with one keypress is enabled"

	^ options allMask: DisableExpandAllMask.!

disableExpandAll: aBool
	"set whether the abiliity to expand the whole tree with one keypress is enabled"

	options := options mask: DisableExpandAllMask set: aBool.
!

discardAllExpanded
	"private -- discard any record of which items are expanded or hidden-expanded"

	self
		discardExpanded;
		discardHiddenExpanded.
!

discardExpanded
	"private -- discard any record of which items are expanded"

	expanded := self makeSet.!

discardHiddenExpanded
	"discard any record we may be keeping of which items were
	expanded when they were last displayed.  This only discards
	the current record, it does not stop us from retaining such records
	in the future"

	self retainExpanded ifTrue: [hiddenExpanded := self makeSet].!

disconnectFromModel
	"remove all event registrations lodged with the current model. Overridden so as also to
	disconnect from our tree model"

	super disconnectFromModel.

	treeModel notNil ifTrue: [treeModel removeEventsTriggeredFor: self].
!

displayedContents
	"supplied for consistancy with TreeView"

	^ self allDisplayedItems.!

displayedOrHiddenTreeBelow: aParent do: a1Block
	"private -- traverse the displayed or hidden-expanded part of our tree in pre-order starting at, but
	not including, aParent, evaluating a1Block for each discovered node"

	(self unorderedChildrenOf: aParent) do:
		[:each |
		a1Block value: each.
		((self isItemExpanded: each) or: [self isItemHiddenExpanded: each]) ifTrue:
			[self displayedOrHiddenTreeBelow: each do: a1Block]].!

displayedTreeBelow: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at, but not including, aParent, evaluating
	a1Block for each discovered node"

	(self isItemExpanded: aParent) ifTrue:
		[(self childrenOf: aParent) do: [:each | self displayedTreeOf: each do: a1Block]].!

displayedTreeDo: a1Block
	"traverse the displayed part of our tree in pre-order evaluating
	a1Block for each discovered node"

	self roots do: [:each | self displayedTreeOf: each do: a1Block].!

displayedTreeOf: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at aParent, evaluating
	a1Block for each discovered node"

	a1Block value: aParent.
	self displayedTreeBelow: aParent do: a1Block.!

displayIndexOf: anObject
	"private -- answer the index of anObject in our list display"

	^ self model indexOf: anObject.!

displayIndexOf: anObject ifAbsent: a0Block
	"private -- answer the index of anObject in our list display, or, if it is
	not displayed, the result of evaluating a0Block"

	^ self model indexOf: anObject ifAbsent: a0Block.!

doSortChildrenBlock
	"answer the <monadicValuable> that, if it is not nil, will be evaluted with a parent
	node as its parameter in order to decide whether to use the #sortBlock when sorting
	the children.  This is primarily intended to allow for the case where a tree has 'synthetic'
	nodes that shouldn't be sorted since their layout was set by the UI designer.
	See also #doSortRoots"

	^ doSortChildrenBlock.
!

doSortChildrenBlock: a1Block
	"set the <monadicValuable> that, if it is not nil, will be evaluted with a parent
	node as its parameter in order to decide whether to use the #sortBlock when sorting
	the children.
	See also #doSortRoots:"

	doSortChildrenBlock = a1Block
		ifTrue: [doSortChildrenBlock := a1Block]
		ifFalse: [doSortChildrenBlock := a1Block. self updateDisplayOrder].


!

doSortRoots
	"answer whether our #sortBlock, if any, will be used to sort the roots of the tree.
	See also #doSortChildrenBlock"

	^ options allMask: DoSortRootsMask.!

doSortRoots: aBool
	"set whether our #sortBlock, if any, will be used to sort the roots of the tree.
	See also #doSortChildrenBlock:"

	aBool = self doSortRoots ifTrue: [^ self].

	options := options mask: DoSortRootsMask set: aBool.
	self updateDisplayOrder
!

ensureItemDisplayed: anObject
	"ensure that all parents of anObject are expanded.  Has no effect for root objects,
	or objects that are not children (recursively) of a root.
	Answers whether the object is actually displayed"

	^ (self isItemDisplayed: anObject)
		ifTrue: [true]
		ifFalse: [(self parentOf: anObject)
			ifNil: [false]
			ifNotNil: [:parent | (self ensureItemDisplayed: parent) and: [self expand: parent. true]]].!

expand
	"expand the tree view to ensure that the immediate children of the current
	selection, if any, are displayed.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self selections do: [:each | self expand: each].
!

expand: anObject
	"expand the tree view to ensure that the immediate children of anObject are displayed.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	| children |

	(self isItemExpandable: anObject) ifFalse: [^ self].

	"ensure it is actually displayed, but remove any hidden-expanded record first, or we'll
	end up in an infinite recursion (eek!!)"
	self
		removeHiddenExpanded: anObject;
		ensureItemDisplayed: anObject.

	"note that this may cause an object with no children to show an open flag; that's by design
	so that TreeModels that distinguish between having a zero-sized set of children and not having
	children will display properly"
	self setItem: anObject openState: (self openState).
	self invalidateItem: anObject.

	"add any children"
	children := self childrenOf: anObject.
	self addAll: children afterIndex: (self displayIndexOf: anObject).
	self addExpanded: anObject.

	"apply our record of which children were expanded when this node was last closed"
	hiddenExpanded notNil ifTrue:
		[children do: [:each | (hiddenExpanded includes: each) ifTrue: [self expand: each]]].

	self notifyNodeExpanded: anObject.
!

expand: anObject because: aSymbol
	"private -- expand/collapse the given Object's node.
	This (part of) is the implementation of double-clicking on the item and of left clicking the
	open/close box"

	self expandList: (Array with: anObject) because: aSymbol.
!

expandAll
	"expand the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is provided for consistency with MoenTreeView.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self selectionOrRoots do: [:each | self expandAll: each].!

expandAll: anObject
	"expand the tree view to ensure that all the (recursive) children of anObject are displayed.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	self expand: anObject.
	(self unorderedChildrenOf: anObject) do: [:each | self expandAll: each].
!

expandAllList: aCollection because: aSymbol
	"private -- expand the tree view to ensure that all (recursive) children of the items
	in aCollection are displayed.
	Since this is user-initiated we must send the vetoable #onSelectionChanging:
	event before making any changes"

	| expand newSelections |

	expand := aCollection select: [:each | self isItemExpandable: each].
	expand isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterExpandAllList: expand.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"all OK, do it"
	"Cursor wait showWhile: ["
	expand do: [:each | self expandAll: each].
	"]."

	"ensure that the selection has changed in the way we wanted it to"
	newSelections notNil ifTrue: [self selections: newSelections].
!

expandAllOrCollapseSelectedBecause: aSymbol
	"private -- expand/collapse the entire tree below the current selection, or the entire tree if there is
	no selection.
	This is the implementation of the number-pad multiply keyboard shortcut"

	(self selectionOrRoots anySatisfy: [:each | self isItemExpanded: each])
		ifTrue: [self collapseList: self selectionRootsOrRoots because: aSymbol]
		ifFalse: [self expandAllList: self selectionOrRoots because: aSymbol].!

expandList: aCollection because: aSymbol
	"private -- expand the tree view to ensure that the immediate children of the items
	in aCollection are displayed.
	Since this is user-initiated we must send the vetoable #onSelectionChanging:
	event before making any changes"

	| expand newSelections |

	expand := aCollection select: [:each | self isItemExpandable: each].
	expand isEmpty ifTrue: [^ self].

	"see what the new selection would be and check if the change is acceptable.
	This answers nil if no change is implied"
	newSelections := self selectionAfterExpandList: expand.
	(newSelections notNil and: [(self checkSelectAll: newSelections because: aSymbol) not])
		ifTrue: [^ self].

	"all OK, do it"
	expand do: [:each | self expand: each].

	"ensure that the selection has changed in the way we wanted it to"
	newSelections notNil ifTrue: [self selections: newSelections].!

expandOrCollapse: anObject because: aSymbol
	"private -- expand/collapse the given Object's node.
	This is the implementation of double-clicking on the item and of left clicking
	the open/close box"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject because: aSymbol]
		ifFalse: [self expand: anObject because: aSymbol].
!

expandOrSelectFirstChildBecause: aSymbol
	"private -- if the current selection (if any) is expanded then select its first child, otherwise
	expand it.
	This is the implementation of the right-arrow-key shortcut"

	| sel |

	sel := self selections at: 1 ifAbsent: [^ self].

	(self isItemExpanded: sel)
		ifTrue: [self selectFirstChildOf: sel because: aSymbol]
		ifFalse: [self expand: sel because: aSymbol].!

expandSelectedBecause: aSymbol
	"private -- expand the tree view to ensure that the immediate children of the current
	selection, if any, are displayed.
	This is the implementation of the numeric-plus keyboard shortcut"

	self expandList: self selections because: aSymbol.!

getChildrenBlock
	"answer the <monadicValuable> that we will use to find the children of
	an Object if we have not been configured with an explicit TreeModel"

	^ getChildrenBlock.
!

getChildrenBlock: a1Block
	"set the <monadicValuable> that we will use to find the children of
	an Object if we have not been configured with an explicit TreeModel"

	getChildrenBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel getChildrenBlock: a1Block].!

getParentBlock
	"answer the <monadicValuable> that we will use to determine the parent of
	an Object if we have not been configured with an explicit TreeModel"

	^ getParentBlock.!

getParentBlock: a1Block
	"set the <monadicValuable> that we will use to determine the parent of
	an Object if we have not been configured with an explicit TreeModel"

	getParentBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel getParentBlock: a1Block].
!

hasButtons
	"answer whether the receiver has expand/contract buttons"

	^ options allMask: HasButtonsMask.!

hasButtons: aBool
	"set whether the receiver has expand/contract buttons"

	options := options mask: HasButtonsMask set: aBool.
	self applyImageLists.
!

hasCheckBoxes
	"answer whether the receiver has check-boxes.
	Overridden because the check box stuff uses the same 'state' images as we need
	for our open/closed indicators.  Always answers false"

	^ false.!

hasCheckBoxes: aBool
	"set whether the receiver has check-boxes.
	Overridden because the check box stuff uses the same 'state' images as we need
	for our open/closed indicators.  Always ignored (for consistancy with TreeView)"!

hasChildren: anObject
	"answer our TreeModel's idea of whether anObject has children"

	^ treeModel hasChildren: anObject.!

hasChildrenBlock
	"answer the <monadicValuable> that we will use to determine whether
	an Object has children if we have not been configured with an explicit
	TreeModel"

	^ hasChildrenBlock.!

hasChildrenBlock: a1Block
	"set the <monadicValuable> that we will use to determine whether
	an Object has children if we have not been configured with an explicit
	TreeModel"

	hasChildrenBlock := a1Block.
	hasExplicitTreeModel ifFalse: [treeModel hasChildrenBlock: a1Block].!

hasHotTracking
	"answer whether the receiver has the hot-tracking style"

	"this seems to be the nearest equivalent"
	^ self listViewStyleAllMask: LVS_EX_TWOCLICKACTIVATE.!

hasHotTracking: aBool
	"set whether the receiver has the hot-tracking style"

	"MSDN seems to imply that a TreeView's hot tracking is the same thing as a list view's
	track select.  But they ain't the same beast at all.
	This seems to be the nearest equivalent"
	self listViewStyleMask: LVS_EX_TWOCLICKACTIVATE set: aBool.!

hasIcons
	"supplied for consistancy with TreeView"

	^ self viewMode ~~ #list and: [self getImageBlock notNil or: [self hasColumnImages]].!

hasLines
	"answer whether the receiver has lines connecting its nodes.
	Unfortunately we do not support lines, at least not yet"

	^ false.!

hasLines: aBool
	"set whether the receiver has lines connecting its nodes.
	Unfortunately we do not support lines, so this is ignored
	but retained for compatability with TreeView"
!

hasLinesAtRoot
	"answer whether the receiver has lines and buttons for its root entries.
	Actually this only affects the buttons.  Note also that it will have no
	effect if we do not have buttons at all"

	^ options allMask: HasLinesAtRootMask.!

hasLinesAtRoot: aBoolean
	"set whether the receiver has lines and buttons for its root entries.
	Actually this only affects the buttons.  Note also that it will have no
	effect if we do not have buttons at all"

	options := options mask: HasLinesAtRootMask set: aBoolean.

	#CUtodo.  "should this be #updateAll instead ?"
	self refreshContents.!

indentationFor: anObject
	"private -- answer the number of image widths to indent the row by.
	We use this to hack an approximation to a tree"

	| depth |

	depth := self depthOf: anObject.

	"try to adjust to take #indentSeparation into account"
	depth := depth * (1 + (indentSeparation / self class stateImageWidth) rounded).

	"if we have buttons then Windows'll add indent for the (possibly blank) state icon too,
	so if we don't want #linesAtRoot (really #buttonsAtRoot) then reduce the depth by one
	to hide any buttons in the first column"
	(self hasButtons and: [self hasLinesAtRoot not]) ifTrue: [depth := depth - 1].

	^ depth.!

indentFromRow: anObject
	"private -- answer the number of image widths to indent the row by.
	We use this to hack an approximation to a tree"

	^ self indentationFor: anObject.!

indentSeparation
	"answer the pixel identation of child items from their parents excluding
	the width of the current set of icons being shown"

	^ indentSeparation.!

indentSeparation: indentation
	"set the pixel identation of child items from their parents excluding
	the width of the current set of icons being shown.
	Note that the actual indentation used will be rounded to the nearest multiple
	of the size of the open/close image.  Also the indentation is not applied in
	quite the same way as a real TreeView does it"

	indentSeparation := indentation.
	self invalidate.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.	"will set our model, etc, as side-effect"

	indentSeparation := self defaultIndentSeparation.
	self viewMode == #report ifFalse: [self viewMode: #report].
	self retainExpanded: true.
	options := self class defaultOptions.
!

inOrder: aCollection parent: anObject
	"private -- answer aCollection of children of anObject as ordered by our sort settings"

	"the test against true is in case the doSortChildrenBlock answers nil or self"
	^ (sortBlock isNil or: [doSortChildrenBlock notNil and: [(doSortChildrenBlock value: anObject) ~= true]])
		ifTrue: [aCollection]
		ifFalse: [(aCollection asSortedCollection: sortBlock) asOrderedCollection].!

inRootOrder: aCollection
	"private -- answer aCollection ordered by our sort settings for root objects"

	^ (sortBlock isNil or: [self doSortRoots not])
		ifTrue: [aCollection]
		ifFalse: [(aCollection asSortedCollection: sortBlock) asOrderedCollection].!

invalidateItem: anObject
	"private -- invalidate the rectangle of the item for anObject"

	| index |

	index := self displayIndexOf: anObject ifAbsent: [^ self].
	self invalidateItemIndex: index.!

invalidateItemIndex: anInteger
	"private -- envalidate the item with the 1-based integer index"

	self invalidateRect: (self itemRect: anInteger textOnly: false).
!

isItemDisplayed: anObject
	"answer whether anObject is currently displayed (i.e. either it is a root or
	its parent has been expanded to show its children -- there is no implication
	that it is actually *visible*, at least without scrolling)"

	^ self model includes: anObject.!

isItemExpandable: anObject
	"answer whether it is possble to expand anObject -- i.e. that it has children,
	but hasn't been expanded yet"

	^ (self hasChildren: anObject) and: [(self isItemExpanded: anObject) not].!

isItemExpanded: anObject
	"answer whether anObject is currently expanded (this doens't imply that
	it has any children, just that the #expand: operation has been performed
	on it)"

	^ expanded includes: anObject.!

isItemHiddenExpanded: anObject
	"answer whether anObject is hidden-expanded; that is, that its parent isn't
	displayed, but if/when it is, anObject will be expanded automatically"

	^ self retainExpanded and: [hiddenExpanded includes: anObject].!

isLargeIcons
	"supplied for conistancy with TreeView.  I'm not entirely sure what the implementation
	should be here, since we'd look Very Odd Indeed in any mode than #report.  However..."

	^ self viewMode == #largeIcons.!

isMonoExpandable
	"answer whether the receiver has the single-expand style.
	Supplied for consistancy with TreeView; always answers false"

	^ false.!

isMonoExpandable: aBool
	"set whether the receiver has the single-expand style.
	Supplied for consistancy with TreeView; currently ignored"!

isSelected: anObject
	"supplied for consistancy with TreeView; answer whether anObject is currently
	selected"

	| comparator |

	comparator := self searchPolicy.
	self selections do: [:each | (comparator compare: each with: anObject) ifTrue: [^ true]].
	^ false.
!

isSmallIcons
	"supplied for conistancy with TreeView.  I'm not entirely sure what the implementation
	should be here, since we'd look Very Odd Indeed in any mode than #report, but that's
	probably the bset we can do"

	^ self viewMode == #report.
!

isVirtual: aBoolean
	"overridden to request the state update requests"

	| mask |

	mask := self lvmGetCallbackMask.
	mask := mask mask: LVIS_STATEIMAGEMASK set: aBoolean.
	self lvmSetCallbackMask: mask.

	^ super isVirtual: aBoolean.!

listModel: aListModel
	"private -- set the ListModel for us to use.  This is invoked indirectly from #model: if the supplied
	model is a ListModel.  We generate a VirtualTreeModel and initialise its pluggable blocks
	from our *Block aspects to use as our TreeModel, and create an internal ListModel to use
	as our #model (the contents of which will be changed as the display changes).
	Actually we use the trivial subclass, VirtualTreeModelWithSearchPolicy, since that allows us to
	preserver the search policy of the list model too.
	Note that once we have initialised ourself from the supplied list model, we take no further interest
	in it"

	| roots newTreeModel newListModel |

	roots := OrderedCollection withAll: aListModel.
	newTreeModel := self makeInternalTreeModel: roots searchPolicy: aListModel searchPolicy.
	newListModel := self makeInternalListModel: roots searchPolicy: aListModel searchPolicy.

	hasExplicitTreeModel := false.
	self treeModel: newTreeModel listModel: newListModel.!

makeInternalListModel: aCollection searchPolicy: aSearchPolicy
	"private -- answer a new ListModel configured with the given initial contents and search policy.
	This will be used  to hold the actual displayed items, and will be passed to 'super' as its model"

	^ self class internalListModelClass
				on: (OrderedCollection withAll: aCollection)
				searchPolicy: aSearchPolicy.
!

makeInternalTreeModel: aCollection searchPolicy: aSearchPolicy
	"private -- answer a new TreeModel configured the given roots, aCollection,
	search policy, and our various blocks"

	| newTreeModel |

	newTreeModel := self class internalTreeModelClass
					withRoots: aCollection
					searchPolicy: aSearchPolicy.

	getParentBlock notNil ifTrue: [newTreeModel getParentBlock: getParentBlock].
	getChildrenBlock notNil ifTrue: [newTreeModel getChildrenBlock: getChildrenBlock].
	hasChildrenBlock notNil ifTrue: [newTreeModel hasChildrenBlock: hasChildrenBlock].

	^ newTreeModel.!

makeSet
	"private -- answer a new Set using our current search policy"

	^ self class setClass searchPolicy: self searchPolicy.
!

makeSetFrom: aCollection
	"private -- answer a new Set which uses our current search policy and contains the
	elements of aCollection"

	^ (self makeSet)
		addAll: aCollection;
		yourself.!

minimumIndent
	"just for consistancy with TreeView"

	^ 0.!

notifyNodeAdded: anObject
	"private -- notify 'our' Observers that a node corresponding to anObject
	has been added.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeAdded: with: anObject.
!

notifyNodeCollapsed: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been collapsed.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeCollapsed: with: anObject.
!

notifyNodeExpanded: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been expanded.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeExpanded: with: anObject.
!

notifyNodeRemoved: anObject
	"private -- notify 'our' Observers that the node corresponding to anObject
	has been removed.  Note that we actually trigger off our presenter (like
	most events triggered by Views)"

	self presenter trigger: #nodeRemoved: with: anObject.
!

onDestroyed
	"called when the View is destroyed (whatever that actually means).  Overridden
	to unlink ourselves from our real tree model's change notifications too"

	self isStateRestoring ifFalse:		"only when we are not in #recreate"
		[treeModel notNil ifTrue: [treeModel removeEventsTriggeredFor: self].
		self discardAllExpanded].

	^ super onDestroyed.!

onDisplayDetailsRequired: lvitem
	"private -- Windows wants the display details for the item identified by lvitem.
	Overriden to handle LVIF_STATE too"

	| object mask |

	object := self objectFromHandle: lvitem handle ifAbsent: [^ nil].
	mask := lvitem mask.

	(self systemDrawsStateImages
		and: [lvitem iSubItem isZero
			and: [lvitem mask allMask: LVIF_STATE]])
		ifTrue: [lvitem stateImageIndex: (self stateIndexFromRow: object)].

	^ super onDisplayDetailsRequired: lvitem.!

onFullyCreated
	"our window has been created. Ensure that it reflects the state we think
	it should have"

	super onFullyCreated.

	self isVirtual ifTrue: [self lvmSetCallbackMask: LVIS_STATEIMAGEMASK].!

onItem: aChildObject addedInParent: aParentObject
	"notification received when aChildObject has been added to the list of
	children of aParentObject.  We assume that aChildObject is not already
	present in the tree as a child of some other object"

	"if the child is currently displayed then ignore it"
	(self isItemDisplayed: aChildObject) ifTrue: [^ self].

	"if the parent, if any, is not currently expanded then ignore it except for (possibly) updating
	the state image, otherwise insert the new child amongst its brothers and sisters"
	(aParentObject notNil and: [(self isItemExpanded: aParentObject) not])
		ifTrue: [self setItemState: aParentObject]
		ifFalse: [self additem: aChildObject toParent: aParentObject].
!

onItem: aChildObject movedToParent: aParentObject
	"notification received when aChildObject has been moved to the list of 	children of aParentObject.
	NB: unlike the TreeView we preserve 'expandedness' of the moved child, rather than just preserving
	the selection and closing all other subtrees"

	| selections |

	"this will prevent selection change events reaching the Presenter."
#CUtodo.  "unfortunately it also prevents #nodeExpanded: and nodeAdded: notifications too -- fix this!!"
	selections := self selections.
	self presenter noEventsDo:
		[self
			basicOnItem: aChildObject movedToParent: aParentObject;
			selections: selections ifAbsent: [];
			updateExpanded;
			updateStates;	
			invalidate].
!

onItem: aChildObject removedFromParent: aParentObject
	"notification received when aChildObject has been removed from the list of
	children of aParentObject."

	(self isItemDisplayed: aChildObject) ifTrue: [self removeZombies].
	aParentObject isNil ifFalse: [self setItemState: aParentObject].
!

onItemUpdated: anObject
	"notification received when one of our elements has been changed"

	self onItemUpdatedAtIndex: (self displayIndexOf: anObject ifAbsent: [^ self]).
	self setItemState: anObject.
!

onKeyPressed: aKeyEvent
	"handler for key press events.
	We want to emulate:
		- the numeric pad's multiply key fully expands/collapses the current selection or the whole tree
		- left-arrow expands children or, if already expanded, selects first
		- right-arrow collapses children, or if not expanded, selects parent
		- numeric+ expands selection
		- numeric- collapses selection"

	| code |

	"God's Teeth, but this is gross!!"

	"early-out with default ListView handling if we're not interested"
	code := aKeyEvent code.
	(code == VK_RIGHT
		or: [code == VK_LEFT
			or: [(code == VK_MULTIPLY and: [self disableExpandAll not])
				or: [code == VK_ADD
					or: [code == VK_SUBTRACT]]]])
		ifFalse: [^ super onKeyPressed: aKeyEvent].

	"I *hate* this selection-changing-cause stuff!!"
	(code == VK_RIGHT) ifTrue: [self expandOrSelectFirstChildBecause: #keyboard].
	(code == VK_LEFT) ifTrue: [self collapseOrSelectParentBecause: #keyboard].
	(code == VK_MULTIPLY and: [self disableExpandAll not]) ifTrue: [self expandAllOrCollapseSelectedBecause: #keyboard].
	(code == VK_ADD) ifTrue: [self expandSelectedBecause: #keyboard].
	(code == VK_SUBTRACT) ifTrue: [self collapseSelectedBecause: #keyboard].

	"remember to do this since we are supressing the superclass implementation"
	self presenter trigger: #keyPressed: with: aKeyEvent.

	"answer 0 to suppress Windows' default handling"
	^ 0.!

onLeftButtonDoubleClicked: aMouseEvent
	"handle a mouse left-button event.  We need to implement opening
	items by double-clicking"

	| hit target |

	#CUtodo.	"what happens if mouse buttons are reversed ?"

	"if it's not over an item then we can just leave it to the superclass"
	hit := self basicItemFromPoint: aMouseEvent position.
	((target := self model at: (hit iItem + 1) ifAbsent: [nil]) notNil)
		ifFalse: [^ super onLeftButtonDoubleClicked: aMouseEvent].

	"toggle the indicated item's expanded state. This does not directly change the
	selection but it may do so implicitly if the effect is to close an ancestor of
	the current selection.  Hence we need to bugger about with the selection cause
	stuff"
	self expandOrCollapse: target because: #mouse.

	"in this case we want to pass the event to the default handling so that any selection
	changes and the default 'action' processing will also be applied"
	^ super onLeftButtonDoubleClicked: aMouseEvent.!

onLeftButtonPressed: aMouseEvent
	"handle a mouse left-button event.  We need to implement opening
	items by clicking their status box"

	| hit target |

	#CUtodo.	"what happens if mouse buttons are reversed ?"

	"see if it's a click on a state icon; if not then let the default list view processing
	take over"
	hit := self basicItemFromPoint: aMouseEvent position.
	(hit isOnItemStateIcon and: [(target := self model at: (hit iItem + 1) ifAbsent: [nil]) notNil])
		ifFalse: [^ super onLeftButtonPressed: aMouseEvent].

	"toggle the indicated item's expanded state. This does not directly change the
	selection, unless shift is down, but it may do so implicitly if the effect is to close an
	ancestor of the current selection.  Hence we need to bugger about with the
	selection cause stuff"
	self expandOrCollapse: target because: #mouse.

	"remember to do this since we are avoiding the superclass implementation"
	self presenter trigger: #leftButtonPressed: with: aMouseEvent.

	"answer 0 to suppress Window's default handling"
	^ 0.!

onTreeChanged: anObject
	"notification received when the tree at and below anObject has been changed.  If
	anObject is nil then it amounts to a notification that *something* (unknown, or
	perhaps the set of roots) has changed.
	Note this is similar in meaning to #refreshFromModelBelow: -- the difference is that
	this (unless #useSmartRefresh is on, in which case it just forwards to
	#refreshFromModelBelow:) collapses the tree below anObject (like the standard
	Dolphin TreeView).  NB: because we need to #removeZombies, this collapsing
	implementation is not significantly cheaper than #refreshFromModelBelow: since
	it still requires a scan of the whole displayed part of the tree"

	self useSmartRefresh ifTrue: [^ self refreshFromModelBelow: anObject].

	"if any part of the tree has changed, then there may be nodes (now removed) that are
	still on our hidden-expanded list; there is no way that we can check for that case
	so just we discard the whole hidden-expanded list"
	self discardHiddenExpanded.

	"easy implementation if the whole tree is potentially invalid"
	anObject isNil ifTrue: [^ self resetTreeToRoots].

	"if the object is not currently displayed, then we've done everything 	we have to"
	(self isItemDisplayed: anObject) ifFalse: [^ self].

	"we collapse the tree at the indicated point"
	self collapse: anObject.

	"but then we *still* have to rescan the displayed tree since anObject may
	have had children which have just vanished (without anyone bothering to
	tell us about them specifically)"
	self removeZombies.

	"finally..."
	self onItemUpdated: anObject.!

openState
	"private -- answer the Symbol name of the 'open' state"

	^ self hasIcons ifTrue: [#Open] ifFalse: [#SmallOpen].
!

parentOf: anObject
	"answer our TreeModel's idea of anObject's parent"

	^ treeModel parentOf: anObject.!

parentsOf: anObject includesAny: aSet
	"private -- answer whether any of the (recursive) parents of anObject are in
	aSet"

	| parent |

	parent := self parentOf: anObject.
	parent isNil ifTrue: [^ false].
	(aSet includes: parent) ifTrue: [^ true].
	^ self parentsOf: parent includesAny: aSet.!

previousDisplayedSiblingOf: anObject in: aCollection
	"private -- given a collection of siblings of anObject (not all of which are necessarily
	displayed yet), answer the last sibling in the collection that is displayed, and which comes
	before anObject.  May be nil if there is no such sibling"

	| sibling comparator |

	sibling := nil.
	comparator := self searchPolicy.
	aCollection do:
		[:each |
		(comparator compare: each with: anObject) ifTrue: [^ sibling].
		(self isItemDisplayed: each) ifTrue: [sibling := each]].

	^ sibling.!

refreshContents
	"our display needs to be refreshed.  Overridden to force state images
	back to Windows, and to throw away the (expensive) record of what
	used to be expanded but is no longer displayed"

	super refreshContents.
	self
		updateStates;
		discardHiddenExpanded.!

refreshFromModel
	"can be called when it is suspected that the current display differs from what the tree model
	would expect.  Normally this only occurs if the tree model is actually virtual (so that changes
	can happen that we haven't been told about in detail).
	NB: I don't know to what extent this is the *intended* semantics of #refreshContents,
	since the Dolphin implementations don't work as well as I would wish with virtual tree
	models that change arbitrarily, so I don't have anything to compare against"

	self refreshFromModelBelow: nil.!

refreshFromModelBelow: anObject
	"can be called when it is suspected that the current display differs from what the tree model
	would expect.  Normally this only occurs if the tree model is actually virtual (so that changes
	can happen that we haven't been told about in detail).  If anObject is nil then refreshes the
	whole tree"

	"NB1: the current implementation does not make full use of anObject, but will
	usually re-scan the whole tree.

	NB2: I really wish I'd decided to use an additional helper tree model to keep track of
	what the *display* thinks is included and what the *displayed* parent-child relationships
	are"

	| oldSels newOrder legit newSels comparator |

	"there may be nodes (now removed) that are still on our hidden-expanded list; there is no
	way that we can check for that case so just we discard the whole hidden-expanded list"
	self discardHiddenExpanded.

	"if the object is not nil, and is not currently displayed, then there's no need to do more"
	(anObject notNil and: [(self isItemDisplayed: anObject) not])
		ifTrue: [^ self].

	"we'll need to allow for selection changes"
	oldSels := self selections.

	"work out what *should* be displayed, and in what order.
	This scan is based on the *tree model's* data, so it will only find the items
	that are legitimately displayed"
	newOrder := OrderedCollection new.
	self displayedTreeDo: [:each | newOrder addLast: each].
	legit := self makeSetFrom: newOrder.

	"remove items that are not legitimately included"
	self removeAllExcept: legit.

	"and filter the selection list too"
	newSels := oldSels select: [:each | legit includes: each].

	"now we have to add any 'new' items.  We get lazy, just add them at the
	end then update the display order"
	self model do: [:each | legit remove: each ifAbsent: []].
	legit do: [:each | self addItem: each afterIndex: self model size].
	self updateDisplayOrderFrom: self allDisplayedItems to: newOrder.

	"and ensure the state images are correct too"
	self updateStates.

	"we may as well make sure..."
	anObject isNil ifFalse: [self onItemUpdated: anObject].

	"and finally, put the selection back to what it should be.
	We have to be carefull here, if the selection has genuinely changed
	then we have to send a selection changed notification, but if the
	selection has not changed, then we *still* have to set the selection
	(since the Window's selection won't have been updated to reflect
	any changes to the order of items) but we have to ensure that the
	*don't* send spurious selection changing events in that case.
	Sigh..."
	newSels size = oldSels size
		ifTrue: [self presenter noEventsDo: [self selections: newSels]]
		ifFalse: [self selections: newSels].!

refreshNonVirtual
	"private -- overridden to refresh the 'state' too"

	super refreshNonVirtual.

	"NB: this should be unecessary, but the superclasses handling of raw element insertion is too
	much of a minefield for me to mess with it.  See ListView>>addNonVirtualItems:atIndex: for
	why I'd rather not try to override it to set the state directly"
	self unorderedRoots do: [:each | self setItemState: each].

!

removeAllExcept: aSet
	"private -- remove all the items that are not in aSet. The set is assumed to have the
	correct search policy, and to contain only legit items -- i.e. ones that are both
	displayed *and* still included in our tree model"

	self model size to: 1 by: -1 do:
		[:i || each |
		each := self model at: i.
		(aSet includes: each) ifFalse:
			[self model removeAtIndex: i.
			self
				removeExpanded: each;
				removeHiddenExpanded: each;
				notifyNodeRemoved: each]].
!

removeExpanded: anObject
	"private -- ensure that anObject is not on our list of expanded
	nodes"

	expanded remove: anObject ifAbsent: [].!

removeFromDisplay: anObject
	"private -- ensure that anObject is not displayed"

	self model removeAtIndex: (self displayIndexOf: anObject ifAbsent: [^ self]).

	self notifyNodeRemoved: anObject.

!

removeHiddenExpanded: anObject
	"private -- ensure that anObject is not on our hidden expanded list (if we have one). 
	This is called in three circumstances:
	-	when anObject has been expanded (since it is no longer *hidden*)
	-	when the user has explicitly expanded *or* collapsed a node
	-	when we find that anObject is no longer part of the tree"

	self retainExpanded ifTrue: [hiddenExpanded remove: anObject ifAbsent: []].!

removeZombies
	"private -- when an object is removed, we need to remove any displayed children too;
	but this is a problem: our tree model is no longer willing to admit that the removed object
	*has* any children, or indeed that it exists at all.  This is a fundamental architectural
	problem with MVC/MVP-type frameworks where the View needs to respond to changes
	*after* they have happened, and so the data needed to 'unwind' the display has already
	been trashed.  All we can do is scan our displayed items removing any that the model is
	no longer willing to countenance"

	| legit |

	"this scan is based on the *tree model's* data, so it will only find the items
	that are legitimately displayed"
	legit := self makeSet.
	self unorderedTreeDo: [:each | legit add: each].

	"remove any that are not in that set"
	self removeAllExcept: legit.

	#CUtodo.  "should we do an #updateStates here ?"!

resetTreeToRoots
	"private -- go back to our TreeModel and ensure that our tree displays
	all and only its current roots"

	self discardAllExpanded.	
	self model list: (OrderedCollection withAll: self roots).	"be sure to make a copy"!

resolutionScaledBy: scale
	"private -- I'm not sure what this does, but it's copied from TreeView"

	indentSeparation := (indentSeparation * scale x) truncated.
	super resolutionScaledBy: scale


!

retainExpanded
	"answer whether we retain a record of which items were expanded even
	when they are not displayed, this is the default (even though it is expensive
	and irritating) 'cos that's the way that TreeView does it"

	^ hiddenExpanded notNil.
!

retainExpanded: aBool
	"set whether we retain a record of which items were expanded even
	when they are not displayed, this is the default"

	aBool = self retainExpanded ifFalse:
		[hiddenExpanded := aBool ifTrue: [self makeSet] ifFalse: [nil]].!

roots
	"answer our TreeModel's idea of its roots in the order specified by our
	sortblock, etc"

	^ self inRootOrder: self unorderedRoots.!

searchPolicy
	"answer the SearchPolicy that is in use.
	This is implicit in our model, hence there is no corresponding
	setter"

	"we could use:
		^ self model searchPolicy.
	which would give the same answer"
	^ treeModel searchPolicy.!

selectFirstChildOf: anObject because: aSymbol
	"private -- see if it's acceptable to select anObject's first child (it it has one), and if so then
	do it.
	This is part of the implementation of the right-arrow-key shortcut"

	| children child all |

	children := self childrenOf: anObject.
	child := children at: 1 ifAbsent: [^ self].
	all := Array with: child.
	(self checkSelectAll: all because: aSymbol) ifTrue: [self selections: all].!

selectionAfterCollapseList: aCollection
	"private -- answer what the selection list *would* be if we were to collapse each
	item in aCollection.
	If no change is implied then answer nil"

	| changes newSelections |

	changes := false.
	newSelections := self selectionsAsSet.
	aCollection do:
		[:each |
		self unorderedTreeBelow: each do:
			[:child | (newSelections includes: child) ifTrue:
					[newSelections remove: child.
					newSelections add: each.
					changes := true]]].
	^ changes
		ifTrue: [newSelections asOrderedCollection]
		ifFalse: [nil].
!

selectionAfterExpandAllList: aCollection
	"private -- answer what the selection list *would* be if we were to 'expand all'
	each item in aCollection.
	If no change is implied then answer nil.
	NB: we assume that each item in aCollection is expandable"

	| expand newSelection |

	"there'll be no change unless we want autoselection (the user holding down the
	shift key and we support multiple selection)"
	self wantExtendedSelection ifFalse: [^ nil].

	"the only items that can have any effect are the ones that are selected"
	newSelection := self selectionsAsSet.
	expand := aCollection select: [:each | newSelection includes: each].
	expand isEmpty ifTrue: [^ nil].

	"but all their (recursive) children will get selected"
	newSelection := self selection asOrderedCollection.
	expand do: [:each | newSelection addAll: (treeModel withAllChildren: each)].

	^ newSelection.!

selectionAfterExpandList: aCollection
	"private -- answer what the selection list *would* be if we were to expand each
	item in aCollection.
	If no change is implied then answer nil.
	NB: we assume that each item in aCollection is expandable"

	| expand newSelection |

	"there'll be no change unless we want autoselection (the user holding down the
	shift key and we support multiple selection)"
	self wantExtendedSelection ifFalse: [^ nil].

	"the only items that can have any effect are the ones that are selected"
	newSelection := self selectionsAsSet.
	expand := aCollection select: [:each | newSelection includes: each].
	expand isEmpty ifTrue: [^ nil].

	"but all their children will get selected, and *their* children too if they are hidden-expanded"
	newSelection := self selection asOrderedCollection.
	expand do: [:each | self displayedOrHiddenTreeBelow: each do: [:child | newSelection addLast: child]].

	^ newSelection.!

selectionOrRoots
	"private -- answer the current selection or, if there is no selection then
	our current roots"

	| list |

	list := self selections.
	^ list isEmpty
		ifTrue: [self unorderedRoots]
		ifFalse: [list].!

selectionRoots
	"private -- answer a collection of the current selection with all items removed
	that are children of other items in the collection"

	| list set |

	list := self selections.
	list size <= 1ifTrue: [^ list].

	set := self makeSetFrom: list.
	^ list reject: [:each | self parentsOf: each includesAny: set].
	!

selectionRootsOrRoots
	"private -- answer the current selection (with all items removed that are
	children of other items in the collection) or, if there is no selection then
	our current roots"

	| list |

	list := self selectionRoots.
	^ list isEmpty
		ifTrue: [self unorderedRoots]
		ifFalse: [list].!

selections: aCollection ifAbsent: a0Block
	"set our selection to the elements of aCollection, if any.  If any of the elements of
	the collection do not exist in our tree, then answer the result of evaluating the <nildadicValuable>, a0Block.
	NB: unlike all the other instance-side methods, this is not a duplicate of one in ListTreeView"

	"ensure that the proposed new selection is actually displayed before we
	allow the superclass definition to take over"
	#CUtodo.  "if the TreeModel throws an exception from #ensureItemDisplayed: then we
			should evaluate a0Block, not allow the error to propogate outwards"
	aCollection do: [:each | self ensureItemDisplayed: each].
	^ super selections: aCollection ifAbsent: a0Block.
!

selectionsAsSet
	"answer our current selection as a Set that uses our current
	seach policy"

	^ self makeSetFrom: self selections.!

selectParentOf: anObject because: aSymbol
	"private -- see if it's acceptable to select anObject's parent (it it has one), and if so then
	do it.
	This is part of the implementation of the left-arrow-key shortcut"

	| parent all |

	parent := (self parentOf: anObject) ifNil: [^ self].
	all := Array with: parent.
	(self checkSelectAll: all because: aSymbol) ifTrue: [self selections: all].!

setItem: anObject indentation: anInteger
	"private -- update the indentation for anObject.
	Note: this causes a Windows error if our control is virtual"

	| objectIndex anLvItem |

	objectIndex := self displayIndexOf: anObject ifAbsent: [^ self].

	anLvItem := (LVITEM new)
			iItem: objectIndex - 1;
			indent: anInteger;
			yourself.

	self lvmSetItem: anLvItem.

	self invalidateItemIndex: objectIndex .
!

setItem: anObject openState: aSymbol
	"private -- update the 'state' image for anObject"

	| objectIndex stateIndex anLvItem |

	self systemDrawsStateImages ifFalse: [^ self].

	objectIndex := self displayIndexOf: anObject ifAbsent: [^ self].
	stateIndex := self stateImageIndex: aSymbol.

	anLvItem := (LVITEM new)
			iItem: objectIndex - 1;
			stateImageIndex: stateIndex;		"a 1-based index in Windows !!"
			yourself.

	self lvmSetItem: objectIndex - 1 state: anLvItem.

	self invalidateItemIndex: objectIndex .


!

setItemIndentation: anObject
	"private -- update the indent for anObject.
	Note: this causes a Windows error if our control is virtual"

	self
		setItem: anObject
		indentation: (self indentationFor: anObject).!

setItemState: anObject
	"private -- update the 'state' image for anObject"

	self
		setItem: anObject
		openState: (self stateFor: anObject).!

setModel: aListOrTreeModel
	"private -- called to set our model.  Since we play games with the model (the one we hold may
	not be the one supplied), we override to dispatch to the 'real' model setters"

	"this will actually (probably) end up calling super>>setModel: with a different model" 
	^ (aListOrTreeModel respondsTo: #roots)
		ifTrue: [self treeModel: aListOrTreeModel]
		ifFalse: [self listModel: aListOrTreeModel].!

sortBlock
	"answer the <diadicValuable> that we use to sort the roots, and the children of
	any parent.  May be nil"

	^ sortBlock.!

sortBlock: a2Block
	"set the <diadicValuable> that we will use to sort the roots, and the children of
	any parent.  May be nil, in which case we use the natural sort order as provided
	by our TreeModel"

	sortBlock = a2Block
		ifTrue: [sortBlock := a2Block]
		ifFalse: [sortBlock := a2Block. self updateDisplayOrder].
!

sortOnColumn: aListViewColumn
	"sort the receiver according to the sort block in aListViewColumn"

	| sorter |

	aListViewColumn isSortable ifFalse: [^ self].

	sorter := self presenter.

	"ugly hack: don't go back to vanilla presenters -- they don't understand how to sort trees"
	#CUtodo.  "make this more flexible"
	(sorter == self or: [sorter isKindOf: ListTreePresenter])
		ifFalse: [sorter := self].

	Cursor wait showWhile: [sorter beSorted: aListViewColumn rowSortBlock].!

stateFor: anObject
	"private -- answer the 'state' identifier for anObject"

	^ (self isItemExpanded: anObject)
			ifTrue: [self openState]
			ifFalse: [(self hasChildren: anObject)
				ifTrue: [self closedState]
				ifFalse: [#Leaf]].!

stateImageIndex: aSymbol
	"private -- answer the state image index corresponding to aSymbol"

	^ self class stateNames indexOf: aSymbol ifAbsent: [0].

!

stateIndexFromRow: anObject
	"private -- answer the index of the state image that is appropriate for anObject"

	^ self stateImageIndex: (self stateFor: anObject).!

stbSaveOn: anSTBOutFiler
	"save a binary representation of the receiver to anSTBOutFiler.
	Overriden not to save the contents of the hiddenExpanded collection too"

	| saved |

	saved := hiddenExpanded.
	self discardHiddenExpanded.	"don't set it to nil or the resurected version will never use hiddenExpanded"

	^ [super stbSaveOn: anSTBOutFiler]
		ensure: [hiddenExpanded := saved].!

systemDrawsStateImages
	"private -- answer whether we can let Windows draw our state image (normally we can)"

	"in point of fact, the only case where Window's doesn't get it right is
	when we are a multi-line EditableListTreeView"
	^ true.!

toggleExpandAll: anObject
	"expand/collapse the entire tree below anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject]
		ifFalse: [self expandAll: anObject].!

toggleExpanded: anObject
	"expand/collapse the tree view under anObject.
	Note that this is considered to be program-initiated and so does not generate
	vetoable #selectionChanging: events"

	(self isItemExpanded: anObject)
		ifTrue: [self collapse: anObject]
		ifFalse: [self expand: anObject].!

treeModel
	"answer our TreeModel.  If the receiver's model as originally supplied was a ListModel,
	then the TreeModel will be an internally generated VirtualTreeModel using our pluggable
	blocks from the #hasChildrenBlock: #childrenOfBlock: and #parentOfBlock: aspects"

	^ treeModel.!

treeModel: aTreeModel
	"private -- set the TreeModel for us to use.  This is invoked indirectly from #model: if the supplied
	model is a TreeModel.  We create an internal ListModel to use as our #model that will
	be changed as the display changes"

	| newListModel |

	"make an internal ListModel to hold the actual display, initialising it with the roots
	and search policy of the tree model, this will be passed to our superclass as
	our 'real' model"
	newListModel := self
				makeInternalListModel: aTreeModel roots
				searchPolicy: aTreeModel searchPolicy.

	hasExplicitTreeModel := true.
	self treeModel: aTreeModel listModel: newListModel.!

treeModel: aTreeModel listModel: aListModel
	"private -- set the Models for us to use.  The TreeModel is only used internally, the ListModel (which
	*must not* be the same as the ListModel of our Presenter if that happens to use one) is passed
	to our inherited implementation"

	treeModel := aTreeModel.
	super setModel: aListModel.
	self discardAllExpanded.
!

unorderedChildrenOf: anObject
	"answer our TreeModel's idea of anObject's immediate children without applying
	our sortblock"

	^ treeModel childrenOf: anObject!

unorderedRoots
	"answer our TreeModel's idea of its roots without applying our sortblock"

	^ treeModel roots.!

unorderedTreeBelow: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at, but not including, aParent,
	evaluating a1Block for each discovered node.  This is a faster version of
	#displayedTreeBelow:do that ignores the ordering implied by our sortblocks"

	(self isItemExpanded: aParent) ifTrue:
		[(self unorderedChildrenOf: aParent) do: [:each | self unorderedTreeOf: each do: a1Block]].!

unorderedTreeDo: a1Block
	"traverse the displayed part of our tree in pre-order evaluating
	a1Block for each discovered node.  This is a faster version of
	#displayedTreeDo: that ignores the ordering implied by our
	sortblocks"

	self unorderedRoots do: [:each | self unorderedTreeOf: each do: a1Block].!

unorderedTreeOf: aParent do: a1Block
	"traverse the displayed part of our tree in pre-order starting at aParent, evaluating
	a1Block for each discovered node.  This is a faster version of
	#displayedTreeOf:do: that ignores the ordering implied by our
	sortblocks"

	a1Block value: aParent.
	self unorderedTreeBelow: aParent do: a1Block.!

updateDisplayOrder
	"private -- ensure that the 'real' list model is showing items in the same order as we expect"

	| oldOrder newOrder comparator firstDiff lastDiff |

	"work out what order we are in"
	oldOrder := self allDisplayedItems.

	"work out what order we *should* be in"
	newOrder := OrderedCollection new: self model size.
	self displayedTreeDo: [:each | newOrder addLast: each].

	"they *should* be the same size, but in case not...
	NB: apart from this one bit of defensive coding we assume that the two versions are
	re-orderings of the *same* elements"
	(newOrder size = oldOrder size) ifFalse: [^ self refreshFromModel].

	self updateDisplayOrderFrom: oldOrder to: newOrder.
!

updateDisplayOrderFrom: anOldOrder to: aNewOrder
	"private -- change the currently displayed order of items from that given
	by the <sequenceableCollection> anOldOrder to that given by the
	<sequenceableCollection> aNewOrder.
	NB: assumes that anOldOrder is a correct reflection of the current
	status, and that the two collections contain the same items and
	are the same size"

	| comparator firstDiff lastDiff |

	"compare"
	comparator := self searchPolicy.
	firstDiff := (1 to: aNewOrder size)
			detect: [:i | (comparator compare: (anOldOrder at: i)
			with: (aNewOrder at: i)) not] ifNone: [^ self].
	lastDiff := (aNewOrder size to: 1 by: -1)
			detect: [:i | (comparator compare: (anOldOrder at: i)
			with: (aNewOrder at: i)) not].

	"and fixup"
	self model
		replaceFrom: firstDiff
		to: lastDiff
		with: aNewOrder
		startingAt: firstDiff.
!

updateExpanded
	"private -- ensure that our idea of what is 'expanded' is still correct; this called when we suspect
	that an item that used to have children no longer does.  NB currently there is no need for checking
	for items that have *gained* children"

	expanded removeAll: (expanded reject: [:each | treeModel hasChildren: each]).
!

updateMode
	"supplied for consistancy with TreeView>>updateMode.  Actually we always
	reply #dynamic (mainly because Windows keeps asking for the indentation even
	after it has been told it; otherwise we would be #dynamic or #static according
	as we are #isVirtual)"

	^ #dynamic.!

updateMode: aSymbol
	"supplied for consistancy with TreeView>>viewMode.  Actually we ignore this"!

updateStates
	"private -- update the 'state' images all objects"

	self model do: [:each | self setItemState: each].!

useSmartRefresh
	"set whether we attempt to preserve open subbranches of the tree when we
	are told that the model has changed below some node"

	^ options allMask: UseSmartRefreshMask.
!

useSmartRefresh: aBool
	"set whether we attempt to preserve open subbranches of the tree when we
	are told that the model has changed below some node"

	options := options mask: UseSmartRefreshMask set: aBool.
!

veryBasicCollapse: anObject
	"private -- recursively collapse all expanded children of anObject, and
	remove them from the display.
	NB: assumes none of the children of anObject are selected.
	NB: assumes anObject is expanded"

	"kill off any children (he he).
	NB: do it in reverse of display order to reduce the amount of data that
	is refreshed for each deletion (it'd be nice if ListModel had optimised forms
	of bulk removal)"
	(self childrenOf: anObject) reverseDo:
		[:each |
		(self isItemExpanded: each) ifTrue:
			[self veryBasicCollapse: each].
		self removeFromDisplay: each].

	"remember that anObject was expanded, but isn't anymore"
	self
		addHiddenExpanded: anObject;
		removeExpanded: anObject.

	self notifyNodeCollapsed: anObject.!

viewMode: aSymbol
	"sets the view mode of the receiver to aSymbol.
	Overridden to understand the TreeView modes #noIcons and #smallIcons"

	(aSymbol == #noIcons or: [aSymbol == #smallIcons])
		ifFalse: [^ super viewMode: aSymbol].

	"this isn't quite right, since we really don't take proper account of the various image blocks,
	but it'll do for its main purpose which is to make mutating TreeViews (in the View Composer)
	work better"
	(aSymbol == #noIcons) ifTrue:
		[self getImageBlock: nil.
		self hasColumnImages: false].
	^ super viewMode: #report.
!

wantExtendedSelection
	"private -- answer whether we should interpret the current user gesture as a request to
	extend the selection"

	"this is silly"
	^ (self isKindOf: MultipleSelectionListView) and: [Keyboard default isShiftDown].! !
!MutipleSelectionListTreeView categoriesFor: #addAll:afterIndex:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #addExpanded:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #addHiddenExpanded:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #addItem:afterIndex:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #additem:toParent:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #additem:toParent:withSiblings:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #allDisplayedItems!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #allExpandedItems!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #allHiddenExpandedItems!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #applyImageLists!do copy!image management!private! !
!MutipleSelectionListTreeView categoriesFor: #basicCollapse:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #basicOnItem:movedToParent:!do copy!event handling!private! !
!MutipleSelectionListTreeView categoriesFor: #beNoIcons!do copy!modes!public! !
!MutipleSelectionListTreeView categoriesFor: #beSorted:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #checkChangeSelectionFrom:to:because:!do copy!events!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #checkSelectAll:because:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #childrenOf:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #closedState!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #collapse!commands!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #collapse:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #collapse:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #collapseAll!commands!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #collapseList:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #collapseOrSelectParentBecause:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #collapseSelectedBecause:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #connectModel!do not copy!initializing!models!public! !
!MutipleSelectionListTreeView categoriesFor: #connectTreeModel!do copy!initializing!models!private! !
!MutipleSelectionListTreeView categoriesFor: #countDisplayedChildrenOf:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #defaultIndentSeparation!constants!do copy!private! !
!MutipleSelectionListTreeView categoriesFor: #depthOf:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #disableExpandAll!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #disableExpandAll:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #discardAllExpanded!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #discardExpanded!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #discardHiddenExpanded!do copy!helpers!operations!public! !
!MutipleSelectionListTreeView categoriesFor: #disconnectFromModel!do copy!initializing!models!public! !
!MutipleSelectionListTreeView categoriesFor: #displayedContents!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #displayedOrHiddenTreeBelow:do:!do copy!enumerating!private! !
!MutipleSelectionListTreeView categoriesFor: #displayedTreeBelow:do:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #displayedTreeDo:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #displayedTreeOf:do:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #displayIndexOf:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #displayIndexOf:ifAbsent:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #doSortChildrenBlock!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #doSortChildrenBlock:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #doSortRoots!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #doSortRoots:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #ensureItemDisplayed:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #expand!commands!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #expand:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #expand:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandAll!commands!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #expandAll:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #expandAllList:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandAllOrCollapseSelectedBecause:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandList:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandOrCollapse:because:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandOrSelectFirstChildBecause:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #expandSelectedBecause:!do copy!private!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #getChildrenBlock!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #getChildrenBlock:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #getParentBlock!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #getParentBlock:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasButtons!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasButtons:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasCheckBoxes!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasCheckBoxes:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasChildren:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasChildrenBlock!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasChildrenBlock:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasHotTracking!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasHotTracking:!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasIcons!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasLines!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasLines:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #hasLinesAtRoot!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #hasLinesAtRoot:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #indentationFor:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #indentFromRow:!adapters!do copy!private! !
!MutipleSelectionListTreeView categoriesFor: #indentSeparation!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #indentSeparation:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #initialize!do copy!initializing!private! !
!MutipleSelectionListTreeView categoriesFor: #inOrder:parent:!do copy!private!sorting! !
!MutipleSelectionListTreeView categoriesFor: #inRootOrder:!do copy!private!sorting! !
!MutipleSelectionListTreeView categoriesFor: #invalidateItem:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #invalidateItemIndex:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #isItemDisplayed:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isItemExpandable:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isItemExpanded:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isItemHiddenExpanded:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isLargeIcons!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isMonoExpandable!accessing-styles!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isMonoExpandable:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #isSelected:!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isSmallIcons!do copy!public!testing! !
!MutipleSelectionListTreeView categoriesFor: #isVirtual:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #listModel:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #makeInternalListModel:searchPolicy:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #makeInternalTreeModel:searchPolicy:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #makeSet!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #makeSetFrom:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #minimumIndent!constants!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #notifyNodeAdded:!do copy!events!private! !
!MutipleSelectionListTreeView categoriesFor: #notifyNodeCollapsed:!do copy!events!private! !
!MutipleSelectionListTreeView categoriesFor: #notifyNodeExpanded:!do copy!events!private! !
!MutipleSelectionListTreeView categoriesFor: #notifyNodeRemoved:!do copy!events!private! !
!MutipleSelectionListTreeView categoriesFor: #onDestroyed!do copy!event handling!models!public! !
!MutipleSelectionListTreeView categoriesFor: #onDisplayDetailsRequired:!do copy!event handling!private! !
!MutipleSelectionListTreeView categoriesFor: #onFullyCreated!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #onItem:addedInParent:!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #onItem:movedToParent:!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #onItem:removedFromParent:!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #onItemUpdated:!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #onKeyPressed:!do copy!event handling!public!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #onLeftButtonDoubleClicked:!do copy!event handling!public!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #onLeftButtonPressed:!do copy!event handling!public!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #onTreeChanged:!do copy!event handling!public! !
!MutipleSelectionListTreeView categoriesFor: #openState!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #parentOf:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #parentsOf:includesAny:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #previousDisplayedSiblingOf:in:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #refreshContents!do copy!public!updating! !
!MutipleSelectionListTreeView categoriesFor: #refreshFromModel!do copy!public!updating! !
!MutipleSelectionListTreeView categoriesFor: #refreshFromModelBelow:!do copy!public!updating! !
!MutipleSelectionListTreeView categoriesFor: #refreshNonVirtual!do copy!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #removeAllExcept:!do copy!expanding/collapsing!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #removeExpanded:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #removeFromDisplay:!do copy!private! !
!MutipleSelectionListTreeView categoriesFor: #removeHiddenExpanded:!do copy!helpers!private! !
!MutipleSelectionListTreeView categoriesFor: #removeZombies!do copy!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #resetTreeToRoots!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #resolutionScaledBy:!do copy!geometry!private! !
!MutipleSelectionListTreeView categoriesFor: #retainExpanded!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #retainExpanded:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #roots!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #searchPolicy!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #selectFirstChildOf:because:!do copy!private!selection!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #selectionAfterCollapseList:!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionAfterExpandAllList:!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionAfterExpandList:!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionOrRoots!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionRoots!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionRootsOrRoots!do copy!private!selection! !
!MutipleSelectionListTreeView categoriesFor: #selections:ifAbsent:!do not copy!public!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectionsAsSet!accessing!do copy!public!selection! !
!MutipleSelectionListTreeView categoriesFor: #selectParentOf:because:!do copy!private!selection!user initiated actions! !
!MutipleSelectionListTreeView categoriesFor: #setItem:indentation:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #setItem:openState:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #setItemIndentation:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #setItemState:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #setModel:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #sortBlock!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #sortBlock:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #sortOnColumn:!columns!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #stateFor:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #stateImageIndex:!do copy!private!states! !
!MutipleSelectionListTreeView categoriesFor: #stateIndexFromRow:!adapters!do copy!private! !
!MutipleSelectionListTreeView categoriesFor: #stbSaveOn:!binary filing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #systemDrawsStateImages!do not copy!private!states!testing! !
!MutipleSelectionListTreeView categoriesFor: #toggleExpandAll:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #toggleExpanded:!do copy!expanding/collapsing!public! !
!MutipleSelectionListTreeView categoriesFor: #treeModel!accessing!do copy!models!public! !
!MutipleSelectionListTreeView categoriesFor: #treeModel:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #treeModel:listModel:!accessing!do copy!models!private! !
!MutipleSelectionListTreeView categoriesFor: #unorderedChildrenOf:!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #unorderedRoots!accessing!do copy!public!sorting! !
!MutipleSelectionListTreeView categoriesFor: #unorderedTreeBelow:do:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #unorderedTreeDo:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #unorderedTreeOf:do:!do copy!enumerating!public! !
!MutipleSelectionListTreeView categoriesFor: #updateDisplayOrder!do copy!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #updateDisplayOrderFrom:to:!do copy!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #updateExpanded!do copy!private!updating! !
!MutipleSelectionListTreeView categoriesFor: #updateMode!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #updateMode:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #updateStates!do copy!private!states!updating! !
!MutipleSelectionListTreeView categoriesFor: #useSmartRefresh!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #useSmartRefresh:!accessing-styles!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #veryBasicCollapse:!do copy!expanding/collapsing!private! !
!MutipleSelectionListTreeView categoriesFor: #viewMode:!accessing!do copy!public! !
!MutipleSelectionListTreeView categoriesFor: #wantExtendedSelection!do copy!private!testing! !

!MutipleSelectionListTreeView class methodsFor!

convertToLocalVersion0: anArray from: anInteger
	"private -- answer a version of anArray that represents at least version 0 of our instances'
	layout"

	"the recursion grounds here"
	^ anArray.!

convertToLocalVersion1: anArray from: anInteger
	"private -- answer a version of anArray that represents at least version 1 of our instances'
	layout"

	| array index |

	anInteger >= 1 ifTrue: [^ anArray].

	array := self convertToLocalVersion0: anArray from: anInteger.

	"going from 0 to 1 changed the types of expanded and hiddenExpanded to PluggableSet"
	index := self indexOfInstVar: 'expanded'.
	array at: index put: (self setClass withAll: (array at: index)).
	index := self indexOfInstVar: 'hiddenExpanded'.
	(array at: index) isNil ifFalse: [array at: index put: (self setClass withAll: (array at: index))].

	"and 'dummy3' (nee 'selChangeCause') is no longer used"
	index := self indexOfInstVar: 'dummy3'.
	array at: index put: nil.

	^ array.!

defaultOptions
	"answer the bitmap of options that instances have by default"

	^ HasButtonsMask | DoSortRootsMask.!

installViewResources
	"private -- install instances as named resources associated
	with various Presenter classes.

		self installViewResources.
	"

	ListTreePresenter addView: self asResource: 'Multi-selection view'.
	ListPresenter addView: self asResource: 'Multi-selection ListTree view'.
!

internalListModelClass
	"answer the factory object to use for our instances' internally
	created list models"

	^ ListModel.!

internalTreeModelClass
	"answer the factory object to use for our instances' internal tree
	models (i.e. when one has not been set explicitly)"

	"we use this, rather than VirtualTreeModel, since it supports pluggable
	search policies (hence the name!!)"
	^ VirtualTreeModelWithSearchPolicy.!

localStbVersion
	"private -- answer the 'real' binary filer version number for instances of the receiver.
	This is allowed to vary independenty of our superclass's #stbVersion"

	^ 1.!

publishedAspectsOfInstances
    	"answer the <Aspect>s published by instances of the receiver"
    
	^ (super publishedAspectsOfInstances)

		removeKey: #hasCheckBoxes ifAbsent: [];			"	-- uses the same 'state' stuff as we need for 'buttons'"

		add: (Aspect boolean: #hasButtons);
"		add: (Aspect boolean: #hasLines);					-- dummied out, we don't have any lines :-( "
		add: (Aspect boolean: #hasLinesAtRoot);			"	-- really means #hasButtonsAtRoot"
		add: (Aspect boolean: #hasHotTracking);
"		add: (Aspect boolean: #isMonoExpandable);		-- dummied out, too much effort for no value"
		add: (Aspect integer: #indentSeparation);
		add: (Aspect boolean: #disableExpandAll);
"		add: (Aspect choice: #updateMode from: TreeView updateModes);
											-- dummied out, not meaningful for lists"
		add: (Aspect boolean: #retainExpanded);
		add: (Aspect boolean: #useSmartRefresh);
		add: (Aspect block: #hasChildrenBlock);
		add: (Aspect block: #getChildrenBlock);
		add: (Aspect block: #getParentBlock);

		add: (Aspect block: #sortBlock);
		add: (Aspect boolean: #doSortRoots);
		add: (Aspect block: #doSortChildrenBlock);

		yourself.!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by instances of the receiver.
	ListTreeViews generate a few extra events that would be useful coming
	off TreeViews too (as with most views, the events are actually triggered
	off our presenter)"

	^ (super publishedEventsOfInstances)
		add: #nodeAdded:;
		add: #nodeRemoved:;
		add: #nodeExpanded:;
		add: #nodeCollapsed:;
		yourself.!

setClass
	"answer the factory object to use to create instances' lists of
	which objects satisfy some condition"

	^ PluggableSet.!

stateImageExtent
	"answer a Point indicating the size we use for state images.
	Note that the state images don't visibly fill the extent"

	^ self stateImageWidth @ self stateImageHeight.!

stateImageHeight
	"answer the height in pixels of our 'state' images.
	Note that the state images don't visibly fill the extent"

	^ 16.!

stateImageList
	"answer an ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	^ self stateImageManager imageListWithExtent: self stateImageExtent.
!

stateImageManager
	"answer the shared ImageMaster that contains the image lists for the
	leaf/expanded/not-expanded images"

	^ ListTreeView stateImageManager.!

stateImageWidth
	"answer the width in pixels of our 'state' images.
	Note that the state images don't visibly fill the extent"

	^ 16.!

stateNames
	"answer an Array of Symbol names of states, these are in the same order
	as the corresponding images in the stateImageList"

	"#SmallOpen and #SmallClosed are not actually used yet"
	^ #(#Leaf #Open #Closed #SmallOpen #SmallClosed).!

stbConvert: anArray fromLocalVersion: anInteger
	"private -- answer a version of anArray that represents the current version of our instances'
	layout.  Note that the version number is our *local* version that varies independently of
	our superclass's version"

	^ self convertToLocalVersion1: anArray from: anInteger.!

stbConvert: anArray fromVersion: anInteger
	"private -- convert from an earlier version by updating and answering the array of instance
	variables.  We have to be tricksy here, since we need to allow for both changes to *our*
	encoding and changes (independantly) made in the super class.  See #stbVersion"

	| localVersion superVersion |

	"decode the two version numbers packed into anInteger"
	localVersion := anInteger quo: self stbEncodingPrime.
	superVersion := anInteger rem: self stbEncodingPrime.

	^ self
		stbConvert: (super stbConvert: anArray fromVersion: superVersion)
		fromLocalVersion: localVersion.!

stbEncodingPrime
	"private -- the prime number that we use to pack two stb versions into one
	integer"

	"should be big enough<pray/> -- allows our superclass to get up to version
	996 before anything breaks"
	^ 997.!

stbVersion
	"answer the current binary filer version number for instances of the receiver.
	Since we need to allow the superclass's version number to increment without
	messing up our own version number, we use an arithmetic coding that allows
	us to 'pack' two version numbers into one integer"

	"we do it by multiplying one by a sufficently large prime, that gives us
	unamgiguity over the range [0, thePrime).
	Note that we multiply *our* local version by the prime, this is necessary
	to allow for older instances to appear to have a local version of 0"
	^ self stbEncodingPrime * self localStbVersion + super stbVersion.! !
!MutipleSelectionListTreeView class categoriesFor: #convertToLocalVersion0:from:!binary filing!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #convertToLocalVersion1:from:!binary filing!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #defaultOptions!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #installViewResources!development!do not copy!must strip!private! !
!MutipleSelectionListTreeView class categoriesFor: #internalListModelClass!constants!do not copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #internalTreeModelClass!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #localStbVersion!binary filing!constants!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #publishedAspectsOfInstances!constants!development!do copy!must strip!public! !
!MutipleSelectionListTreeView class categoriesFor: #publishedEventsOfInstances!constants!do copy!events!public! !
!MutipleSelectionListTreeView class categoriesFor: #setClass!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateImageExtent!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateImageHeight!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateImageList!accessing!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateImageManager!accessing!do not copy!image management!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateImageWidth!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stateNames!constants!do copy!public! !
!MutipleSelectionListTreeView class categoriesFor: #stbConvert:fromLocalVersion:!binary filing!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #stbConvert:fromVersion:!binary filing!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #stbEncodingPrime!binary filing!constants!do copy!private! !
!MutipleSelectionListTreeView class categoriesFor: #stbVersion!binary filing!do copy!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ListPresenter name: 'ListTree view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAANEEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAACwAAAENVIExpc3RUcmVlUgAAAAwAAABMaXN0VHJlZVZpZXdiAAAALAAAAAAA
AAAAAAAAYgAAAAIAAACCAAAABAAAAE0QAUQBBAAAoAEAAEYDCQACAAAATGlzdE1vZGVsAAAAAMoA
AAAAAAAA0AAAAGIAAAAAAAAAAAAAAAYAFABJZGVudGl0eVNlYXJjaFBvbGljeQAAAAAAAAAAAAAA
AAUAAAAAAAAAAAAAAAAAAACgAQAAAAAAAIIAAAAIAAAAbwP//wAAAACaAAAAAAAAAFIAAAAQAAAA
RG9scGhpbiBNVlAgQmFzZVIAAAARAAAAQmFzaWNMaXN0QWJzdHJhY3SaAAAAAAAAAFIAAAAXAAAA
RG9scGhpbiBDb21tb24gQ29udHJvbHNSAAAAEgAAAEljb25pY0xpc3RBYnN0cmFjdA4CEQBTVEJT
aW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAJACAABSAAAAEAAAAEljb25JbWFnZU1hbmFnZXK6AAAA
AAAAAFIAAAAHAAAAY3VycmVudAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMoAAAAAAAAA0AAAAGIA
AAABAAAARgwOAAUAAABMaXN0Vmlld0NvbHVtbgAAAABSAAAACAAAAENvbHVtbiAxyQAAALoAAAAA
AAAAUgAAAAQAAABsZWZ0gAIAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABAAAABTb3J0ZWRD
b2xsZWN0aW9uAAAAAAAAAACgAQAAAAAAAAMAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYAAAByZXBv
cnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAgAAAARgcgAAEAAABWaXJ0dWFsVHJlZU1vZGVsV2l0
aFNlYXJjaFBvbGljeQAAAAAAAAAAygAAAAAAAADQAAAAQAIAAAYCBwBNZXNzYWdlAAAAALoAAAAA
AAAAUgAAAAgAAABjaGlsZHJlbmIAAAAAAAAAQgQAAAAAAAC6AAAAAAAAAFIAAAAGAAAAcGFyZW50
gAQAAAAAAABCBAAAAAAAALoAAAAAAAAAUgAAAAYAAABub3ROaWyABAAAYAIAAAAAAADKAAAAAAAA
AJoAAAAAAAAAwAMAAFIAAAAMAAAAUGx1Z2dhYmxlU2V0QAIAAMoAAAAAAAAAAAUAAEACAAAlAAAA
AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVh
dGVBdDpleHRlbnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQAAAAEAAADCBQAAAAAAAJEBAAC9AgAA
oAEAAAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////
////////AAAAAAAAAADIAAAAXgEAAMoAAAAAAAAA0AAAAEACAADCBQAAAAAAAMEAAADBAAAAAAAA
AOEHAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAA
AAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAA
AAAAUgAAAAcAAABjdXJyZW50UgAAABYAAABJY29uaWNMaXN0QWJzdHJhY3QuaWNvDgIfAFNUQkV4
dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAA
AA=='))!

(ResourceIdentifier class: ListPresenter name: 'Multi-selection ListTree view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOEEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAACwAAAENVIExpc3RUcmVlUgAAABwAAABNdXRpcGxlU2VsZWN0aW9uTGlzdFRy
ZWVWaWV3YgAAACwAAAAAAAAAAAAAAGIAAAACAAAAggAAAAQAAABJEAFEAQQAAKABAABGAwkAAgAA
AExpc3RNb2RlbAAAAADKAAAAAAAAANAAAABiAAAAAAAAAAAAAAAGABQASWRlbnRpdHlTZWFyY2hQ
b2xpY3kAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAoAEAAAAAAACCAAAACAAAAG8D//8AAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0
mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9uIENvbnRyb2xzUgAAABIAAABJY29uaWNMaXN0
QWJzdHJhY3QOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAACQAgAAUgAAABAAAABJY29u
SW1hZ2VNYW5hZ2VyugAAAAAAAABSAAAABwAAAGN1cnJlbnQAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AADKAAAAAAAAANAAAABiAAAAAQAAAEYMDgAFAAAATGlzdFZpZXdDb2x1bW4AAAAAUgAAAAgAAABD
b2x1bW4gMckAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdIACAACaAAAAAAAAAFIAAAAHAAAARG9scGhp
blIAAAAQAAAAU29ydGVkQ29sbGVjdGlvbgAAAAAAAAAAoAEAAAAAAAADAAAAAAAAAAAAAAC6AAAA
AAAAAFIAAAAGAAAAcmVwb3J0YgAAAAAAAAAAAAAAYQgAAAAAAAAAAAAAIAAAAEYHIAABAAAAVmly
dHVhbFRyZWVNb2RlbFdpdGhTZWFyY2hQb2xpY3kAAAAAAAAAAMoAAAAAAAAA0AAAAEACAAAGAgcA
TWVzc2FnZQAAAAC6AAAAAAAAAFIAAAAIAAAAY2hpbGRyZW5iAAAAAAAAAEIEAAAAAAAAugAAAAAA
AABSAAAABgAAAHBhcmVudIAEAAAAAAAAQgQAAAAAAAC6AAAAAAAAAFIAAAAGAAAAbm90TmlsgAQA
AGACAAAAAAAAygAAAAAAAACaAAAAAAAAAMADAABSAAAADAAAAFBsdWdnYWJsZVNldEACAADKAAAA
AAAAAAAFAABAAgAAJQAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgEPAE1lc3Nh
Z2VTZXF1ZW5jZQAAAADKAAAAAAAAANAAAABiAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAA
AAAAAFIAAAAQAAAAY3JlYXRlQXQ6ZXh0ZW50OmIAAAACAAAABgIFAFBvaW50AAAAAAEAAAABAAAA
wgUAAAAAAACRAQAAvQIAAKABAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAA
AAAAAAAA/////////////////////wAAAAAAAAAAyAAAAF4BAADKAAAAAAAAANAAAABAAgAAwgUA
AAAAAADBAAAAwQAAAAAAAADhBwAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmds
ZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZl
RmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAAdAAAATXVsdGlwbGVTZWxlY3Rp
b25MaXN0Vmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAA
EAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: ListTreePresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAANEEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAACwAAAENVIExpc3RUcmVlUgAAAAwAAABMaXN0VHJlZVZpZXdiAAAALAAAAAAA
AAAAAAAAYgAAAAIAAACCAAAABAAAAE0QAUQBBAAAoAEAAEYDCQACAAAATGlzdE1vZGVsAAAAAMoA
AAAAAAAA0AAAAGIAAAAAAAAAAAAAAAYAFABJZGVudGl0eVNlYXJjaFBvbGljeQAAAAAAAAAAAAAA
AAUAAAAAAAAAAAAAAAAAAACgAQAAAAAAAIIAAAAIAAAAbwP//wAAAACaAAAAAAAAAFIAAAAQAAAA
RG9scGhpbiBNVlAgQmFzZVIAAAARAAAAQmFzaWNMaXN0QWJzdHJhY3SaAAAAAAAAAFIAAAAXAAAA
RG9scGhpbiBDb21tb24gQ29udHJvbHNSAAAAEgAAAEljb25pY0xpc3RBYnN0cmFjdA4CEQBTVEJT
aW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAJACAABSAAAAEAAAAEljb25JbWFnZU1hbmFnZXK6AAAA
AAAAAFIAAAAHAAAAY3VycmVudAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMoAAAAAAAAA0AAAAGIA
AAABAAAARgwOAAUAAABMaXN0Vmlld0NvbHVtbgAAAABSAAAACAAAAENvbHVtbiAxyQAAALoAAAAA
AAAAUgAAAAQAAABsZWZ0gAIAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABAAAABTb3J0ZWRD
b2xsZWN0aW9uAAAAAAAAAACgAQAAAAAAAAMAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYAAAByZXBv
cnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAgAAAARgcgAAEAAABWaXJ0dWFsVHJlZU1vZGVsV2l0
aFNlYXJjaFBvbGljeQAAAAAAAAAAygAAAAAAAADQAAAAQAIAAAYCBwBNZXNzYWdlAAAAALoAAAAA
AAAAUgAAAAgAAABjaGlsZHJlbmIAAAAAAAAAQgQAAAAAAAC6AAAAAAAAAFIAAAAGAAAAcGFyZW50
gAQAAAAAAABCBAAAAAAAALoAAAAAAAAAUgAAAAYAAABub3ROaWyABAAAYAIAAAAAAADKAAAAAAAA
AJoAAAAAAAAAwAMAAFIAAAAMAAAAUGx1Z2dhYmxlU2V0QAIAAMoAAAAAAAAAAAUAAEACAAAlAAAA
AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVh
dGVBdDpleHRlbnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQAAAAEAAADCBQAAAAAAAJEBAAC9AgAA
oAEAAAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////
////////AAAAAAAAAADIAAAAXgEAAMoAAAAAAAAA0AAAAEACAADCBQAAAAAAAMEAAADBAAAAAAAA
AOEHAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAA
AAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAA
AAAAUgAAAAcAAABjdXJyZW50UgAAABYAAABJY29uaWNMaXN0QWJzdHJhY3QuaWNvDgIfAFNUQkV4
dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAA
AA=='))!

(ResourceIdentifier class: ListTreePresenter name: 'Multi-selection view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOEEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAACwAAAENVIExpc3RUcmVlUgAAABwAAABNdXRpcGxlU2VsZWN0aW9uTGlzdFRy
ZWVWaWV3YgAAACwAAAAAAAAAAAAAAGIAAAACAAAAggAAAAQAAABJEAFEAQQAAKABAABGAwkAAgAA
AExpc3RNb2RlbAAAAADKAAAAAAAAANAAAABiAAAAAAAAAAAAAAAGABQASWRlbnRpdHlTZWFyY2hQ
b2xpY3kAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAoAEAAAAAAACCAAAACAAAAG8D//8AAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0
mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9uIENvbnRyb2xzUgAAABIAAABJY29uaWNMaXN0
QWJzdHJhY3QOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAACQAgAAUgAAABAAAABJY29u
SW1hZ2VNYW5hZ2VyugAAAAAAAABSAAAABwAAAGN1cnJlbnQAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AADKAAAAAAAAANAAAABiAAAAAQAAAEYMDgAFAAAATGlzdFZpZXdDb2x1bW4AAAAAUgAAAAgAAABD
b2x1bW4gMckAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdIACAACaAAAAAAAAAFIAAAAHAAAARG9scGhp
blIAAAAQAAAAU29ydGVkQ29sbGVjdGlvbgAAAAAAAAAAoAEAAAAAAAADAAAAAAAAAAAAAAC6AAAA
AAAAAFIAAAAGAAAAcmVwb3J0YgAAAAAAAAAAAAAAYQgAAAAAAAAAAAAAIAAAAEYHIAABAAAAVmly
dHVhbFRyZWVNb2RlbFdpdGhTZWFyY2hQb2xpY3kAAAAAAAAAAMoAAAAAAAAA0AAAAEACAAAGAgcA
TWVzc2FnZQAAAAC6AAAAAAAAAFIAAAAIAAAAY2hpbGRyZW5iAAAAAAAAAEIEAAAAAAAAugAAAAAA
AABSAAAABgAAAHBhcmVudIAEAAAAAAAAQgQAAAAAAAC6AAAAAAAAAFIAAAAGAAAAbm90TmlsgAQA
AGACAAAAAAAAygAAAAAAAACaAAAAAAAAAMADAABSAAAADAAAAFBsdWdnYWJsZVNldEACAADKAAAA
AAAAAAAFAABAAgAAJQAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgEPAE1lc3Nh
Z2VTZXF1ZW5jZQAAAADKAAAAAAAAANAAAABiAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAA
AAAAAFIAAAAQAAAAY3JlYXRlQXQ6ZXh0ZW50OmIAAAACAAAABgIFAFBvaW50AAAAAAEAAAABAAAA
wgUAAAAAAACRAQAAvQIAAKABAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAA
AAAAAAAA/////////////////////wAAAAAAAAAAyAAAAF4BAADKAAAAAAAAANAAAABAAgAAwgUA
AAAAAADBAAAAwQAAAAAAAADhBwAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmds
ZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZl
RmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAAdAAAATXVsdGlwbGVTZWxlY3Rp
b25MaXN0Vmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAA
EAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: TreePresenter name: 'ListTree view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAANEEAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAACwAAAENVIExpc3RUcmVlUgAAAAwAAABMaXN0VHJlZVZpZXdiAAAALAAAAAAA
AAAAAAAAYgAAAAIAAACCAAAABAAAAE0QAUQBBAAAoAEAAEYDCQACAAAATGlzdE1vZGVsAAAAAMoA
AAAAAAAA0AAAAGIAAAAAAAAAAAAAAAYAFABJZGVudGl0eVNlYXJjaFBvbGljeQAAAAAAAAAAAAAA
AAUAAAAAAAAAAAAAAAAAAACgAQAAAAAAAIIAAAAIAAAAbwP//wAAAACaAAAAAAAAAFIAAAAQAAAA
RG9scGhpbiBNVlAgQmFzZVIAAAARAAAAQmFzaWNMaXN0QWJzdHJhY3SaAAAAAAAAAFIAAAAXAAAA
RG9scGhpbiBDb21tb24gQ29udHJvbHNSAAAAEgAAAEljb25pY0xpc3RBYnN0cmFjdA4CEQBTVEJT
aW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAJACAABSAAAAEAAAAEljb25JbWFnZU1hbmFnZXK6AAAA
AAAAAFIAAAAHAAAAY3VycmVudAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMoAAAAAAAAA0AAAAGIA
AAABAAAARgwOAAUAAABMaXN0Vmlld0NvbHVtbgAAAABSAAAACAAAAENvbHVtbiAxyQAAALoAAAAA
AAAAUgAAAAQAAABsZWZ0gAIAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABAAAABTb3J0ZWRD
b2xsZWN0aW9uAAAAAAAAAACgAQAAAAAAAAMAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYAAAByZXBv
cnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAgAAAARgcgAAEAAABWaXJ0dWFsVHJlZU1vZGVsV2l0
aFNlYXJjaFBvbGljeQAAAAAAAAAAygAAAAAAAADQAAAAQAIAAAYCBwBNZXNzYWdlAAAAALoAAAAA
AAAAUgAAAAgAAABjaGlsZHJlbmIAAAAAAAAAQgQAAAAAAAC6AAAAAAAAAFIAAAAGAAAAcGFyZW50
gAQAAAAAAABCBAAAAAAAALoAAAAAAAAAUgAAAAYAAABub3ROaWyABAAAYAIAAAAAAADKAAAAAAAA
AJoAAAAAAAAAwAMAAFIAAAAMAAAAUGx1Z2dhYmxlU2V0QAIAAMoAAAAAAAAAAAUAAEACAAAlAAAA
AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVh
dGVBdDpleHRlbnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQAAAAEAAADCBQAAAAAAAJEBAAC9AgAA
oAEAAAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////
////////AAAAAAAAAADIAAAAXgEAAMoAAAAAAAAA0AAAAEACAADCBQAAAAAAAMEAAADBAAAAAAAA
AOEHAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAA
AAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRvcroAAAAA
AAAAUgAAAAcAAABjdXJyZW50UgAAABYAAABJY29uaWNMaXN0QWJzdHJhY3QuaWNvDgIfAFNUQkV4
dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAA
AA=='))!

