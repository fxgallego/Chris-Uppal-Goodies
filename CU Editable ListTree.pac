| package |
package := Package name: 'CU Editable ListTree'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

A package which extends John Aspinal''s excellent EditableListView (http://www.solutionsoft.co.uk/widgets) to make a corresponding EditableListTreeView.

This version was developed against v1.10 of EditableListView. It will not work with the very earliest versions (before John started using a version number).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

History:

2.00
-	Replicated changes in ListTree v 8.

1.00
-	First release.

'.

package basicPackageVersion: '2.02'.


package classNames
	add: #EditableListTreeView;
	add: #ListModelForEditableListTree;
	yourself.

package methodNames
	add: #EditableListView -> #initialize;
	yourself.

package resourceNames
	add: #ListPresenter -> 'Editable ListTree view';
	add: #ListTreePresenter -> 'Editable view';
	add: #TreePresenter -> 'Editable ListTree view';
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ListPresenter -> 'Editable ListTree view';
	add: #ListTreePresenter -> 'Editable view';
	add: #TreePresenter -> 'Editable ListTree view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ListTree';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Solutions Software\Widgets\SSW EditableListView';
	yourself).

package!

"Class Definitions"!

ListModel subclass: #ListModelForEditableListTree
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EditableListView subclass: #EditableListTreeView
	instanceVariableNames: 'hasExplicitTreeModel treeModel dummy3 expanded hiddenExpanded options indentSeparation getChildrenBlock hasChildrenBlock getParentBlock sortBlock doSortChildrenBlock dummy2 dummy1'
	classVariableNames: ''
	poolDictionaries: 'ListTreeConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!EditableListView methodsFor!

initialize

#CUadded.	"added this since the rowHeight, etc are only set in the stored view resources
		so that instances created with #new are invalid"

	super initialize.

	self
		isMultiSelect: false;
		rowHeight: 1.
! !
!EditableListView categoriesFor: #initialize!initializing!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

ListModelForEditableListTree guid: (GUID fromString: '{EEBA4CB7-0537-4560-888B-FE061C1342DD}')!
ListModelForEditableListTree comment: 'Copyright © Chris Uppal, 2003.
chris.uppal@metagnostic.org

This is a ''private'' class used internally by EditableListTreeViews.

The point is to get around a problem caused by the use the Observer pattern to keep a ListView (or, in this case EditableListTreeView) in synch with the ListModel that it is a View of.  The problem is that change notifications are sent *after* the chance has been made, by which time it''s too late for the editable columns to disengage themselves.  Therefore these objects are used instead, and they override the necessary (and *only* the necessary) methods to trigger an #aboutToChangeList event, which is trapped by the EditableListTreeView so as to cancel any ongoing edits *before* then change occurs.

NB: this technique of overriding only the methods that are actually used is undoubtedly fragile, but the alternatives are just as bad...'!
!ListModelForEditableListTree categoriesForClass!Unclassified! !
!ListModelForEditableListTree methodsFor!

add: newElement afterIndex: anIndex
	"overridden to generate an #aboutToChangeList event first"

	self notifyListChanging.

	^ super add: newElement afterIndex: anIndex.!

list: aSequenceableCollection
	"overridden to generate an #aboutToChangeList event first"

	self notifyListChanging.

	^ super list: aSequenceableCollection.!

notifyListChanging
	"private -- notify listeners that we are about to change the elements of this
	list.
	NB: it is important to realise that we *only* generate these notifications in
	methods that are actually invoked by EditableListTrees -- this is not intended to be
	a general-purpose class"

	self trigger: #aboutToChangeList.!

remove: oldElement ifAbsent: a0Block
	"overridden to generate an #aboutToChangeList event first"

	self notifyListChanging.

	^ super remove: oldElement ifAbsent: a0Block.!

removeAtIndex: anIndex
	"overridden to generate an #aboutToChangeList event first"

	self notifyListChanging.

	^ super removeAtIndex: anIndex.!

replaceFrom: start to: stop with: replacementElements startingAt: repStart
	"overridden to generate an #aboutToChangeList event first"

	self notifyListChanging.

	^ super replaceFrom: start to: stop with: replacementElements startingAt: repStart.! !
!ListModelForEditableListTree categoriesFor: #add:afterIndex:!adding!public! !
!ListModelForEditableListTree categoriesFor: #list:!accessing!public! !
!ListModelForEditableListTree categoriesFor: #notifyListChanging!private!updating! !
!ListModelForEditableListTree categoriesFor: #remove:ifAbsent:!public!removing! !
!ListModelForEditableListTree categoriesFor: #removeAtIndex:!public!removing! !
!ListModelForEditableListTree categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing! !

!ListModelForEditableListTree class methodsFor!

publishedEventsOfInstances
	"the usual..."

	^ (super publishedEventsOfInstances)
		add: #aboutToChangeList;
		yourself.! !
!ListModelForEditableListTree class categoriesFor: #publishedEventsOfInstances!constants!development!events!public! !

EditableListTreeView guid: (GUID fromString: '{928C455C-60E2-49F1-B3CA-09482F8EC14B}')!
EditableListTreeView comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org


EditableListTreeView bears the same relationship to EditableListView as ListTreeView does to ListView.

IMPORTANT NOTE:  ALL THE METHODS THAT ARE NOT IN THE ''do not copy'' CATEGORY ARE INTENDED TO BE EXACT COPIES OF THOSE IN ListTreeView.

Many thanks to John Aspinal, not only for the EditableListView itself, but also for helping to create this hybrid.

Please note one restriction -- these views (unlike ListTreeView) only support identity comparison policies.

BTW, as far as I know the superclass is not happy to have columns added after the View is open so the examples in ListTreeView class won''t work with one of these -- you have to set up the columns in the View Composer or do it before the View is opened.'!
!EditableListTreeView categoriesForClass!Unclassified! !
!EditableListTreeView methodsFor!

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
	connect to our internal tree model and to arrange to cancel ongoing edits when the
	underlying list changes"

	super connectModel.

	self model
		when: #aboutToChangeList
		send: #commitOngoingEdit
		to: self.

	self connectTreeModel.!

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

drawStateImage: aContext
	"private -- draw our own state image into the given context.
	NB: this is not normally used except by multi-line EditableTreeListViews.

	Special thanks to John Aspinall of SolutionSoft for this code"

	| imageRect stateRect state stateIndex origin |

	imageRect := self
			lvmGetSubItemRect: (aContext dwItemSpec@aContext iSubItem) 
			bounding: LVIR_ICON.

	stateRect := imageRect moveBy: ((self class stateImageWidth negated - 3)@0).
	state := self stateFor: aContext item.
	stateIndex := self stateImageIndex: state.
	stateIndex < 1 ifTrue: [^ self].

	origin := stateRect topLeft + (0@((stateRect height - self class stateImageHeight) // 2)).
	self class stateImageList
		draw: stateIndex
		on: aContext canvas
		at: origin
		flags: 0.

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

postDraw: aContext columnIndex: anInteger
	"overriden to draw the state image manually if necessary"

	(anInteger = 1 and: [self hasButtons and: [self systemDrawsStateImages not]])
		ifTrue: [self drawStateImage: aContext].

	^ super postDraw: aContext columnIndex: anInteger.!

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

	"Windows gets it wrong if we have multi-line elements"
	^ self isMultiline not.!

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
!EditableListTreeView categoriesFor: #addAll:afterIndex:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #addExpanded:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #addHiddenExpanded:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #addItem:afterIndex:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #additem:toParent:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #additem:toParent:withSiblings:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #allDisplayedItems!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #allExpandedItems!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #allHiddenExpandedItems!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #applyImageLists!do copy!image management!private! !
!EditableListTreeView categoriesFor: #basicCollapse:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #basicOnItem:movedToParent:!do copy!event handling!private! !
!EditableListTreeView categoriesFor: #beNoIcons!do copy!modes!public! !
!EditableListTreeView categoriesFor: #beSorted:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #checkChangeSelectionFrom:to:because:!do copy!events!helpers!private! !
!EditableListTreeView categoriesFor: #checkSelectAll:because:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #childrenOf:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #closedState!do copy!private!states! !
!EditableListTreeView categoriesFor: #collapse!commands!do copy!public! !
!EditableListTreeView categoriesFor: #collapse:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #collapse:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #collapseAll!commands!do copy!public! !
!EditableListTreeView categoriesFor: #collapseList:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #collapseOrSelectParentBecause:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #collapseSelectedBecause:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #connectModel!do not copy!initializing!models!public! !
!EditableListTreeView categoriesFor: #connectTreeModel!do copy!initializing!models!private! !
!EditableListTreeView categoriesFor: #countDisplayedChildrenOf:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #defaultIndentSeparation!constants!do copy!private! !
!EditableListTreeView categoriesFor: #depthOf:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #disableExpandAll!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #disableExpandAll:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #discardAllExpanded!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #discardExpanded!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #discardHiddenExpanded!do copy!helpers!operations!public! !
!EditableListTreeView categoriesFor: #disconnectFromModel!do copy!initializing!models!public! !
!EditableListTreeView categoriesFor: #displayedContents!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #displayedOrHiddenTreeBelow:do:!do copy!enumerating!private! !
!EditableListTreeView categoriesFor: #displayedTreeBelow:do:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #displayedTreeDo:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #displayedTreeOf:do:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #displayIndexOf:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #displayIndexOf:ifAbsent:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #doSortChildrenBlock!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #doSortChildrenBlock:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #doSortRoots!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #doSortRoots:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #drawStateImage:!do not copy!drawing!private!states! !
!EditableListTreeView categoriesFor: #ensureItemDisplayed:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #expand!commands!do copy!public! !
!EditableListTreeView categoriesFor: #expand:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #expand:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandAll!commands!do copy!public! !
!EditableListTreeView categoriesFor: #expandAll:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #expandAllList:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandAllOrCollapseSelectedBecause:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandList:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandOrCollapse:because:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandOrSelectFirstChildBecause:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #expandSelectedBecause:!do copy!private!user initiated actions! !
!EditableListTreeView categoriesFor: #getChildrenBlock!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #getChildrenBlock:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #getParentBlock!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #getParentBlock:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #hasButtons!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasButtons:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #hasCheckBoxes!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #hasCheckBoxes:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #hasChildren:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasChildrenBlock!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #hasChildrenBlock:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #hasHotTracking!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasHotTracking:!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasIcons!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasLines!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasLines:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #hasLinesAtRoot!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #hasLinesAtRoot:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #indentationFor:!do copy!private!states! !
!EditableListTreeView categoriesFor: #indentFromRow:!adapters!do copy!private! !
!EditableListTreeView categoriesFor: #indentSeparation!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #indentSeparation:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #initialize!do copy!initializing!private! !
!EditableListTreeView categoriesFor: #inOrder:parent:!do copy!private!sorting! !
!EditableListTreeView categoriesFor: #inRootOrder:!do copy!private!sorting! !
!EditableListTreeView categoriesFor: #invalidateItem:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #invalidateItemIndex:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #isItemDisplayed:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isItemExpandable:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isItemExpanded:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isItemHiddenExpanded:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isLargeIcons!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isMonoExpandable!accessing-styles!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isMonoExpandable:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #isSelected:!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isSmallIcons!do copy!public!testing! !
!EditableListTreeView categoriesFor: #isVirtual:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #listModel:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #makeInternalListModel:searchPolicy:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #makeInternalTreeModel:searchPolicy:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #makeSet!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #makeSetFrom:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #minimumIndent!constants!do copy!public! !
!EditableListTreeView categoriesFor: #notifyNodeAdded:!do copy!events!private! !
!EditableListTreeView categoriesFor: #notifyNodeCollapsed:!do copy!events!private! !
!EditableListTreeView categoriesFor: #notifyNodeExpanded:!do copy!events!private! !
!EditableListTreeView categoriesFor: #notifyNodeRemoved:!do copy!events!private! !
!EditableListTreeView categoriesFor: #onDestroyed!do copy!event handling!models!public! !
!EditableListTreeView categoriesFor: #onDisplayDetailsRequired:!do copy!event handling!private! !
!EditableListTreeView categoriesFor: #onFullyCreated!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #onItem:addedInParent:!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #onItem:movedToParent:!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #onItem:removedFromParent:!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #onItemUpdated:!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #onKeyPressed:!do copy!event handling!public!user initiated actions! !
!EditableListTreeView categoriesFor: #onLeftButtonDoubleClicked:!do copy!event handling!public!user initiated actions! !
!EditableListTreeView categoriesFor: #onLeftButtonPressed:!do copy!event handling!public!user initiated actions! !
!EditableListTreeView categoriesFor: #onTreeChanged:!do copy!event handling!public! !
!EditableListTreeView categoriesFor: #openState!do copy!private!states! !
!EditableListTreeView categoriesFor: #parentOf:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #parentsOf:includesAny:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #postDraw:columnIndex:!do not copy!helpers!private! !
!EditableListTreeView categoriesFor: #previousDisplayedSiblingOf:in:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #refreshContents!do copy!public!updating! !
!EditableListTreeView categoriesFor: #refreshFromModel!do copy!public!updating! !
!EditableListTreeView categoriesFor: #refreshFromModelBelow:!do copy!public!updating! !
!EditableListTreeView categoriesFor: #refreshNonVirtual!do copy!private!updating! !
!EditableListTreeView categoriesFor: #removeAllExcept:!do copy!expanding/collapsing!private!updating! !
!EditableListTreeView categoriesFor: #removeExpanded:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #removeFromDisplay:!do copy!private! !
!EditableListTreeView categoriesFor: #removeHiddenExpanded:!do copy!helpers!private! !
!EditableListTreeView categoriesFor: #removeZombies!do copy!private!updating! !
!EditableListTreeView categoriesFor: #resetTreeToRoots!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #resolutionScaledBy:!do copy!geometry!private! !
!EditableListTreeView categoriesFor: #retainExpanded!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #retainExpanded:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #roots!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #searchPolicy!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #selectFirstChildOf:because:!do copy!private!selection!user initiated actions! !
!EditableListTreeView categoriesFor: #selection:ifAbsent:!do not copy!public!selection! !
!EditableListTreeView categoriesFor: #selectionAfterCollapseList:!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selectionAfterExpandAllList:!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selectionAfterExpandList:!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selectionOrRoots!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selectionRoots!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selectionRootsOrRoots!do copy!private!selection! !
!EditableListTreeView categoriesFor: #selections:ifAbsent:!do not copy!public!selection! !
!EditableListTreeView categoriesFor: #selectionsAsSet!accessing!do copy!public!selection! !
!EditableListTreeView categoriesFor: #selectParentOf:because:!do copy!private!selection!user initiated actions! !
!EditableListTreeView categoriesFor: #setItem:indentation:!do copy!private!states! !
!EditableListTreeView categoriesFor: #setItem:openState:!do copy!private!states! !
!EditableListTreeView categoriesFor: #setItemIndentation:!do copy!private!states! !
!EditableListTreeView categoriesFor: #setItemState:!do copy!private!states! !
!EditableListTreeView categoriesFor: #setModel:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #sortBlock!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #sortBlock:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #sortOnColumn:!columns!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #stateFor:!do copy!private!states! !
!EditableListTreeView categoriesFor: #stateImageIndex:!do copy!private!states! !
!EditableListTreeView categoriesFor: #stateIndexFromRow:!adapters!do copy!private! !
!EditableListTreeView categoriesFor: #stbSaveOn:!binary filing!do copy!public! !
!EditableListTreeView categoriesFor: #systemDrawsStateImages!do not copy!private!states!testing! !
!EditableListTreeView categoriesFor: #toggleExpandAll:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #toggleExpanded:!do copy!expanding/collapsing!public! !
!EditableListTreeView categoriesFor: #treeModel!accessing!do copy!models!public! !
!EditableListTreeView categoriesFor: #treeModel:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #treeModel:listModel:!accessing!do copy!models!private! !
!EditableListTreeView categoriesFor: #unorderedChildrenOf:!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #unorderedRoots!accessing!do copy!public!sorting! !
!EditableListTreeView categoriesFor: #unorderedTreeBelow:do:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #unorderedTreeDo:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #unorderedTreeOf:do:!do copy!enumerating!public! !
!EditableListTreeView categoriesFor: #updateDisplayOrder!do copy!private!updating! !
!EditableListTreeView categoriesFor: #updateDisplayOrderFrom:to:!do copy!private!updating! !
!EditableListTreeView categoriesFor: #updateExpanded!do copy!private!updating! !
!EditableListTreeView categoriesFor: #updateMode!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #updateMode:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #updateStates!do copy!private!states!updating! !
!EditableListTreeView categoriesFor: #useSmartRefresh!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #useSmartRefresh:!accessing-styles!do copy!public! !
!EditableListTreeView categoriesFor: #veryBasicCollapse:!do copy!expanding/collapsing!private! !
!EditableListTreeView categoriesFor: #viewMode:!accessing!do copy!public! !
!EditableListTreeView categoriesFor: #wantExtendedSelection!do copy!private!testing! !

!EditableListTreeView class methodsFor!

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

	ListTreePresenter addView: self asResource: 'Editable view'.
	ListPresenter addView: self asResource: 'Editable ListTree view'.			"NB: don't forget to repackage this"
	TreePresenter addView: self asResource: 'Editable ListTree view'.			"or this"
!

internalListModelClass
	"answer the factory object to use for our instances' internally
	created list models"

	^ ListModelForEditableListTree.!

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
!EditableListTreeView class categoriesFor: #convertToLocalVersion0:from:!binary filing!do copy!private! !
!EditableListTreeView class categoriesFor: #convertToLocalVersion1:from:!binary filing!do copy!private! !
!EditableListTreeView class categoriesFor: #defaultOptions!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #installViewResources!development!do not copy!must strip!private! !
!EditableListTreeView class categoriesFor: #internalListModelClass!constants!do not copy!public! !
!EditableListTreeView class categoriesFor: #internalTreeModelClass!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #localStbVersion!binary filing!constants!do copy!private! !
!EditableListTreeView class categoriesFor: #publishedAspectsOfInstances!constants!development!do copy!must strip!public! !
!EditableListTreeView class categoriesFor: #publishedEventsOfInstances!constants!do copy!events!public! !
!EditableListTreeView class categoriesFor: #setClass!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #stateImageExtent!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #stateImageHeight!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #stateImageList!accessing!do copy!public! !
!EditableListTreeView class categoriesFor: #stateImageManager!accessing!do not copy!image management!public! !
!EditableListTreeView class categoriesFor: #stateImageWidth!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #stateNames!constants!do copy!public! !
!EditableListTreeView class categoriesFor: #stbConvert:fromLocalVersion:!binary filing!do copy!private! !
!EditableListTreeView class categoriesFor: #stbConvert:fromVersion:!binary filing!do copy!private! !
!EditableListTreeView class categoriesFor: #stbEncodingPrime!binary filing!constants!do copy!private! !
!EditableListTreeView class categoriesFor: #stbVersion!binary filing!do copy!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ListPresenter name: 'Editable ListTree view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAACAGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAFAAAAENVIEVkaXRhYmxlIExpc3RUcmVlUgAAABQAAABFZGl0YWJsZUxpc3RU
cmVlVmlld2IAAAA0AAAAAAAAAAAAAABiAAAAAgAAAIIAAAAEAAAATRABRAEEAACgAQAARgMcAAIA
AABMaXN0TW9kZWxGb3JFZGl0YWJsZUxpc3RUcmVlAAAAAMoAAAAAAAAA0AAAAGIAAAAAAAAAAAAA
AA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAMAAAA
U2VhcmNoUG9saWN5ugAAAAAAAABSAAAACAAAAGlkZW50aXR5AAAAAAAAAAAFAAAAAAAAAAAAAAAA
AAAAoAEAAAAAAACCAAAACAAAAC8C//8AAAAAmgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJh
c2VSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9u
IENvbnRyb2xzUgAAABIAAABJY29uaWNMaXN0QWJzdHJhY3RaAgAAAAAAAJoAAAAAAAAA4AIAAFIA
AAAQAAAASWNvbkltYWdlTWFuYWdlcroAAAAAAAAAUgAAAAcAAABjdXJyZW50AAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAABGFhYADgAAAEVkaXRhYmxlTGlzdFZpZXdD
b2x1bW4AAAAAUgAAAAgAAABDb2x1bW4gMckAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdNACAACaAAAA
AAAAAIACAABSAAAAEAAAAFNvcnRlZENvbGxlY3Rpb24AAAAAAAAAAKABAAAAAAAAAwAAAAAAAAAA
AAAAEAAAAAAAAABGERAACQAAAEVtYmVkZGVkVGV4dEVkaXQAAAAAAAAAAAAAAABiAAAAAgAAAAEB
AggBAAAAIAQAAEYECwACAAAAVmFsdWVIb2xkZXIAAAAAAAAAACAAAABaAgAAAAAAAHACAAC6AAAA
AAAAAFIAAAAIAAAAZXF1YWxpdHkAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAIAQAAAAAAAAA
AAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYA
AAByZXBvcnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAAAAAAygAAAAAAAADQAAAAQAIAACAAAAAA
AAAAAAAAAAMAAAAAAAAAAAAAACAAAABGByAAAQAAAFZpcnR1YWxUcmVlTW9kZWxXaXRoU2VhcmNo
UG9saWN5AAAAAAAAAADKAAAAAAAAANAAAABAAgAABgIHAE1lc3NhZ2UAAAAAugAAAAAAAABSAAAA
CAAAAGNoaWxkcmVuYgAAAAAAAAACBQAAAAAAALoAAAAAAAAAUgAAAAYAAABwYXJlbnRABQAAAAAA
AAIFAAAAAAAAugAAAAAAAABSAAAABgAAAG5vdE5pbEAFAABgAgAAAAAAAMoAAAAAAAAAmgAAAAAA
AACAAgAAUgAAAAwAAABQbHVnZ2FibGVTZXRAAgAAygAAAAAAAADABQAAQAIAACUAAAABAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQ
AAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4
dGVudDpiAAAAAgAAAAYCBQBQb2ludAAAAAABAAAAAQAAAIIGAAAAAAAAkQEAAL0CAACgAQAABgEP
AFdJTkRPV1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8A
AAAAAAAAAMgAAABeAQAAygAAAAAAAADQAAAAQAIAAIIGAAAAAAAAwQAAAMEAAAAAAAAA5wcAAEYF
BAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIA
AAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAA
BwAAAGN1cnJlbnRSAAAAFgAAAEljb25pY0xpc3RBYnN0cmFjdC5pY28OAh8AU1RCRXh0ZXJuYWxS
ZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: ListTreePresenter name: 'Editable view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAACAGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAFAAAAENVIEVkaXRhYmxlIExpc3RUcmVlUgAAABQAAABFZGl0YWJsZUxpc3RU
cmVlVmlld2IAAAA0AAAAAAAAAAAAAABiAAAAAgAAAIIAAAAEAAAATRABRAEEAACgAQAARgMcAAIA
AABMaXN0TW9kZWxGb3JFZGl0YWJsZUxpc3RUcmVlAAAAAMoAAAAAAAAA0AAAAGIAAAAAAAAAAAAA
AA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAMAAAA
U2VhcmNoUG9saWN5ugAAAAAAAABSAAAACAAAAGlkZW50aXR5AAAAAAAAAAAFAAAAAAAAAAAAAAAA
AAAAoAEAAAAAAACCAAAACAAAAC8C//8AAAAAmgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJh
c2VSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9u
IENvbnRyb2xzUgAAABIAAABJY29uaWNMaXN0QWJzdHJhY3RaAgAAAAAAAJoAAAAAAAAA4AIAAFIA
AAAQAAAASWNvbkltYWdlTWFuYWdlcroAAAAAAAAAUgAAAAcAAABjdXJyZW50AAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAABGFhYADgAAAEVkaXRhYmxlTGlzdFZpZXdD
b2x1bW4AAAAAUgAAAAgAAABDb2x1bW4gMckAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdNACAACaAAAA
AAAAAIACAABSAAAAEAAAAFNvcnRlZENvbGxlY3Rpb24AAAAAAAAAAKABAAAAAAAAAwAAAAAAAAAA
AAAAEAAAAAAAAABGERAACQAAAEVtYmVkZGVkVGV4dEVkaXQAAAAAAAAAAAAAAABiAAAAAgAAAAEB
AggBAAAAIAQAAEYECwACAAAAVmFsdWVIb2xkZXIAAAAAAAAAACAAAABaAgAAAAAAAHACAAC6AAAA
AAAAAFIAAAAIAAAAZXF1YWxpdHkAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAIAQAAAAAAAAA
AAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYA
AAByZXBvcnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAAAAAAygAAAAAAAADQAAAAQAIAACAAAAAA
AAAAAAAAAAMAAAAAAAAAAAAAACAAAABGByAAAQAAAFZpcnR1YWxUcmVlTW9kZWxXaXRoU2VhcmNo
UG9saWN5AAAAAAAAAADKAAAAAAAAANAAAABAAgAABgIHAE1lc3NhZ2UAAAAAugAAAAAAAABSAAAA
CAAAAGNoaWxkcmVuYgAAAAAAAAACBQAAAAAAALoAAAAAAAAAUgAAAAYAAABwYXJlbnRABQAAAAAA
AAIFAAAAAAAAugAAAAAAAABSAAAABgAAAG5vdE5pbEAFAABgAgAAAAAAAMoAAAAAAAAAmgAAAAAA
AACAAgAAUgAAAAwAAABQbHVnZ2FibGVTZXRAAgAAygAAAAAAAADABQAAQAIAACUAAAABAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQ
AAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4
dGVudDpiAAAAAgAAAAYCBQBQb2ludAAAAAABAAAAAQAAAIIGAAAAAAAAkQEAAL0CAACgAQAABgEP
AFdJTkRPV1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8A
AAAAAAAAAMgAAABeAQAAygAAAAAAAADQAAAAQAIAAIIGAAAAAAAAwQAAAMEAAAAAAAAA5wcAAEYF
BAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIA
AAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAA
BwAAAGN1cnJlbnRSAAAAFgAAAEljb25pY0xpc3RBYnN0cmFjdC5pY28OAh8AU1RCRXh0ZXJuYWxS
ZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: TreePresenter name: 'Editable ListTree view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAACAGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAFAAAAENVIEVkaXRhYmxlIExpc3RUcmVlUgAAABQAAABFZGl0YWJsZUxpc3RU
cmVlVmlld2IAAAA0AAAAAAAAAAAAAABiAAAAAgAAAIIAAAAEAAAATRABRAEEAACgAQAARgMcAAIA
AABMaXN0TW9kZWxGb3JFZGl0YWJsZUxpc3RUcmVlAAAAAMoAAAAAAAAA0AAAAGIAAAAAAAAAAAAA
AA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAMAAAA
U2VhcmNoUG9saWN5ugAAAAAAAABSAAAACAAAAGlkZW50aXR5AAAAAAAAAAAFAAAAAAAAAAAAAAAA
AAAAoAEAAAAAAACCAAAACAAAAC8C//8AAAAAmgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJh
c2VSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9u
IENvbnRyb2xzUgAAABIAAABJY29uaWNMaXN0QWJzdHJhY3RaAgAAAAAAAJoAAAAAAAAA4AIAAFIA
AAAQAAAASWNvbkltYWdlTWFuYWdlcroAAAAAAAAAUgAAAAcAAABjdXJyZW50AAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAABGFhYADgAAAEVkaXRhYmxlTGlzdFZpZXdD
b2x1bW4AAAAAUgAAAAgAAABDb2x1bW4gMckAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdNACAACaAAAA
AAAAAIACAABSAAAAEAAAAFNvcnRlZENvbGxlY3Rpb24AAAAAAAAAAKABAAAAAAAAAwAAAAAAAAAA
AAAAEAAAAAAAAABGERAACQAAAEVtYmVkZGVkVGV4dEVkaXQAAAAAAAAAAAAAAABiAAAAAgAAAAEB
AggBAAAAIAQAAEYECwACAAAAVmFsdWVIb2xkZXIAAAAAAAAAACAAAABaAgAAAAAAAHACAAC6AAAA
AAAAAFIAAAAIAAAAZXF1YWxpdHkAAAAAAAAAAAAAAAAFAAAAAAAAAAAAAAAAAAAAIAQAAAAAAAAA
AAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALoAAAAAAAAAUgAAAAYA
AAByZXBvcnRiAAAAAAAAAAAAAABhCAAAAAAAAAAAAAAAAAAAygAAAAAAAADQAAAAQAIAACAAAAAA
AAAAAAAAAAMAAAAAAAAAAAAAACAAAABGByAAAQAAAFZpcnR1YWxUcmVlTW9kZWxXaXRoU2VhcmNo
UG9saWN5AAAAAAAAAADKAAAAAAAAANAAAABAAgAABgIHAE1lc3NhZ2UAAAAAugAAAAAAAABSAAAA
CAAAAGNoaWxkcmVuYgAAAAAAAAACBQAAAAAAALoAAAAAAAAAUgAAAAYAAABwYXJlbnRABQAAAAAA
AAIFAAAAAAAAugAAAAAAAABSAAAABgAAAG5vdE5pbEAFAABgAgAAAAAAAMoAAAAAAAAAmgAAAAAA
AACAAgAAUgAAAAwAAABQbHVnZ2FibGVTZXRAAgAAygAAAAAAAADABQAAQAIAACUAAAABAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQ
AAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4
dGVudDpiAAAAAgAAAAYCBQBQb2ludAAAAAABAAAAAQAAAIIGAAAAAAAAkQEAAL0CAACgAQAABgEP
AFdJTkRPV1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8A
AAAAAAAAAMgAAABeAQAAygAAAAAAAADQAAAAQAIAAIIGAAAAAAAAwQAAAMEAAAAAAAAA5wcAAEYF
BAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIA
AAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAA
BwAAAGN1cnJlbnRSAAAAFgAAAEljb25pY0xpc3RBYnN0cmFjdC5pY28OAh8AU1RCRXh0ZXJuYWxS
ZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

