| package |
package := Package name: 'CU Selection From List Presenter'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Simple Presenter that allows a user to select from a list of items.  Alternative view also allows the user to set the order of the selection.  Looks like the 2-pane selection components used by Windows in various places.  Should be a pluggin replacement for a ListPresenter showing a multiple selection list view.

Includes two replacements for ChoicePresenter that use these components instead of multiple-selection list views.

Also includes two trivial subclasses of ChoicePrompter that use the new presenters instead of multiple-selection list views.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.04'.


package classNames
	add: #ChoicesPresenter;
	add: #ChoicesPrompter;
	add: #OrderedChoicesPresenter;
	add: #OrderedChoicesPrompter;
	add: #SelectionFromListPresenter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ChoicesPresenter -> 'Default view';
	add: #ChoicesPrompter -> 'Extensible multi-selection choice prompter';
	add: #ChoicesPrompter -> 'Multi-selection choice prompter';
	add: #OrderedChoicesPresenter -> 'Default view';
	add: #OrderedChoicesPrompter -> 'Extensible multi-selection choice prompter';
	add: #OrderedChoicesPrompter -> 'Multi-selection choice prompter';
	add: #SelectionFromListPresenter -> 'Default view';
	add: #SelectionFromListPresenter -> 'Ordered view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package!

"Class Definitions"!

Presenter subclass: #SelectionFromListPresenter
	instanceVariableNames: 'available selected sortblock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ChoicePrompter subclass: #ChoicesPrompter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ChoicesPrompter subclass: #OrderedChoicesPrompter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ValuePresenter subclass: #ChoicesPresenter
	instanceVariableNames: 'choices nilChoice listPresenter isHandlingSelectionChanged'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ChoicesPresenter subclass: #OrderedChoicesPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SelectionFromListPresenter guid: (GUID fromString: '{B7207E85-08D4-4A92-96B8-DE9A59A09180}')!
SelectionFromListPresenter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these uses two lists to represent the ''available'' and ''selected'' items, and allows the user to move items back and forth.

Optionally also allows the user to set the order of the selected items.

It is intended to be a plugin replacement for a ListPresenter on a MultipleSelectionListView.  (The major ommision is that we don''t generate #onSelectionChanging: events before the selection is changed -- far too much effor).'!
!SelectionFromListPresenter categoriesForClass!Unclassified! !
!SelectionFromListPresenter methodsFor!

addToSelected
	"command -- move the selected items in 'available' into 'selected'"

	self canAddToSelected ifFalse: [^ self].

	self moveToSelected: self availableListPresenter selections.!

availableLabelPresenter
	"answer the TextPresenter that owns our 'Available' label"

	^ self presenterNamed: 'AvailableLabel'.
!

availableListPresenter
	"answer the ListPresenter that owns our 'Available' list"

	^ self presenterNamed: 'AvailableList'.
!

basicSelections: aCollection
	"private -- set the current selected items to those in aCollection"

	available
		addAll: selected;
		removeAll: aCollection.
	selected
		removeAll;
		addAll: aCollection.

	self trigger: #selectionChanged.!

beNotSorted
	"remove any sorting constriaint on our lists"

	self beSorted: nil.
!

beSorted
	"apply a default sorting constraint to our lists"

	self beSorted: self defaultSortBlock.
!

beSorted: a2Block
	"set the <diadicValuable> that we use to enforce a sort order on our elements.
	This order is actually applied to our model and to both the available and
	selected lists (if you use the #moveSelectedUp commands -- or the user does --
	then the sort constraint on the selected list *only* is removed).
	Note that this will reset the current selection"

	sortblock := a2Block.

	self availableListPresenter beSorted: sortblock.
	self selectedListPresenter beSorted: sortblock.
	self model noEventsDo: [self list: self list].!

canAddToSelected
	"private -- answer true iff we may perform the #addToSelected command ?"

	^ self availableListPresenter hasSelection.!

canMoveSelectedDown
	"private -- answer true iff we may perform the #moveSelectedDown command ?"

	| indices max |

	indices := self selectedListPresenter view selectionsByIndex.
	max := selected size - indices size.

	^ indices notEmpty and: [indices anySatisfy: [:each | each <= max]].
!

canMoveSelectedFirst
	"private -- answer true iff we may perform the #moveSelectedFirst command ?"

	^ self canMoveSelectedUp.
!

canMoveSelectedLast
	"private -- answer true iff we may perform the #moveSelectedLast command ?"

	^ self canMoveSelectedDown.
!

canMoveSelectedUp
	"private -- answer true iff we may perform the #moveSelectedUp command ?"

	| indices min |

	indices := self selectedListPresenter view selectionsByIndex.
	min := indices size.

	^ indices notEmpty and: [indices anySatisfy: [:each | each > min]].!

canRemoveFromSelected
	"private -- answer true iff we may perform the #removeFromSelected command ?"

	^ self selectedListPresenter hasSelection.!

clear
	"Remove all contents in the receiver's model"

	self model removeAll.!

contentsOfDragSession: aSession
	"private -- answer the interesting Objects from aSession, if any"

	^ aSession dragObjects
		select: [:each | each isFormatAvailable: #Object]
		thenCollect: [:each | each object].!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (ListPresenter on: available) name: 'AvailableList';
		add: (ListPresenter on: selected) name: 'SelectedList';
		add: (TextPresenter on: 'Available') name: 'AvailableLabel';
		add: (TextPresenter on: 'Selected') name: 'SelectedLabel'.
!

createSchematicWiring
	"private -- arrange to listen to changes to our components"

	self availableListPresenter
		when: #actionPerformed send: #addToSelected to: self;
		when: #dragOver: send: #onDragOverAvailable: to: self;
		when: #drop: send: #onDropOverAvailable: to: self.

	self selectedListPresenter
		when: #actionPerformed send: #removeFromSelected to: self;
		when: #dragOver: send: #onDragOverSelected: to: self;
		when: #drop: send: #onDropOverSelected: to: self.

	^ super createSchematicWiring.
!

declineEventsFromModel
	"private -- arrange not to receive notifications from our model"

	self model removeEventsTriggeredFor: self.
!

defaultSortBlock
	"private -- answer a default sort block"

	^ SortedCollection defaultSortBlock.!

ensureSelectionVisible
	"null-op for compatability with SelectableItemsPresenter"!

getContentsBlock: a1Block
	"set the <monadicValuable> that will be used to describe the contents of our sublists"

	self listViewColumnsDo: [:each | each getContentsBlock: a1Block].!

getImageBlock: a1Block
	"set the <monadicValuable> that will be used to show images in the sublists"

	self listViewColumnsDo: [:each | each getImageBlock: a1Block].!

getTextBlock: a1Block
	"set the <monadicValuable> that will be used to describe the contents of our sublists"

	self listViewColumnsDo: [:each | each getTextBlock: a1Block].!

hasItemImages
	"answer whether our embedded lists show images"

	^ self availableListPresenter view hasColumnImages.!

hasItemImages: aBool
	"set whether our embedded lists show images (using the default getImageBlock or whatever
	may have been configured)"

	self listViewsDo: [:each | each hasColumnImages: aBool].!

hasSelection
	"answer true if we have a current selection"

	^ selected notEmpty.!

isSorted
	"answers whether we are imposing a sort order on our elements"

	^ sortblock notNil.!

list
	"answer the list from which we are making a selection"

	^ self model list.!

list: aCollection
	"set the list from which we are making a selection"

	| list |

	"preserve any existing sort constraint"
	list := sortblock isNil
			ifTrue: [aCollection]
			ifFalse: [aCollection asSortedCollection: sortblock].

	^ self model list: list.!

listViewColumnsDo: a1Block
	"evaluate a1Block for both of our embedded list views' principle columns
	(which is what we use to display stuff)"

	self listViewsDo: [:each | a1Block value: (each columnAtIndex: 1)].!

listViewsDo: a1Block
	"evaluate a1Block for both of our embedded list views"

	a1Block
		value: self availableListPresenter view;
		value: self selectedListPresenter view.
!

model: aListModel
	"set our model to be aListModel"

	self declineEventsFromModel.
	super model: aListModel.
	self solicitEventsFromModel.

	available := ListModel
			on: (OrderedCollection withAll: aListModel)
			searchPolicy: aListModel searchPolicy.
	selected := ListModel newWithSearchPolicy: aListModel searchPolicy.

	self availableListPresenter model: available.
	self selectedListPresenter model: selected.
!

moveSelectedDown
	"command -- move the selected items down in the list if possible.
	Removes any sorting of the selected list as a side effect"

	| indices last |

	self canMoveSelectedDown ifFalse: [^ self].

	indices := self selectedListPresenter view selectionsByIndex.
	indices := indices asSortedCollection asArray.

	self unsortSelectedList.	"NB: looses current selection"

	last := selected size.
	indices size to: 1 by: -1 do:
		[:i || index |
		index := indices at: i.
		index < last ifTrue:
			[selected swap: index with: index + 1.
			indices at: i put: index + 1].
		last := last - 1].

	self selectedListPresenter view selectionsByIndex: indices.

	self trigger: #selectionChanged.


!

moveSelectedFirst
	"command -- move the selected items to be first in the list.
	Removes any sorting of the selected list as a side effect"

	| indices |

	self canMoveSelectedFirst ifFalse: [^ self].

	indices := self selectedListPresenter view selectionsByIndex.
	indices := indices asSortedCollection asArray.

	self unsortSelectedList.	"NB: looses current selection"

	1 to: indices size do:
		[:to || from |
		from := indices at: to.
		from = to ifFalse:
			[| item |
			item := selected removeAtIndex: from.
			selected add: item beforeIndex: to]].

	self selectedListPresenter view selectionsByIndex: (1 to: indices size).

	self trigger: #selectionChanged.!

moveSelectedLast
	"command -- move the selected items to be last in the list.
	Removes any sorting of the selected list as a side effect"

	| indices to |

	self canMoveSelectedLast ifFalse: [^ self].

	indices := self selectedListPresenter view selectionsByIndex.
	indices := indices asSortedCollection asArray.

	self unsortSelectedList.	"NB: looses current selection"

	"God, but I hate this kind of code..."
	to := selected size.
	indices size to: 1 by: -1 do:
		[:i || from |
		from := indices at: i.
		from = to ifFalse:
			[| item |
			item := selected removeAtIndex: from.
			selected add: item beforeIndex: to].
		to := to - 1].

	self selectedListPresenter view selectionsByIndex: (to + 1 to: selected size).

	self trigger: #selectionChanged.!

moveSelectedUp
	"command -- move any selected items up if possible.
	Removes any sorting of the selected list as a side effect"

	| indices |
	self canMoveSelectedUp ifFalse: [^ self].

	indices := self selectedListPresenter view selectionsByIndex.
	indices := indices asSortedCollection asArray.

	self unsortSelectedList.	"NB: looses current selection"

	1 to: indices size do:
		[:i || index |
		index := indices at: i.
		index > i ifTrue:
			[selected swap: index with: index - 1.
			indices at: i put: index-1]].

	self selectedListPresenter view selectionsByIndex: indices.

	self trigger: #selectionChanged.
!

moveToAvailable: aList
	"private -- move the given collection of objects from selected to available and select them"

	selected removeAll: aList.
	available addAll: aList.

	self availableListPresenter selections: aList.

	self trigger: #selectionChanged.
!

moveToSelected: aList
	"private -- move the given collection of objects from available to selected and select them"

	available removeAll: aList.
	selected addAll: aList.

	self selectedListPresenter selections: aList.

	self trigger: #selectionChanged.
!

onDragOverAvailable: aSession
	"private -- a d&d session is passing over the available list.
	Update the session accordingly"

	"we only drag between our two panes, and then we always interpret it as a move"
	aSession dragSource = self selectedListPresenter view ifTrue:
		[aSession operation: #move].


!

onDragOverSelected: aSession
	"private -- a d&d session is passing over the selected list.
	Update the session accordingly"

	"we only drag between our two panes, and then we always interpret it as a move"
	aSession dragSource = self availableListPresenter view ifTrue:
		[aSession operation: #move].


!

onDropOverAvailable: aSession
	"private -- a d&d session is attempting to drop on the available list.
	Update the session accordingly"

	"we only drag between our two panes"
	(aSession dragSource = self selectedListPresenter view and: [aSession operation = #move])
		ifTrue: [self moveToAvailable: (self contentsOfDragSession: aSession)].

	aSession operation: nil.	"supress 'cut' that we've already done"
!

onDropOverSelected: aSession
	"private -- a d&d session is attempting to drop on the selected list.
	Update the session accordingly"

	"we only drag between our two panes"
	(aSession dragSource = self availableListPresenter view and: [aSession operation = #move])
		ifTrue: [self moveToSelected: (self contentsOfDragSession: aSession)].

	aSession operation: nil.	"supress 'cut' that we've already done"


!

onItem: anObject addedAtIndex: anInteger
	"private -- anObject has been added to our model, update the
	derived lists accordingly"

	available add: anObject.!

onItem: anObject removedAtIndex: anInteger
	"private -- anObject has been removed from our model, update the
	derived lists accordingly"

	available remove: anObject ifAbsent: [].
	selected remove: anObject ifAbsent: [^ self].

	self trigger: #selectionChanged.
!

onItem: anObject updatedAtIndex: anInteger
	"private -- anObject has been changed in our model, pass the
	notification to our derived list views"

	| index |

	index := available indexOf: anObject.
	index = 0 ifFalse: [available updateAtIndex: index].

	index := selected indexOf: anObject.
	index = 0 ifFalse: [selected updateAtIndex: index].

!

onListChanged
	"private -- our model's list has changed, reset ourselves to
	a base state"

	"this will preserve any ordering"
	self availableListPresenter list: (OrderedCollection withAll: self model).
	self selectedListPresenter list: (OrderedCollection new).

	self trigger: #selectionChanged.!

preserveModelOrder
	"apply a sort constraint that will keep items in the same order that they appear in the
	model.  This is quite expensive and so not the default"

	self beSorted: [:e1 :e2 | (model indexOf: e1) <= (model indexOf: e2)].!

queryCommand: aCommandQuery
	"private -- the usual..."

	| enabled |

	super queryCommand: aCommandQuery.
	enabled := aCommandQuery isEnabled.

	aCommandQuery command == #addToSelected ifTrue: [enabled := self canAddToSelected].
	aCommandQuery command == #removeFromSelected ifTrue: [enabled := self canRemoveFromSelected].

	aCommandQuery command == #moveSelectedFirst ifTrue: [enabled := self canMoveSelectedFirst].
	aCommandQuery command == #moveSelectedLast ifTrue: [enabled := self canMoveSelectedLast].
	aCommandQuery command == #moveSelectedUp ifTrue: [enabled := self canMoveSelectedUp].
	aCommandQuery command == #moveSelectedDown ifTrue: [enabled := self canMoveSelectedDown].

	aCommandQuery isEnabled: enabled.!

removeFromSelected
	"command -- move the selected items in 'selected' into 'available'"

	self canRemoveFromSelected ifFalse: [^ self].

	self moveToAvailable: self selectedListPresenter selections.
!

resetSelection
	"empty our current selection"

	self selections: #().!

selectedLabelPresenter
	"answer the TextPresenter that owns our 'Selected' label"

	^ self presenterNamed: 'SelectedLabel'.
!

selectedListPresenter
	"answer the ListPresenter that owns our 'Selected' list"

	^ self presenterNamed: 'SelectedList'.
!

selection
	"answer a, possibly empty, collection of the currently selected items.
	NB: this overloading of #selection is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selections.!

selection: aCollection
	"set the, possibly empty, collection of the currently selected items.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selections: aCollection.
!

selection: aCollection ifAbsent: a0or1Block
	"set the currently selected items from, possibly empty, collection, evaluating
	a0or1Block with the collection of items that are missing if any are.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selections: aCollection ifAbsent: a0or1Block.
!

selectionByIndex
	"answer a, possibly empty, collection of the indexes of any currently
	selected items.
	NB: this overloading of #selection is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selectionsByIndex.!

selectionByIndex: aCollection
	"set the curent selection from the, possibly empty, collection of indices.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selectionsByIndex: aCollection.
!

selectionByIndex: aCollection ifAbsent: a0or1Block
	"set the curent selection from the, possibly empty, collection of indices,
	evaluating a1Blockwith the collection of indices that are not in range if there
	are any such.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selectionsByIndex: aCollection ifAbsent: a0or1Block.
!

selectionByIndex: aCollection ifNone: a1Block
	"set the curent selection from the, possibly empty, collection of indices,
	evaluating a1Block with the collection of indices that are not in range if there
	are any such.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selectionsByIndex: aCollection ifAbsent: a1Block.
!

selectionByIndexIfNone: a0Block
	"answer a collection of the indexes of any currently
	selected items, or the result of evaluating a0Block if that collection
	is empty.
	NB: this overloading of #selection is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self hasSelection
		ifTrue: [self selectionByIndex]
		ifFalse: [a0Block value].!

selectionIfNone: a0Block
	"answer a collection of any currently selected items, or the result
	of evaluating a0Block if that collection is empty.
	NB: this overloading of #selection is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self hasSelection
		ifTrue: [self selection]
		ifFalse: [a0Block value].!

selectionOrNil
	"despit the name, this is the same as #selection.
	NB: this overloading of #selection is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	^ self selection.!

selectionOrNil: aCollectionOrNil
	"set the, possibly empty, collection of the currently selected items.
	NB: this overloading of #selection: is because we are strving for
	compatibility with a ListPresenter on a mutliple selection list, which
	behaves in this slightly odd fashion"

	self selections: (aCollectionOrNil ifNil: [#()]).

	^ aCollectionOrNil
!

selections
	"answer a list of the currently selected items in the order that they appear in
	the 'selected' pane.  Will be empty if there are no selections"

	^ OrderedCollection withAll: selected.!

selections: aCollection
	"set the current selected items to aCollection, raising an error for any
	items in aCollection that are not in our model"

	^ self
		selections: aCollection
		ifAbsent: [:them | self errorNotFound: them].
!

selections: aCollection ifAbsent: a0or1Block
	"set the current selected items to aCollection.  If any
	items in aCollection are not in our model then evaluate
	a0or1Block with the collection of extraneous items"

	| new |

	new := aCollection select: [:each | self model includes: each].

	self basicSelections: new.

	new size = aCollection size ifTrue: [^ aCollection].

	a0or1Block argumentCount = 1
		ifTrue: [a0or1Block value: (aCollection reject: [:each | self model includes: each])]
		ifFalse: [a0or1Block value].

	^ aCollection.!

selectionsByIndex
	"answer a collection of the current selected items indices in our model"

	^ self selections collect: [:each | self model indexOf: each].!

selectionsByIndex: aCollection
	"set the current selected items those indexed by aCollection,
	raising an error if any items are our of range"

	^ self
		selectionsByIndex: aCollection
		ifAbsent: [:them | self errorNotFound: them].
!

selectionsByIndex: aCollection ifAbsent: a0or1Block
	"set the current selected items those indexed by aCollection.
	If any indices in aCollection are our of range then evaluate
	a0or1Block with the collection of invalid entries"

	| size new |

	size := self model size.
	new := aCollection select: [:each | each between: 1 and: size].

	self basicSelections: (new collect: [:each | self model at: each]).

	new size = aCollection size ifTrue: [^ aCollection].

	a0or1Block argumentCount = 1
		ifTrue: [a0or1Block value: (aCollection reject: [:each | each between: 1 and: size])]
		ifFalse: [a0or1Block value].

	^ aCollection.!

solicitEventsFromModel
	"private -- arrange to get the notifications we need from our model"

	self model
		when: #listChanged send: #onListChanged to: self;
		when: #item:addedAtIndex: send: #onItem:addedAtIndex: to: self;
		when: #item:removedAtIndex: send: #onItem:removedAtIndex: to: self;
		when: #item:updatedAtIndex: send: #onItem:updatedAtIndex: to: self.
!

unsortSelectedList
	"private -- ensure that the selected collection is not sorted"

	| slp |

	slp := self selectedListPresenter.

	slp isSorted ifTrue: [slp beNotSorted].! !
!SelectionFromListPresenter categoriesFor: #addToSelected!commands!public! !
!SelectionFromListPresenter categoriesFor: #availableLabelPresenter!public!subpresenters! !
!SelectionFromListPresenter categoriesFor: #availableListPresenter!public!subpresenters! !
!SelectionFromListPresenter categoriesFor: #basicSelections:!helpers!private!selection! !
!SelectionFromListPresenter categoriesFor: #beNotSorted!public!sorting! !
!SelectionFromListPresenter categoriesFor: #beSorted!public!sorting! !
!SelectionFromListPresenter categoriesFor: #beSorted:!public!sorting! !
!SelectionFromListPresenter categoriesFor: #canAddToSelected!commands!private! !
!SelectionFromListPresenter categoriesFor: #canMoveSelectedDown!commands!private! !
!SelectionFromListPresenter categoriesFor: #canMoveSelectedFirst!commands!private! !
!SelectionFromListPresenter categoriesFor: #canMoveSelectedLast!commands!private! !
!SelectionFromListPresenter categoriesFor: #canMoveSelectedUp!commands!private! !
!SelectionFromListPresenter categoriesFor: #canRemoveFromSelected!commands!private! !
!SelectionFromListPresenter categoriesFor: #clear!accessing!public! !
!SelectionFromListPresenter categoriesFor: #contentsOfDragSession:!helpers!private! !
!SelectionFromListPresenter categoriesFor: #createComponents!initializing!private!subpresenters! !
!SelectionFromListPresenter categoriesFor: #createSchematicWiring!initializing!private! !
!SelectionFromListPresenter categoriesFor: #declineEventsFromModel!event handling!private! !
!SelectionFromListPresenter categoriesFor: #defaultSortBlock!constants!private!sorting! !
!SelectionFromListPresenter categoriesFor: #ensureSelectionVisible!public!selection! !
!SelectionFromListPresenter categoriesFor: #getContentsBlock:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #getImageBlock:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #getTextBlock:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #hasItemImages!public!testing! !
!SelectionFromListPresenter categoriesFor: #hasItemImages:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #hasSelection!public!selection! !
!SelectionFromListPresenter categoriesFor: #isSorted!public!sorting!testing! !
!SelectionFromListPresenter categoriesFor: #list!accessing!public! !
!SelectionFromListPresenter categoriesFor: #list:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #listViewColumnsDo:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #listViewsDo:!accessing!public! !
!SelectionFromListPresenter categoriesFor: #model:!initializing!models!public! !
!SelectionFromListPresenter categoriesFor: #moveSelectedDown!commands!public! !
!SelectionFromListPresenter categoriesFor: #moveSelectedFirst!commands!public! !
!SelectionFromListPresenter categoriesFor: #moveSelectedLast!commands!public! !
!SelectionFromListPresenter categoriesFor: #moveSelectedUp!commands!public! !
!SelectionFromListPresenter categoriesFor: #moveToAvailable:!helpers!private! !
!SelectionFromListPresenter categoriesFor: #moveToSelected:!helpers!private! !
!SelectionFromListPresenter categoriesFor: #onDragOverAvailable:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onDragOverSelected:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onDropOverAvailable:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onDropOverSelected:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onItem:addedAtIndex:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onItem:removedAtIndex:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onItem:updatedAtIndex:!event handling!private! !
!SelectionFromListPresenter categoriesFor: #onListChanged!event handling!private! !
!SelectionFromListPresenter categoriesFor: #preserveModelOrder!public!sorting! !
!SelectionFromListPresenter categoriesFor: #queryCommand:!commands!private! !
!SelectionFromListPresenter categoriesFor: #removeFromSelected!commands!public! !
!SelectionFromListPresenter categoriesFor: #resetSelection!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectedLabelPresenter!public!subpresenters! !
!SelectionFromListPresenter categoriesFor: #selectedListPresenter!public!subpresenters! !
!SelectionFromListPresenter categoriesFor: #selection!public!selection! !
!SelectionFromListPresenter categoriesFor: #selection:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selection:ifAbsent:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionByIndex!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionByIndex:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionByIndex:ifAbsent:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionByIndex:ifNone:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionByIndexIfNone:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionIfNone:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionOrNil!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionOrNil:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selections!public!selection! !
!SelectionFromListPresenter categoriesFor: #selections:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selections:ifAbsent:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionsByIndex!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionsByIndex:!public!selection! !
!SelectionFromListPresenter categoriesFor: #selectionsByIndex:ifAbsent:!public!selection! !
!SelectionFromListPresenter categoriesFor: #solicitEventsFromModel!event handling!private! !
!SelectionFromListPresenter categoriesFor: #unsortSelectedList!private!sorting! !

!SelectionFromListPresenter class methodsFor!

defaultModel
	"answer the model to use by default"

	^ ListPresenter defaultModel.!

icon

	^ ListPresenter icon.! !
!SelectionFromListPresenter class categoriesFor: #defaultModel!models!public! !
!SelectionFromListPresenter class categoriesFor: #icon!constants!public! !

ChoicesPrompter guid: (GUID fromString: '{F4337AAB-A4DE-4B46-A636-4ECE5E62B284}')!
ChoicesPrompter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Just replaces the default multiple-selection views with 2-list versions.


Examples (copied from superclass):

ChoicesPrompter multipleChoices: #[0 1 2 3] caption: ''Please choose''. "<-- display it"

model := (Array with: 2) asValue.
ChoicesPrompter on: model multipleChoices: #[0 1 2 3] caption: ''Please choose''.
model value "<-- display it"
'!
!ChoicesPrompter categoriesForClass!Unclassified! !
!ChoicesPrompter methodsFor!

beNotSorted
	"remove any sorting constriaint on our lists"

	choicePresenter beNotSorted.
!

beSorted
	"apply a default sorting constraint to our lists"

	choicePresenter beSorted.
!

beSorted: a2Block
	"set the <diadicValuable> that we use to enforce a sort order on our elements"

	choicePresenter beSorted: a2Block.!

choicesPresenterClass
	"private -- answer the class of Presenter we should
	use to show our list of choices"

	^ ChoicesPresenter.!

createComponents
	"create sub-presenters as needed"

	"NB: deliberately no supersend"

	choicePresenter := self
				add: self choicesPresenterClass new
				name: 'choices'.!

getTextBlock: a1Block
	"overriden to pass the <monadicValuable> to our choices presenter rather than
	blatting it directly into its view"

	choicePresenter getTextBlock: a1Block.!

isSorted
	"answers whether we are imposing a sort order on our elements"

	^ choicePresenter isSorted.!

preserveChoiceOrder
	"apply a sort constraint that will keep items in the same order that they appear in the
	choices.  This is quite expensive and so not the default"

	choicePresenter preserveChoiceOrder.! !
!ChoicesPrompter categoriesFor: #beNotSorted!public!sorting! !
!ChoicesPrompter categoriesFor: #beSorted!public!sorting! !
!ChoicesPrompter categoriesFor: #beSorted:!public!sorting! !
!ChoicesPrompter categoriesFor: #choicesPresenterClass!constants!private! !
!ChoicesPrompter categoriesFor: #createComponents!initializing!public! !
!ChoicesPrompter categoriesFor: #getTextBlock:!accessing!public! !
!ChoicesPrompter categoriesFor: #isSorted!public!sorting! !
!ChoicesPrompter categoriesFor: #preserveChoiceOrder!public!sorting! !

OrderedChoicesPrompter guid: (GUID fromString: '{CEDE3997-867D-4599-845F-2A2D212DB88F}')!
OrderedChoicesPrompter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Just replaces the default multiple-selection views with ordered 2-list versions.


Examples (copied from superclass):

OrderedChoicesPrompter multipleChoices: #[0 1 2 3] caption: ''Please choose''. "<-- display it"

model := (Array with: 2) asValue.
OrderedChoicesPrompter on: model multipleChoices: #[0 1 2 3] caption: ''Please choose''.
model value "<-- display it"
'!
!OrderedChoicesPrompter categoriesForClass!Unclassified! !
!OrderedChoicesPrompter methodsFor!

choicesPresenterClass
	"private -- answer the class of Presenter we should
	use to show our list of choices"

	^ OrderedChoicesPresenter.! !
!OrderedChoicesPrompter categoriesFor: #choicesPresenterClass!constants!private! !

ChoicesPresenter guid: (GUID fromString: '{C9B4A0BD-9CD7-4A63-9340-8915205D954F}')!
ChoicesPresenter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these uses two lists to represent the ''available'' and ''selected'' items, and allows the user to move items back and forth.

It is intended to be a plugin replacement for a ChoicePresenter.'!
!ChoicesPresenter categoriesForClass!Unclassified! !
!ChoicesPresenter methodsFor!

beNotSorted
	"remove any sorting constriaint on our lists"

	listPresenter beNotSorted.
!

beSorted
	"apply a default sorting constraint to our lists"

	listPresenter beSorted.
!

beSorted: a2Block
	"set the <diadicValuable> that we use to enforce a sort order on our elements"

	listPresenter beSorted: a2Block.!

choices: aCollection
	"set the list to choose from"

	self noEventsDo:
		[choices list: (OrderedCollection withAll: aCollection).
		self updateSelection].!

createComponents
	"create presenters in order that they may be bound into MVP triads"

	listPresenter := self add: (SelectionFromListPresenter on: choices) name: 'List'.
!

createSchematicWiring
	"arrange to listen to changes to our components"

	listPresenter when: #selectionChanged send: #onSelectionChanged to: self!

getTextBlock: a1Block
	"set the <monadicValuable>that we use to render our items"

	listPresenter getTextBlock: a1Block.
!

initialize
	"private -- establish a coherent initial state"

	choices := ListModel newEquality.
	nilChoice := #().
	isHandlingSelectionChanged := false.

	super initialize.
!

isSorted
	"answers whether we are imposing a sort order on our elements"

	^ listPresenter isSorted.!

model: aValueModel
	"set our model"

	self noEventsDo: [super model: aValueModel].
	self updateSelection.!

nilChoice: anObject
	"set the choice that will be used to represent nil.  By default this is #() -- no selection"

	nilChoice := anObject!

onSelectionChanged
	"private -- the selection has changed in our list, update our value accordingly"

	| choice newValue |

	choice := listPresenter selections.
	newValue := choice = nilChoice
				ifTrue: [nil]
				ifFalse: [choice].

	[isHandlingSelectionChanged := true. self value: newValue] ensure: [isHandlingSelectionChanged := false].!

onValueChanged
	"our value has changed, so we need to update our displayed list"

	self updateSelection.
	super onValueChanged
	!

preserveChoiceOrder
	"apply a sort constraint that will keep items in the same order that they appear in the
	choices.  This is quite expensive and so not the default"

	listPresenter preserveModelOrder.!

updateSelection
	"private -- our value has been changed, ensure that our list presenter reflects it"

	isHandlingSelectionChanged ifTrue: [^ self].

	listPresenter
		selections: (self value ifNil: [nilChoice])
		ifAbsent: ["may as well ignore errors"].! !
!ChoicesPresenter categoriesFor: #beNotSorted!public!sorting! !
!ChoicesPresenter categoriesFor: #beSorted!public!sorting! !
!ChoicesPresenter categoriesFor: #beSorted:!public!sorting! !
!ChoicesPresenter categoriesFor: #choices:!accessing!public! !
!ChoicesPresenter categoriesFor: #createComponents!initializing!public!subpresenters! !
!ChoicesPresenter categoriesFor: #createSchematicWiring!initializing!public! !
!ChoicesPresenter categoriesFor: #getTextBlock:!accessing!public! !
!ChoicesPresenter categoriesFor: #initialize!initializing!private! !
!ChoicesPresenter categoriesFor: #isSorted!public!sorting! !
!ChoicesPresenter categoriesFor: #model:!accessing!public! !
!ChoicesPresenter categoriesFor: #nilChoice:!accessing!public! !
!ChoicesPresenter categoriesFor: #onSelectionChanged!event handling!private! !
!ChoicesPresenter categoriesFor: #onValueChanged!event handling!public! !
!ChoicesPresenter categoriesFor: #preserveChoiceOrder!public!sorting! !
!ChoicesPresenter categoriesFor: #updateSelection!accessing!private! !

!ChoicesPresenter class methodsFor!

defaultModel
	"answer a model used by default"

	^ nil asValue comparisonPolicy: SearchPolicy equality.! !
!ChoicesPresenter class categoriesFor: #defaultModel!models!public! !

OrderedChoicesPresenter guid: (GUID fromString: '{7990E675-7B9E-4DAE-8ACC-EA9ED05FB551}')!
OrderedChoicesPresenter comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these uses two lists to represent the ''available'' and ''selected'' items, and allows the user to move items back and forth and up and down.

It is intended to be a plugin replacement for a ChoicePresenter.'!
!OrderedChoicesPresenter categoriesForClass!Unclassified! !
"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ChoicesPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAJsDAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAAAAAAAAAAAAcAAAAA
AAAAAAAAAAAAAACgAQAABgcMAEJvcmRlckxheW91dAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAAAA
AACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAANAAAAUmVmZXJlbmNlVmlld2IAAAAOAAAAAAAAAKAB
AABiAAAAAgAAAIIAAAAEAAAAAAAARAEAAgAwAgAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
ADACAAAGAhIAUmVzb3VyY2VJZGVudGlmaWVyAAAAAJoAAAAAAAAAUgAAACAAAABDVSBTZWxlY3Rp
b24gRnJvbSBMaXN0IFByZXNlbnRlclIAAAAaAAAAU2VsZWN0aW9uRnJvbUxpc3RQcmVzZW50ZXJS
AAAADAAAAERlZmF1bHQgdmlldwAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAA
AGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRl
bnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQAAAAEAAACCAwAAAAAAAL0CAAD1AQAAMAIAAAYBDwBX
SU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAA
AAAAAABeAQAA+gAAAGIAAAAAAAAAggMAAAAAAADBAAAAwQAAAAAAAAAVAAAA6gAAAAAAAAAAAQAA
YgAAAAIAAAAwAgAAUgAAAAQAAABMaXN0AAAAAPICAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAAy
AwAAAAAAAFADAABiAAAAAgAAAIIDAAAAAAAACwAAAAsAAACCAwAAAAAAAL0CAAD1AQAAoAEAALID
AAAAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUAAABjAQAA/wAA
AMoAAAAAAAAA0AAAAGIAAAABAAAAMAIAAPADAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAA
EAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAY
AAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAA
AENvbnRhaW5lclZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAA
UgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: ChoicesPrompter name: 'Extensible multi-selection choice prompter') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAAAY2FuY2VsUgAAAAYAAABDYW5jZWwBAAAAAQAAAAAAAAAAAAAAAQAAANIDAAAAAAAAygAAAAAA
AADQAAAAYgAAAAIAAAASBAAAAAAAADAEAABiAAAAAgAAACICAAAAAAAAuwMAABUAAAAiAgAAAAAA
AI0AAAAzAAAAwAcAABIEAAAAAAAAkAQAAGIAAAABAAAAUgAAAAYAAABDYW5jZWzABwAA0gQAAAAA
AAByAAAALAAAACwAAAAAAAAAAQAAAP/////////////////////dAQAACgAAACMCAAAjAAAAygAA
AAAAAADQAAAAEAUAACAFAAAAAAAAEwAAADIFAAAAAAAAUAUAAHX///9wBQAAAQAAAJAFAADP////
sAUAAAEAAADqAAAAAAAAAAABAAAQBQAABgIJAFJlY3RhbmdsZQAAAAAiAgAAAAAAAAEAAAABAAAA
IgIAAAAAAAABAAAAAQAAANIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAASBAAAAAAAADAEAABi
AAAAAgAAACICAAAAAAAADwAAADsCAAAiAgAAAAAAAEcEAABHAAAAYAIAANIEAAAAAAAAcgAAACwA
AAAsAAAAAAAAAAEAAAD/////////////////////BwAAAB0BAAAqAgAAQAEAAMoAAAAAAAAA0AAA
AGIAAAADAAAA0AUAABADAADABwAAIAUAAAAAAAATAAAAAAAAAAAAAACaAQAAAAAAAJoAAAAAAAAA
wAEAAFIAAAANAAAAUmVmZXJlbmNlVmlld2IAAAAOAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAA
AAAARAEAAgAwCgAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAADAKAAAGAhIAUmVzb3VyY2VJ
ZGVudGlmaWVyAAAAAJoAAAAAAAAAUgAAACAAAABDVSBTZWxlY3Rpb24gRnJvbSBMaXN0IFByZXNl
bnRlclIAAAAQAAAAQ2hvaWNlc1ByZXNlbnRlclIAAAAMAAAARGVmYXVsdCB2aWV3AAAAANIDAAAA
AAAAygAAAAAAAADQAAAAYgAAAAEAAAASBAAAAAAAADAEAABiAAAAAgAAACICAAAAAAAADwAAAA8A
AAAiAgAAAAAAAEcEAAAtAgAAMAoAANIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////
////////////BwAAAAcAAAAqAgAAHQEAAGIAAAAAAAAAIAUAAAAAAAAVAAAA6gAAAAAAAAAAAQAA
YgAAAAIAAAAwCgAAUgAAAAcAAABjaG9pY2VzQgkAAAAAAAAiAgAAAAAAAA8AAAAPAAAAIgIAAAAA
AAARAAAADwAAAAAAAAAAAAAAAAAAAAAAAABpbgAAAAAAAAAAAAAAAAAAIgIAAAAAAAD1AQAANwEA
AAEAAAAAAAAAAAAAAAYDCQBTZW1hcGhvcmUAAAAAAAAAAAAAAAABAAAAAAAAAIIAAAAEAAAAQrfV
d9IDAAAAAAAAygAAAAAAAADQAAAAYgAAAAMAAAASBAAAAAAAADAEAABiAAAAAgAAACICAAAAAAAA
CwAAAAsAAAAiAgAAAAAAAHUEAADHAgAAoAEAABIEAAAAAAAAkAQAAGIAAAABAAAAUgAAAB4AAABT
ZWxlY3QgZnJvbSBhdmFpbGFibGUgY2hvaWNlczqgAQAAEgQAAAAAAAC6AAAAAAAAAFIAAAAIAAAA
bWVudUJhcjpiAAAAAQAAAAAAAACgAQAA0gQAAAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP//////
//////////////8FAAAABQAAAD8CAABoAQAAygAAAAAAAADQAAAAYgAAAAIAAAAwCgAAYAIAACAF
AAAAAAAAFQAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAA
AACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9y
ugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAADgAAAERpYWxvZ1ZpZXcuaWNvDgIfAFNUQkV4dGVy
bmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: ChoicesPrompter name: 'Multi-selection choice prompter') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAPwFAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACgAAAERpYWxvZ1ZpZXdiAAAAHgAA
AAAAAAAAAAAAYgAAAAIAAAABAJgBAQACAKABAAAAAAAABgELAFN5c3RlbUNvbG9yAAAAAB8AAAAG
AgUAUG9pbnQAAAAA9QEAAL0CAACHAAAAAAAAAAAAAAAAAAAAoAEAAAYHDABCb3JkZXJMYXlvdXQA
AAAAAQAAAAEAAAAAAAAAmgEAAAAAAACaAAAAAAAAAMABAABSAAAADQAAAFJlZmVyZW5jZVZpZXdi
AAAADgAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAYAIAAAAAAAAAAAAAAAAAAAcA
AAAAAAAAAAAAAAAAAABgAgAABgISAFJlc291cmNlSWRlbnRpZmllcgAAAACaAAAAAAAAAMABAABS
AAAACQAAAFByZXNlbnRlclIAAAAWAAAAT0sgQ2FuY2VsIGJ1dHRvbiBibG9jawAAAAAGAQ8ATWVz
c2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoA
AAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRlbnQ6YgAAAAIAAAAiAgAAAAAAAA8AAAA7AgAAIgIA
AAAAAAAVBAAARwAAAGACAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAB
AAAA/////////////////////wcAAAAdAQAAEQIAAEABAABiAAAAAAAAACICAAAAAAAAwQAAAMEA
AAAAAAAAFQAAAAAAAAAAAAAAmgEAAAAAAABwAgAAYgAAAA4AAAAAAAAAoAEAAGIAAAACAAAAggAA
AAQAAAAAAABEAQACABAEAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAEAQAAMICAAAAAAAA
mgAAAAAAAABSAAAAIAAAAENVIFNlbGVjdGlvbiBGcm9tIExpc3QgUHJlc2VudGVyUgAAABAAAABD
aG9pY2VzUHJlc2VudGVyUgAAAAwAAABEZWZhdWx0IHZpZXcAAAAAEgMAAAAAAADKAAAAAAAAANAA
AABiAAAAAQAAAFIDAAAAAAAAcAMAAGIAAAACAAAAIgIAAAAAAAAPAAAADwAAACICAAAAAAAAFQQA
AC0CAAAQBAAAwgMAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8HAAAA
BwAAABECAAAdAQAA8AMAAAAEAAAAAAAAFQAAAOoAAAAAAAAAAAEAAGIAAAACAAAAEAQAAFIAAAAH
AAAAY2hvaWNlcwYCCQBSZWN0YW5nbGUAAAAAIgIAAAAAAAAPAAAADwAAACICAAAAAAAAEQAAAA8A
AAAAAAAAAAAAAAAAAAAAAAAAwW4AAAAAAAAAAAAAAAAAACICAAAAAAAA9QEAADcBAAABAAAAAAAA
AAAAAAAGAwkAU2VtYXBob3JlAAAAAAAAAAAAAAAAAQAAAAAAAACCAAAABAAAAEK31XcSAwAAAAAA
AMoAAAAAAAAA0AAAAGIAAAADAAAAUgMAAAAAAABwAwAAYgAAAAIAAAAiAgAAAAAAAAsAAAALAAAA
IgIAAAAAAABDBAAAxwIAAKABAABSAwAAAAAAALoAAAAAAAAAUgAAAAUAAAB0ZXh0OmIAAAABAAAA
UgAAAB4AAABTZWxlY3QgZnJvbSBhdmFpbGFibGUgY2hvaWNlczqgAQAAUgMAAAAAAAC6AAAAAAAA
AFIAAAAIAAAAbWVudUJhcjpiAAAAAQAAAAAAAACgAQAAwgMAAAAAAAByAAAALAAAACwAAAAAAAAA
AAAAAP////////////////////8FAAAABQAAACYCAABoAQAAygAAAAAAAADQAAAAYgAAAAIAAAAQ
BAAAYAIAAAAEAAAAAAAAFQAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0
b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZp
bGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAADgAAAERpYWxvZ1ZpZXcuaWNvDgIf
AFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUu
ZGxsAAAAAA=='))!

(ResourceIdentifier class: OrderedChoicesPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAJsDAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAAAAAAAAAAAAcAAAAA
AAAAAAAAAAAAAACgAQAABgcMAEJvcmRlckxheW91dAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAAAA
AACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAANAAAAUmVmZXJlbmNlVmlld2IAAAAOAAAAAAAAAKAB
AABiAAAAAgAAAIIAAAAEAAAAAAAARAEAAgAwAgAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
ADACAAAGAhIAUmVzb3VyY2VJZGVudGlmaWVyAAAAAJoAAAAAAAAAUgAAACAAAABDVSBTZWxlY3Rp
b24gRnJvbSBMaXN0IFByZXNlbnRlclIAAAAaAAAAU2VsZWN0aW9uRnJvbUxpc3RQcmVzZW50ZXJS
AAAADAAAAE9yZGVyZWQgdmlldwAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAA
AGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoAAAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRl
bnQ6YgAAAAIAAAAGAgUAUG9pbnQAAAAAAQAAAAEAAACCAwAAAAAAAL0CAAD1AQAAMAIAAAYBDwBX
SU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAA
AAAAAABeAQAA+gAAAGIAAAAAAAAAggMAAAAAAADBAAAAwQAAAAAAAAAVAAAA6gAAAAAAAAAAAQAA
YgAAAAIAAAAwAgAAUgAAAAQAAABMaXN0AAAAAPICAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAAy
AwAAAAAAAFADAABiAAAAAgAAAIIDAAAAAAAACwAAAAsAAACCAwAAAAAAAL0CAAD1AQAAoAEAALID
AAAAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUAAABjAQAA/wAA
AMoAAAAAAAAA0AAAAGIAAAABAAAAMAIAAPADAAAAAAAAEwAAAEYFBAADAAAASWNvbgAAAAAAAAAA
EAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhpblIAAAAY
AAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRSAAAAEQAA
AENvbnRhaW5lclZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJveHkAAAAA
UgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: OrderedChoicesPrompter name: 'Extensible multi-selection choice prompter') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
/////////////////////wcAAAAHAAAAKgIAAB0BAABiAAAAAAAAACAFAAAAAAAAFQAAAOoAAAAA
AAAAAAEAAGIAAAACAAAAMAoAAFIAAAAHAAAAY2hvaWNlc0IJAAAAAAAAIgIAAAAAAAAPAAAADwAA
ACICAAAAAAAAEQAAAA8AAAAAAAAAAAAAAAAAAAAAAAAAiW8AAAAAAAAAAAAAAAAAACICAAAAAAAA
9QEAADcBAAABAAAAAAAAAAAAAAAGAwkAU2VtYXBob3JlAAAAAAAAAAAAAAAAAQAAAAAAAACCAAAA
BAAAAEK31XfSAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAADAAAAEgQAAAAAAAAwBAAAYgAAAAIAAAAi
AgAAAAAAAAsAAAALAAAAIgIAAAAAAAB1BAAAxwIAAKABAAASBAAAAAAAAJAEAABiAAAAAQAAAFIA
AAAeAAAAU2VsZWN0IGZyb20gYXZhaWxhYmxlIGNob2ljZXM6oAEAABIEAAAAAAAAugAAAAAAAABS
AAAACAAAAG1lbnVCYXI6YgAAAAEAAAAAAAAAoAEAANIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAAA
AAD/////////////////////BQAAAAUAAAA/AgAAaAEAAMoAAAAAAAAA0AAAAGIAAAACAAAAMAoA
AGACAAAgBQAAAAAAABUAAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9u
UHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxl
TG9jYXRvcroAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAAAA4AAABEaWFsb2dWaWV3Lmljbw4CHwBT
VEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRs
bAAAAAA='))!

(ResourceIdentifier class: OrderedChoicesPrompter name: 'Multi-selection choice prompter') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAAMGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACgAAAERpYWxvZ1ZpZXdiAAAAHgAA
AAAAAAAAAAAAYgAAAAIAAAABAJgBAQACAKABAAAAAAAABgELAFN5c3RlbUNvbG9yAAAAAB8AAAAG
AgUAUG9pbnQAAAAA9QEAAL0CAACHAAAAAAAAAAAAAAAAAAAAoAEAAAYHDABCb3JkZXJMYXlvdXQA
AAAAAQAAAAEAAAAAAAAAmgEAAAAAAACaAAAAAAAAAMABAABSAAAADQAAAFJlZmVyZW5jZVZpZXdi
AAAADgAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAYAIAAAAAAAAAAAAAAAAAAAcA
AAAAAAAAAAAAAAAAAABgAgAABgISAFJlc291cmNlSWRlbnRpZmllcgAAAACaAAAAAAAAAMABAABS
AAAACQAAAFByZXNlbnRlclIAAAAWAAAAT0sgQ2FuY2VsIGJ1dHRvbiBibG9jawAAAAAGAQ8ATWVz
c2FnZVNlcXVlbmNlAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAABgMLAE1lc3NhZ2VTZW5kAAAAALoA
AAAAAAAAUgAAABAAAABjcmVhdGVBdDpleHRlbnQ6YgAAAAIAAAAiAgAAAAAAAA8AAAA7AgAAIgIA
AAAAAAAVBAAARwAAAGACAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAB
AAAA/////////////////////wcAAAAdAQAAEQIAAEABAABiAAAAAAAAACICAAAAAAAAwQAAAMEA
AAAAAAAAFQAAAAAAAAAAAAAAmgEAAAAAAABwAgAAYgAAAA4AAAAAAAAAoAEAAGIAAAACAAAAggAA
AAQAAAAAAABEAQACABAEAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAEAQAAMICAAAAAAAA
mgAAAAAAAABSAAAAIAAAAENVIFNlbGVjdGlvbiBGcm9tIExpc3QgUHJlc2VudGVyUgAAABcAAABP
cmRlcmVkQ2hvaWNlc1ByZXNlbnRlclIAAAAMAAAARGVmYXVsdCB2aWV3AAAAABIDAAAAAAAAygAA
AAAAAADQAAAAYgAAAAEAAABSAwAAAAAAAHADAABiAAAAAgAAACICAAAAAAAADwAAAA8AAAAiAgAA
AAAAABUEAAAtAgAAEAQAAMIDAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////
////BwAAAAcAAAARAgAAHQEAAPADAAAABAAAAAAAABUAAADqAAAAAAAAAAABAABiAAAAAgAAABAE
AABSAAAABwAAAGNob2ljZXMGAgkAUmVjdGFuZ2xlAAAAACICAAAAAAAADwAAAA8AAAAiAgAAAAAA
ABEAAAAPAAAAAAAAAAAAAAAAAAAAAAAAACVvAAAAAAAAAAAAAAAAAAAiAgAAAAAAAPUBAAA3AQAA
AQAAAAAAAAAAAAAABgMJAFNlbWFwaG9yZQAAAAAAAAAAAAAAAAEAAAAAAAAAggAAAAQAAABCt9V3
EgMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAAFIDAAAAAAAAcAMAAGIAAAACAAAAIgIAAAAAAAAL
AAAACwAAACICAAAAAAAAQwQAAMcCAACgAQAAUgMAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4dDpi
AAAAAQAAAFIAAAAeAAAAU2VsZWN0IGZyb20gYXZhaWxhYmxlIGNob2ljZXM6oAEAAFIDAAAAAAAA
ugAAAAAAAABSAAAACAAAAG1lbnVCYXI6YgAAAAEAAAAAAAAAoAEAAMIDAAAAAAAAcgAAACwAAAAs
AAAAAAAAAAAAAAD/////////////////////BQAAAAUAAAAmAgAAaAEAAMoAAAAAAAAA0AAAAGIA
AAACAAAAEAQAAGACAAAABAAAAAAAABUAAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RC
U2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVs
YXRpdmVGaWxlTG9jYXRvcroAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAAAA4AAABEaWFsb2dWaWV3
Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhp
bmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: SelectionFromListPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAAXAAAA6gAAAAAAAAAAAQAAYgAAAAQAAABACgAAUgAAAA4AAABBdmFpbGFibGVMYWJlbKALAABS
AAAADQAAAEF2YWlsYWJsZUxpc3QAAAAA4gMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAACIEAAAA
AAAAQAQAAGIAAAACAAAAwgIAAAAAAAABAAAAAQAAAMICAAAAAAAApQEAAEkDAADgCQAAIgUAAAAA
AAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAAAAAANIAAACkAQAAygAA
AAAAAADQAAAAYgAAAAIAAABACgAAoAsAAHAFAAAAAAAAEwAAAHACAACaAQAAAAAAALABAABiAAAA
DwAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIA0A8AAAAAAAAAAAAAAAAAAAcAAAAA
AAAAAAAAAAAAAADQDwAAIgoAAAAAAAABAAAAAQAAAJoBAAAAAAAAUAoAAGIAAAAQAAAAAAAAANAP
AABiAAAAAgAAAIIAAAAEAAAAAQEARAEAAAAgEAAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
ACAQAAAAAAAAggAAAAgAAAAzA///AAAAALIKAAAAAAAAAAAAAAAAAAAAAAAA4gMAAAAAAADKAAAA
AAAAANAAAABiAAAAAgAAACIEAAAAAAAAQAQAAGIAAAACAAAAwgIAAAAAAAABAAAAAQAAAMICAAAA
AAAApQEAACsAAAAgEAAAIgQAAAAAAADgBAAAYgAAAAEAAABSAAAACwAAACZTZWxlY3RlZA0KIBAA
ACIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAAAAAAAADSAAAA
FQAAAMoAAAAAAAAA0AAAAGAFAABwBQAAAAAAABMAAAAAAAAAAAAAAAAAAACaAQAAAAAAALALAABi
AAAAHgAAAAAAAADQDwAAYgAAAAIAAACCAAAABAAAAElQAUQBBAAAUBEAABIMAAAAAAAAygAAAAAA
AADQAAAAYAUAAAAAAABQDAAAAAAAAAAAAAAfAAAAAAAAAAAAAAAAAAAAUBEAAAAAAACCAAAACAAA
ADkA//8AAAAAwAwAAAAAAADgDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAygAAAAAAAADQAAAA
YgAAAAEAAABSDQAAAAAAAFIAAAAIAAAAQ29sdW1uIDGdAQAAgA0AAMAMAACgDQAAAAAAAAAAAABQ
EQAAwA0AAAMAAAAAAAAAAAAAAOANAAAADgAAAAAAAGEAAAAAAAAAAAAAAOIDAAAAAAAAygAAAAAA
AADQAAAAYgAAAAIAAAAiBAAAAAAAAEAEAABiAAAAAgAAAMICAAAAAAAAAQAAACsAAADCAgAAAAAA
AKUBAAAfAwAAUBEAACIEAAAAAAAA4AQAAGIAAAABAAAAUgAAAAgAAABDb2x1bW4gMVARAAAiBQAA
AAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAVAAAA0gAAAKQBAADK
AAAAAAAAANAAAABgBQAAcAUAAAAAAAAXAAAA6gAAAAAAAAAAAQAAYgAAAAQAAAAgEAAAUgAAAA0A
AABTZWxlY3RlZExhYmVsUBEAAFIAAAAMAAAAU2VsZWN0ZWRMaXN0AAAAAOIDAAAAAAAAygAAAAAA
AADQAAAAYgAAAAEAAAAiBAAAAAAAAEAEAABiAAAAAgAAAMICAAAAAAAAWQIAAAEAAADCAgAAAAAA
AKUBAABJAwAA0A8AACIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////
LAEAAAAAAAD+AQAApAEAAMoAAAAAAAAA0AAAAGIAAAACAAAAIBAAAFARAABwBQAAAAAAABMAAABw
BQAAAAAAABMAAABGBQQAAwAAAEljb24AAAAAAAAAABAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkA
AAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAAGAAAAEltYWdlUmVsYXRpdmVGaWxlTG9jYXRv
croAAAAAAAAAUgAAAAcAAABjdXJyZW50UgAAABEAAABDb250YWluZXJWaWV3Lmljbw4CHwBTVEJF
eHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAA
AAA='))!

(ResourceIdentifier class: SelectionFromListPresenter name: 'Ordered view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
TW92ZSAmbGFzdAEAAAABAAAAAAAAAAAAAAABAAAAMgMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAA
AHIDAAAAAAAAkAMAAGIAAAACAAAAwgMAAAAAAABJAwAAIwEAAMIDAAAAAAAAoQAAADMAAACQCgAA
cgMAAAAAAAAABAAAYgAAAAEAAAAgAAAAkAoAAHIDAAAAAAAAQAQAAGIAAAABAAAAUgAAAAoAAABN
b3ZlICZsYXN0kAoAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////
pAEAAJEAAAD0AQAAqgAAAMoAAAAAAAAA0AAAAMAEAADQBAAAAAAAABMAAADiBAAAAAAAAMAIAAAB
AAAAAAUAAAEAAADgCAAAFQAAAGAFAAAzAAAA6gAAAAAAAAAAAQAAYgAAAAgAAAAACQAAUgAAABYA
AABNb3ZlU2VsZWN0aW9uc1VwQnV0dG9ukAoAAFIAAAAYAAAATW92ZVNlbGVjdGlvbnNMYXN0QnV0
dG9uMAcAAFIAAAAYAAAATW92ZVNlbGVjdGlvbnNEb3duQnV0dG9ucAIAAFIAAAAZAAAATW92ZVNl
bGVjdGlvbnNGaXJzdEJ1dHRvbgAAAAAyAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAAcgMAAAAA
AACQAwAAYgAAAAIAAADCAwAAAAAAAAsAAAALAAAAwgMAAAAAAAD9AwAASQMAAKABAACCBAAAAAAA
AHIAAAAsAAAALAAAAAAAAAAAAAAA/////////////////////wUAAAAFAAAAAwIAAKkBAADKAAAA
AAAAANAAAABiAAAABQAAAIAFAABwAgAAAAkAADAHAACQCgAA0AQAAAAAAAATAAAARgUEAAMAAABJ
Y29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABE
b2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3Vy
cmVudFIAAAARAAAAQ29udGFpbmVyVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJh
cnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

