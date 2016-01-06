| package |
package := Package name: 'CU Derived List Model'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A DerivedListModel is a "view" (in the database sense) of another ListModel. It provides filtering and sorting independently of its subject.  See the DerivedListModel class comment for more information.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package classNames
	add: #DerivedListModel;
	add: #DerivedListPresenter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

ListModel subclass: #DerivedListModel
	instanceVariableNames: 'underlyingList filterBlock sortBlock isIgnoringEvents'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ListPresenter subclass: #DerivedListPresenter
	instanceVariableNames: 'filterBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DerivedListModel guid: (GUID fromString: '{6CC3E3A1-D942-4EF8-B100-E990E2A4ED68}')!
DerivedListModel comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A DerivedListModel is a ''view'' (in the database sense) of another ListModel, mediated by a filter block, and allowing its elements to be sorted without affecting the underlying ListModel.

We arrange to watch for changes to the list.  The idea is that you can create several instances all of which are ''on'' the same llist model, and so any change to the contents of one will be reflected in the others.

NB: the #list: method changes the underlying list, whereas the #list method retrieves the filtered and sorted view.  If that''s a problem, well, sorry...
NB: the default ListPresenter "knows" that it should sort lists (e.g. by columns) using #list:, which would bugger up a DerivedListModel, so this package includes a fixed DerviedListPresenter that can be used in preference.

Some of this code is based on the implementation of a similar concept by Dmitry Zamotkin, that was posted to comp.lang.smalltalk.dolphin on 2001-07-09.

Extended example:
	
	"set up the base list, and open a list presenter to show it"
	base := ListModel on: #(1 2 3 4 5 6 5 4 3 2 1) asOrderedCollection.
	(ListPresenter showOn: base) topShell caption: ''Base''.
	
	"open a couple of lists derived from the same base.  Note the use of DervedListPresenter to show a DerivedListModel"
	odds := DerivedListModel on: base filterBlock: [:each | each odd].
	(DerivedListPresenter show: ''Enhanced list view'' on: odds) topShell caption: ''Odds''.
	evens := DerivedListModel on: base filterBlock: [:each | each even].
	(DerivedListPresenter show: ''Enhanced list view'' on: evens) topShell caption: ''Evens''.
	
	"try messing with updating base directly, feel free to change the sorting of the two
	derrived lists"
	base list: #(11 22 33 44 55 66 77 88 77 66 55 44 33 22 11) asOrderedCollection.
	base add: 100.
	base add: 999.
	base remove: 100.
	base remove: 999.
	"these will throw errrors if the base list is sorted"
	base at: 2 put: 11111.
	base at: 1 put: 22222.
	base addLast: 1000000.
	
	"try messng with the filter blocks"
	odds filterBlock: [:each | each < 50].
	evens filterBlock: [:each | each > 100].
	odds filterBlock: nil.
	evens filterBlock: [:each | each even].
	odds filterBlock: [:each | each odd].
	
	"try messing with the derived lists"
	evens remove: 22.
	evens add: 22.
	evens remove: 11111.	"should throw error"
	odds remove: 11111.	"should be OK"
	evens add: 11111.		"note that this works !!"
	evens removeAll.		"leaves just the odds"
	
	"more messing. Will cause errors if the derived collections are sorted"
	evens addLast: 22.
	odds at: 1 put: 0.
	odds addFirst: -1000.
	
	"something to watch out for"
	evens list: (1 to: 10) asOrderedCollection.

'!
!DerivedListModel categoriesForClass!Unclassified! !
!DerivedListModel methodsFor!

add: anObject afterIndex: anIndex
	"add the given object immediately after the given index (which may be 0).
	NB: this will add the item to an arbitrary position in the underlying collection, and will
	only add it to this view if the object satisfies our sort block (if not then no
	#itemAdded: events will be generated)"

	"better to check index before adding or we'll risk inconsistancy"
	(anIndex between: 0 and: self size) ifFalse: [^ self errorSubscriptBounds: anIndex].

	"similarly have to make sure we're not sorted upfront"
	self isSorted ifTrue: [self list shouldNotImplement].

	self ignoringEventsDo: [underlyingList add: anObject].

	^ (self wouldInclude: anObject)
		ifTrue: [super add: anObject afterIndex: anIndex]
		ifFalse: [anObject].!

addAnsweringIndex: anObject
	"add anObject to our list, answer the index at which it was added according to
	our sort block.
	NB: if the item does not pass our filter block, then it wll not be added so this
	method can do nothing except answer 0.  Avoid such situations in contexts
	where the caller is not prepared to deal with an out-of-index response"

	"we use addAnsweringIndex:, even though we ignore the answered index,
	since that can have slightly different semantics from other forms of #add"
	self ignoringEventsDo: [underlyingList addAnsweringIndex: anObject].

	^ (self wouldInclude: anObject)
		ifTrue: [super addAnsweringIndex: anObject]
		ifFalse: [0].
!

at: anIndex put: anObject
	"replace the existing entry at the given index in our *view*.
	NB1: it tricky to see exactly *what* this means, given that we are a view.
	What we do is find the first identical item in our underlying view and
	replace that, and then replace the index item in our list.
	NB2: if anObject doesn't pass our filter, then anObject will not appear in
	this list, so we will apear to have shrunk by one element"

	| old oldIsAt |

	"better to check index before adding or we'll risk inconsistancy"
	(anIndex between: 1 and: self size) ifFalse: [^ self errorSubscriptBounds: anIndex].

	"similarly have to make sure we're not sorted upfront"
	self isSorted ifTrue: [self list shouldNotImplement].

	old := self at: anIndex.
	oldIsAt := underlyingList identityIndexOf: old.	"this should only fail if we're in an inconsistant state somehow"
	self ignoringEventsDo:
		[underlyingList
			remove: old ifAbsent:[];
			add: anObject].

	(self wouldInclude: anObject)
		ifTrue: [super at: anIndex put: anObject]
		ifFalse: [super removeAtIndex: anIndex].

	^ anObject.!

beSorted
	"set ourself to use a default sort block"

	self sortBlock: SortedCollection defaultSortBlock.
!

beSorted: a2Block
	"set ourself to use the given sort block"

	self sortBlock: a2Block.
!

beUnsorted
	"set ourself to use no sorting.
	NB: unlike the case of setting a nil #sortBlock, this will /not/ cause us to
	revert to the underlying sort order"

	self isSorted ifTrue:
		[sortBlock := nil.
		list := OrderedCollection withAll: list].!

declineEvents
	"private -- unhook ourself from any events generated by our underlying
	ListModel"

	underlyingList removeEventsTriggeredFor: self.!

filterBlock
	"answer our filter block or nil if we are not filtering"

	^ filterBlock.
!

filterBlock: a1Block
	"set our filter block to the given <monadicValuable> or nil to turn off
	filtering"

	filterBlock == a1Block ifTrue: [^ self].

	self underlyingList: underlyingList filterBlock: a1Block.
!

ignoringEventsDo: a0Block
	"private -- answer the result of evaluating a0Block in an environment where
	we don't listen for changes to our underlying list"

	| wasIgnoringEvents |

	"NB: we can't just use #noEventsDo: on the underlying ListModel since it
	may well have other Observers"
	wasIgnoringEvents := isIgnoringEvents.
	isIgnoringEvents := true.
	^ a0Block ensure: [isIgnoringEvents := wasIgnoringEvents].
	!

initialize
	"private -- establish a coherent initial state"

	isIgnoringEvents := false.

	^ super initialize.!

isFiltered
	"answer whether we are currently presenting a filtered view"

	^ filterBlock notNil.!

isSorted
	"answer whether we are currently presenting a sorted view"

	^ sortBlock notNil.!

list: aSequenceableCollection
	"despite this name, this does *not* set the list of items that constitute this view, instead
	we have to treat it as a request to change the underlying list in order to keep some kind
	of compatability with ListModel"

	underlyingList list: aSequenceableCollection.!

onItem: anObject addedAtIndex: anIndex
	"private -- a new item has been added to our underlying collection.
	Add it also to our derived list.
	There's not a lot we can do to preserve the index, since that will
	be meaningless unless we have no sort block or folter block (which
	is presumably unlikely -- or what are we for at all ?)"

	"we ignore notifications that result from our own modifications to the underlying list"
	isIgnoringEvents ifTrue: [^ self].

	(self wouldInclude: anObject) ifFalse: [^ self].

	"have to use #addAnsweringIndex: because super #add: will call our own #addAnsweringIndex:..."
	super addAnsweringIndex: anObject.!

onItem: anObject removedAtIndex: anIndex
	"private -- an existing item has been removed from our underlying collection.
	Also remove it from our derived list"

	"we ignore notifications that result from our own modifications to the underlying list"
	isIgnoringEvents ifTrue: [^ self].

	(self wouldInclude: anObject) ifFalse: [^ self].

	"have to call super #removeAtIndex: because #remove: would just end up back in our own implementation"
	super removeAtIndex: (self indexOf: anObject ifAbsent: [^ self])
!

onItem: anObject updatedAtIndex: anIndex
	"private -- the given item in our underlying collection has changed or been replaced.
	Pass the notification on to our derived list"

	| existing killIndex |

	"we ignore notifications that result from our own modifications to the underlying list"
	isIgnoringEvents ifTrue: [^ self].

	"unfortunately the notification doesn't tell us *which* object (if any) has been replaced, so
	we'd have to do a complete scan to ensure that the derived view only contained objects that
	were still in the underlying view.  But we'd have to be carefull about muiltiple occurences (both
	before and after), and in the end it's just too miuch work to get right, so we wimp out..."
	self recomputeView.!

onListChanged
	"private -- our underlying collection has changed somehow.
	rebuild everything"

	self recomputeView.!

postCopy
	"called after we have been copied.  Overriden to ensure
	we have a copy of the underlyingList"

	super postCopy.
	underlyingList := underlyingList copy.!

recomputeView
	"re-compute our view from our undlerlyingList and the filter and sort blocks"

	| derived |

	derived := OrderedCollection withAll: underlyingList list.
	filterBlock ifNotNil: [:it | derived := derived select: it].
	sortBlock ifNotNil: [:it | derived := derived asSortedCollection: it].

	super setList: derived searchPolicy: underlyingList searchPolicy.

	self notifyListChanged.
!

remove: anObject ifAbsent: a0Block
	"remove the given object from this view and from our underlying collection, or
	answer the result of evaluating a0Block if the object is not present IN THIS VIEW"

	^ (self wouldInclude: anObject)
		ifTrue: [underlyingList remove: anObject ifAbsent: a0Block]
		ifFalse: [a0Block value].
!

removeAll
	"discard all our contents.  I.e. remove all the objects IN THIS VIEW
	from the underlying collection which may or may not become empty"

	(Array withAll: self list) do: [:each | underlyingList remove: each ifAbsent: []].
!

removeAtIndex: anIndex
	"remove the item at the given index, answering the removed element"

	^ underlyingList remove: (self at: anIndex).!

resize: anInteger
	"private -- not sure what this is supposed to mean for a view, so we ignore it..."
!

setList: collection searchPolicy: policy
	"private -- we only get here if I've forgotton something"

	self shouldNotImplement.!

solicitEvents
	"private -- hook ourself up the the list-change events generated by our underlying
	ListModel"

	underlyingList
		when: #item:addedAtIndex: send: #onItem:addedAtIndex: to: self;
		when: #item:removedAtIndex: send: #onItem:removedAtIndex: to: self;
		when: #item:updatedAtIndex: send: #onItem:updatedAtIndex: to: self;
		when: #listChanged send: #onListChanged to: self.!

sortBlock
	"answer our sort block or nil if we are not sorted"

	^ sortBlock.
!

sortBlock: a2BlockOrNil
	"set our sort block to the given <dyadicValuable> or nil, which will cause us
	to revert to the underlying collection's actual order (see #beUnsorted for
	a version that does not cause that reversion)"

	sortBlock = a2BlockOrNil ifTrue: [^ self].

	"NB: forwarding to the general case like this will involve re-running the filter block,
	which is logically unecessary, but that's a simple way to ensure that we revert
	to the underlying order, not just forget our current ordering"
	self underlyingList: underlyingList sortBlock: a2BlockOrNil.

!

swap: index1 with: index2
	"swap the elements at the given indices.
	Overriden to swap the elements in the derived list without a lot of
	messing around"

	list swap: index1 with: index2.

	self
		notifyUpdatedAtIndex: index1;
		notifyUpdatedAtIndex: index2.!

underlyingList
	"answer our underlying ListModel, of which we provide a 'view'"

	^ underlyingList.
!

underlyingList: aListModel
	"set ourselves to wrap the given ListModel and present
	a 'view' of it that is filtered or sorted using our current sort
	and filter blocks"

	self underlyingList: aListModel filterBlock: filterBlock sortBlock: sortBlock.!

underlyingList: aListModel filterBlock: a1Block
	"set ourselves to wrap the given ListModel and present
	a 'view' of it filtered by a1Block and sorted by our existing
	sort block"

	self underlyingList: aListModel filterBlock: a1Block sortBlock: sortBlock.!

underlyingList: aListModel filterBlock: a1Block sortBlock: a2Block
	"set ourselves to wrap the given ListModel and present
	a 'view' of it filtered by a1Block.  Note that using this method
	can involve significantly less redundant computation than setting
	the attributes individually"

	underlyingList == aListModel ifFalse:
		[self declineEvents.
		underlyingList := aListModel.
		self solicitEvents].
	filterBlock := a1Block.
	sortBlock := a2Block.

	self recomputeView.
!

underlyingList: aListModel sortBlock: a2Block
	"set ourselves to wrap the given ListModel and present
	a 'view' of it sorted by a2Block and filtered by our existing
	filter block"

	self underlyingList: aListModel filterBlock: filterBlock sortBlock: a2Block.!

updateAtIndex: anInteger
	"issue changed notification for the element at the given index.
	Note that we answer the *element* at the index.
	Overriden to inform the underlying list too"

	| item |

	item := super updateAtIndex: anInteger.
	underlyingList updateItem: item ifAbsent: [].

	^ item.!

updateItem: anObject ifAbsent: a0Block
	"issue changed notification for the given element or evaluate a0Block
	if THE UNDERLYING LIST does not contain it.
	Note that we answer the *index* of the element or 0 if the element is
	not currently in this view"

	underlyingList updateItem: anObject ifAbsent: [^ a0Block value].

	^ super updateItem: anObject ifAbsent: [0].

!

wouldInclude: anObject
	"answer whether our filter block (if any) would accept anObject"

	^ filterBlock
		ifNil: [true]
		ifNotNil: [:it | it value: anObject].! !
!DerivedListModel categoriesFor: #add:afterIndex:!adding!public! !
!DerivedListModel categoriesFor: #addAnsweringIndex:!adding!public! !
!DerivedListModel categoriesFor: #at:put:!accessing!public! !
!DerivedListModel categoriesFor: #beSorted!public!sorting! !
!DerivedListModel categoriesFor: #beSorted:!public!sorting! !
!DerivedListModel categoriesFor: #beUnsorted!public!sorting! !
!DerivedListModel categoriesFor: #declineEvents!event handling!private! !
!DerivedListModel categoriesFor: #filterBlock!accessing!filtering!public! !
!DerivedListModel categoriesFor: #filterBlock:!accessing!filtering!public! !
!DerivedListModel categoriesFor: #ignoringEventsDo:!event handling!private! !
!DerivedListModel categoriesFor: #initialize!initializing!private! !
!DerivedListModel categoriesFor: #isFiltered!filtering!public!testing! !
!DerivedListModel categoriesFor: #isSorted!public!sorting!testing! !
!DerivedListModel categoriesFor: #list:!accessing!public! !
!DerivedListModel categoriesFor: #onItem:addedAtIndex:!event handling!private! !
!DerivedListModel categoriesFor: #onItem:removedAtIndex:!event handling!private! !
!DerivedListModel categoriesFor: #onItem:updatedAtIndex:!event handling!private! !
!DerivedListModel categoriesFor: #onListChanged!event handling!private! !
!DerivedListModel categoriesFor: #postCopy!copying!public! !
!DerivedListModel categoriesFor: #recomputeView!public!updating! !
!DerivedListModel categoriesFor: #remove:ifAbsent:!public!removing! !
!DerivedListModel categoriesFor: #removeAll!public!removing! !
!DerivedListModel categoriesFor: #removeAtIndex:!public!removing! !
!DerivedListModel categoriesFor: #resize:!mutating!private! !
!DerivedListModel categoriesFor: #setList:searchPolicy:!initializing!private! !
!DerivedListModel categoriesFor: #solicitEvents!event handling!private! !
!DerivedListModel categoriesFor: #sortBlock!accessing!public!sorting! !
!DerivedListModel categoriesFor: #sortBlock:!accessing!public!sorting! !
!DerivedListModel categoriesFor: #swap:with:!operations!public! !
!DerivedListModel categoriesFor: #underlyingList!accessing!public! !
!DerivedListModel categoriesFor: #underlyingList:!accessing!public! !
!DerivedListModel categoriesFor: #underlyingList:filterBlock:!accessing!public! !
!DerivedListModel categoriesFor: #underlyingList:filterBlock:sortBlock:!accessing!filtering!public!sorting! !
!DerivedListModel categoriesFor: #underlyingList:sortBlock:!accessing!public! !
!DerivedListModel categoriesFor: #updateAtIndex:!public!updating! !
!DerivedListModel categoriesFor: #updateItem:ifAbsent:!public!searching!updating! !
!DerivedListModel categoriesFor: #wouldInclude:!filtering!public!testing! !

!DerivedListModel class methodsFor!

on: aListModel
	"answer a new instance that provides a filtered and/or sorted 'view' of the given
	ListModel"

	^ self on: aListModel filterBlock: nil sortBlock: nil.
!

on: aListModel filterBlock: a1Block
	"answer a new instance that provides a filtered and/or sorted 'view' of the given
	ListModel.  The initial filter is set by the <monadicValuable>, a1Block.  It may be
	that the resulting instance 'looks' empty if none of the existing elements satisfy the
	filter condition"

	^ self on: aListModel filterBlock: a1Block sortBlock: nil.!

on: aListModel filterBlock: a1Block sortBlock: a2Block
	"answer a new instance that provides a filtered and/or sorted 'view' of the given
	ListModel.  The initial filter is set by the <monadicValuable>, a1Block and the initial
	ordering is set by the <dyadicValuable>m a2Block"

	^ (self basicNew)
		initialize;
		underlyingList: aListModel filterBlock: a1Block sortBlock: a2Block;
		yourself.
!

on: list searchPolicy: searchPolicy
	"private -- overriden because we *only* wrap existing ListModels"

	^ self shouldNotImplement.
!

on: aListModel sortBlock: a2Block
	"answer a new instance that provides a filtered and/or sorted 'view' of the given
	ListModel.  The initial ordering is set by the <dyadicValuable>m a2Block"

	^ self on: aListModel filterBlock: nil sortBlock: a2Block.! !
!DerivedListModel class categoriesFor: #on:!instance creation!public! !
!DerivedListModel class categoriesFor: #on:filterBlock:!instance creation!public! !
!DerivedListModel class categoriesFor: #on:filterBlock:sortBlock:!instance creation!public! !
!DerivedListModel class categoriesFor: #on:searchPolicy:!instance creation!private! !
!DerivedListModel class categoriesFor: #on:sortBlock:!instance creation!public! !

DerivedListPresenter guid: (GUID fromString: '{3A13471A-2757-4744-BB3B-0FC2A1A7C2B9}')!
DerivedListPresenter comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Version of ListPresenter that "knoiws" that its model is able to filter and sort stuff, and which therefore doesn''t spoil the party by doing so itself.'!
!DerivedListPresenter categoriesForClass!Unclassified! !
!DerivedListPresenter methodsFor!

beNotSorted
	"set our model to use no sorting"

	self model beUnsorted.!

beSorted
	"set our model to use a default sort block"

	self model beSorted.!

beSorted: a2Block
	"set our model to use the given sort block"

	self model beSorted: a2Block.
!

filterBlock
	"answer our model's filter block or nil"

	^ self model filterBlock.
!

filterBlock: a1Block
	"set our model's filter block to the given <monadicValuable> or nil to turn off
	filtering"

	self model filterBlock: a1Block.!

isFiltered
	"answer whether we are currently presenting a filtered view"

	^ self model isFiltered.!

isSorted
	"answer whether we are currently presenting a sorted view"

	^ self model isSorted.!

sortBlock
	"answer our model's sort block or nil if it is not sorted"

	^ self model sortBlock.
!

sortBlock: a2BlockOrNil
	"set our smodel's sort block to the given <dyadicValuable> or nil, which will cause us
	to revert to the underlying collection's actual order (see #beNotSorted for
	a version that does not cause that reversion)"

	"it should be unecessary to update the instav, but it may help reduce fragility"
	super sortBlock: a2BlockOrNil.

	self model sortBlock: a2BlockOrNil.! !
!DerivedListPresenter categoriesFor: #beNotSorted!public!sorting! !
!DerivedListPresenter categoriesFor: #beSorted!public!sorting! !
!DerivedListPresenter categoriesFor: #beSorted:!public!sorting! !
!DerivedListPresenter categoriesFor: #filterBlock!accessing!filtering!public! !
!DerivedListPresenter categoriesFor: #filterBlock:!accessing!filtering!public! !
!DerivedListPresenter categoriesFor: #isFiltered!filtering!public!testing! !
!DerivedListPresenter categoriesFor: #isSorted!public!sorting!testing! !
!DerivedListPresenter categoriesFor: #sortBlock!accessing!public!sorting! !
!DerivedListPresenter categoriesFor: #sortBlock:!accessing!public!sorting! !

!DerivedListPresenter class methodsFor!

defaultModel
	"answer a model to be used by default"

	^ DerivedListModel on: (super defaultModel).! !
!DerivedListPresenter class categoriesFor: #defaultModel!models!public! !

"Binary Globals"!

"Resources"!

