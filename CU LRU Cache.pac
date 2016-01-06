| package |
package := Package name: 'CU LRU Cache'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A Dictionary that has a maximum capacity, and which will evict the least recently accessed element in order to make new ones without exceeding its configured capacity.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '2.00'.


package classNames
	add: #LRUDictionary;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

AbstractDictionary variableSubclass: #LRUDictionary
	instanceVariableNames: 'dictionary evictionBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LRUDictionary guid: (GUID fromString: '{0732497E-4B9A-4DC7-B09A-34CD0FC066A0}')!
LRUDictionary comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

An LRU disctionary is a Dictionary-like object that keeps up to its #capacity elements in an internal <Dictionary> and which will remove the least-recently accessed element from the table if necessary in order to be able to add (#att:put:) a new element without exceeding the configured capacity.

The idea is that it can be used as a cache to hold objects that you might need again, but might not too, and where the cost of re-generating them is high enough that you don''t want to do that more than you have to, but where the (memory) cost of keeping them is high enough that you don''t want to keep more around than you have to either.

By default we use an internal LookupTable to perform the lookups, but that can be changed to a PluggableLookupTable by setting our #comparisonPolicy:.

Doing a #select:, #collect:, #reject:, etc, on an LRUDictionary will actually answer the result of forwarding the message to our internal <Dictionary>, i.e. the "LRU-ness" is not preserved.

As an implementation note, we keep the LRU list of keys in our own slots, and reorder that as items are accessed.  It''s not always clear which operations can be considered to be ''accesses'' of a given key (e.g. consider #keyAtValue:), so to some extent this is arbitrary depending on the inherited implementation.  We do override #keyAndValuesDo: to prevent re-ordering in a fairly large class of loops over our elements.

Note that this implementation is quite slow (O(n) in our capacity) and could be improved, for instance by using a linked-list + dictionary to hold the LRU data.

A future enhancement may allow the table to be have weak keys, however that''s not as easy as just plugging in a WeakDictionary since we have to protect our own state against concurrent access too.'!
!LRUDictionary categoriesForClass!Unclassified! !
!LRUDictionary methodsFor!

ageOf: aKey
	"answer an integer indicating how 'old' (in the LRU sense) we think the given
	key object is.  That will be 0 if we don't hold that key currently (by analogy with
	#indexOf:) or a number between 1 and our #capacity inclusive.  Higher numbers
	mean that the object is older"

	^ self ageOf: aKey ifAbsent: [0].!

ageOf: aKey ifAbsent: a0Block
	"answer an integer indicating how 'old' (in the LRU sense) we think the given
	key object is.  That will be a number between 1 and our #capacity inclusive.
	Higher numbers mean that the object is older.  If we don't know about the
	key object then answer the result of evaluating a0Block"

	| sp |

	"we hold keys in our own slots in order of increasing age"

	sp := self searchPolicy.
	1 to: self size do: [:i | (sp compare: (self basicAt: i) with: aKey) ifTrue: [^ i]].

	^ a0Block value.!

at: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	| answer |

	answer := dictionary at: aKey ifAbsent: [^ a0Block value].
	self touch: aKey.

	^ answer.!

at: aKey put: anObject
	"one of the root methods for <Dictionary>"

	aKey isNil ifTrue: [^ self error: 'key cannot be nil'].

	self makeRoomFor: aKey.

	"the order of the next two lines is critical"
	self touch: aKey.
	^ dictionary at: aKey put: anObject.!

atAge: anInteger
	"answer nth most recently accesed key in the collection"

	^ self basicAt: anInteger.!

averageProbesPerElement
	"answer the average number of probes necessary to  find an element"

	^ dictionary averageProbesPerElement.!

byAgeDo: a2Block
	"evaluate a2Block passing in keys and values in order starting with our oldest
	element and finishing with our youngest"

	self size to: 1 by: -1 do:
		[:age || key value |
		key := self atAge: age.
		value := dictionary at: key.
		a2Block value: key value: value].!

capacity
	"answer how many elements we can contain before we start
	evicting the least-recent-used ones"

	^ self basicSize.!

capacity: anInteger
	"set how many elements we can contain before we start
	evicting the least-recent-used ones.
	NB: may cause older elements to be evicted"

	| newSize |

	newSize := anInteger max: 0.

	[self size > newSize] whileTrue: [self evictOldest].
	self basicResize: newSize.!

collisions
	"answser how many of our elements would not be found on first hash probe"

	^ dictionary collisions.!

discardLRUFor: aKey
	"private -- ensure that aKey is no longer taking up room in our LRU record"

	| index size |

	index := self ageOf: aKey ifAbsent: [^ self].
	size := self size.
	index to: size-1 do: [:i | self basicAt: i put: (self basicAt: i+1)].
	self basicAt: size put: nil.
	!

evictionBlock
	"answer the <monadicValuable> that will be evaluated for any object
	that is implicitly (via #at:put:, #capacity:) or explicitly (via #evict*) evicted.
	The block will *not* be evaluated when items are removed using the
	#remove: family of methods.
	May be nil"

	^ evictionBlock.!

evictionBlock: a1BlockOrNil
	"set the <monadicValuable> that will be evaluated for any object
	that is implicitly (via #at:put:, #capacity:) or explicitly (via #evict*) evicted.
	The block will *not* be evaluated when items are removed using the
	#remove: family of methods"

	evictionBlock := a1BlockOrNil.!

evictKey: aKey ifAbsent: a0Block
	"remove and answer the item with the given key, or the
	result of evaluatiiong a0Block if there is none.
	This differs from #removeKeyIfNone only in that it
	applies the #evictionBlock (if any) to the removed
	item (the item, not the key)"

	| removed |

	removed := self removeKey: aKey ifAbsent: [^ a0Block value].

	evictionBlock ifNotNil: [:it | it value: removed].

	^ removed.!

evictOldest
	"remove and answer the oldest element thow an error if
	we have no oldest member (we are empty).
	This differs from #removeOldest only in that it
	applies the #evictionBlock (if any) to the removed
	item"

	^ self evictOldestIfNone: [self errorNotFound: '<the oldest>'].!

evictOldestIfNone: a0Block
	"remove and answer the oldest element or the result of
	evaluating a0Block if we have no oldest member (we are
	empty)
	This differs from #removeOldestIfNone: only in that it
	applies the #evictionBlock (if any) to the removed
	item"

	| removed |

	removed := self removeOldestIfNone: [^ a0Block value].

	evictionBlock ifNotNil: [:it | it value: removed].

	^ removed.
!

grow
	"overiden to throw an error since we are not growable (just in case)"

	self shouldNotImplement.!

includesKey: aKey
	"one of the root methods for <Dictionary>.  We don't want to pick
	up the default implementation because this should not affect the
	LRU rankings"

	^ dictionary includesKey: aKey.!

initialize
	"private establish a coherent intial state"

	dictionary := self makeDictionary.!

keys
	"answer a <collection> containing our keys"

	"it'd be easier, but more fragile, just to override #keysClass"

	^ dictionary keys.!

keysAndValuesDo: a2Block
	"one of the root methods for <Dictionary>.
	This is overriden mainly so that loops will not cause us to re-order our LRU list"

	^ dictionary keysAndValuesDo: a2Block.!

makeDictionary
	"private -- answer a new <Dictionary> suitable for doing our lookups using our default search policy"

	^ LookupTable new: self capacity.

!

makeDictionary: aSearchPolicy
	"private -- answer a new <Dictionary>suitable for doing lookups using the given search policy"

	^ PluggableLookupTable
		new: self capacity
		searchPolicy: aSearchPolicy.
!

makeRoomFor: aKey
	"private -- if we don't have space for the given key element, then
	evict the least-recently-used"

	(self size < self capacity or: [dictionary includesKey: aKey])
		ifFalse: [self evictOldest].!

oldest
	"answer the least recently accesed key in the collection"

	^ self atAge: self size.!

postCopy
	"called after we have been copied in order that we can do any last-minute cleanup"

	super postCopy.

	dictionary := dictionary copy.
!

rehash
	"re-build any data-structures that depend on object's #hash-es"

	dictionary rehash.!

removeKey: aKey ifAbsent: a0Block
	"remove and answer the item with the given key, or the
	result of evaluatiiong a0Block if there is none.
	Note that the eviction block is not executed since the
	item is being removed explicitly"

	self discardLRUFor: aKey.

	^ dictionary removeKey: aKey ifAbsent: a0Block.!

removeOldest
	"remove and answer the oldest element thow an error if
	we have no oldest member (we are empty).
	Note that the eviction block is not executed since the
	item is being removed explicitly"

	^ self removeOldestIfNone: [self errorNotFound: '<the oldest>'].!

removeOldestIfNone: a0Block
	"remove and answer the oldest element or the result of
	evaluating a0Block if we have no oldest member (we are
	empty)
	Note that the eviction block is not executed since the
	item is being removed explicitly"

	| age oldest |

	age := self size.
	age = 0 ifTrue: [^ a0Block value].

	oldest := self basicAt: age.
	self basicAt: age put: nil.

	^ dictionary removeKey: oldest.!

searchPolicy
	"answer our SearchPolicy"

	^ dictionary searchPolicy.!

searchPolicy: aSearchPolicy
	"set our SearchPolicy to that given.
	Note that it is only legal to do this while we are empty"

	self assert: [self isEmpty].

	dictionary := self makeDictionary: aSearchPolicy.!

shrink
	"reduce our storage to a minimum, this doesn't make a lot of
	sense for an LRU cache, but we may as well shrink the dictionary"

	dictionary shrink.!

size
	"one of the root methods for <Collection>"

	^ dictionary size.!

species
	"answer the kind of collection to answer from #collect:, etc"

	#CUtodo.  "does this interfere with #= or #equals: ?"

	^ dictionary species.!

touch: aKey
	"private -- mark the given key object as being the 'youngest' (in the LRU sense) that we
	contain.  It is assumed that anObject is either already in our dictionary, or that it soon will
	be.
	NB: this must be called *before* any new key is added to the dictionary, since it assumes
	that we have exactly #size valid LRU entries"

	| index |

	"we hold keys in our own slots in order of increasing age"

	index := self ageOf: aKey ifAbsent: [self capacity].

	index to: 2 by: -1 do: [:i | self basicAt: i put: (self basicAt: i-1)].
	self basicAt: 1 put: aKey.!

youngest
	"answer the most recently accesed key in the collection"

	^ self atAge: 1.! !
!LRUDictionary categoriesFor: #ageOf:!accessing!public! !
!LRUDictionary categoriesFor: #ageOf:ifAbsent:!accessing!public! !
!LRUDictionary categoriesFor: #at:ifAbsent:!accessing!public! !
!LRUDictionary categoriesFor: #at:put:!accessing!adding!public! !
!LRUDictionary categoriesFor: #atAge:!accessing!public! !
!LRUDictionary categoriesFor: #averageProbesPerElement!development!public!statistics! !
!LRUDictionary categoriesFor: #byAgeDo:!enumerating!public! !
!LRUDictionary categoriesFor: #capacity!accessing!public! !
!LRUDictionary categoriesFor: #capacity:!accessing!public! !
!LRUDictionary categoriesFor: #collisions!development!public!statistics! !
!LRUDictionary categoriesFor: #discardLRUFor:!operations!private! !
!LRUDictionary categoriesFor: #evictionBlock!accessing!public! !
!LRUDictionary categoriesFor: #evictionBlock:!accessing!public! !
!LRUDictionary categoriesFor: #evictKey:ifAbsent:!public!removing! !
!LRUDictionary categoriesFor: #evictOldest!public!removing! !
!LRUDictionary categoriesFor: #evictOldestIfNone:!public!removing! !
!LRUDictionary categoriesFor: #grow!adding!private! !
!LRUDictionary categoriesFor: #includesKey:!public!searching! !
!LRUDictionary categoriesFor: #initialize!initializing!private! !
!LRUDictionary categoriesFor: #keys!accessing!public! !
!LRUDictionary categoriesFor: #keysAndValuesDo:!enumerating!public! !
!LRUDictionary categoriesFor: #makeDictionary!helpers!private! !
!LRUDictionary categoriesFor: #makeDictionary:!helpers!private! !
!LRUDictionary categoriesFor: #makeRoomFor:!operations!private! !
!LRUDictionary categoriesFor: #oldest!accessing!public! !
!LRUDictionary categoriesFor: #postCopy!copying!public! !
!LRUDictionary categoriesFor: #rehash!operations!public! !
!LRUDictionary categoriesFor: #removeKey:ifAbsent:!public!removing! !
!LRUDictionary categoriesFor: #removeOldest!public!removing! !
!LRUDictionary categoriesFor: #removeOldestIfNone:!public!removing! !
!LRUDictionary categoriesFor: #searchPolicy!accessing!public! !
!LRUDictionary categoriesFor: #searchPolicy:!accessing!mutating!public! !
!LRUDictionary categoriesFor: #shrink!mutating!public! !
!LRUDictionary categoriesFor: #size!accessing!public! !
!LRUDictionary categoriesFor: #species!constants!public! !
!LRUDictionary categoriesFor: #touch:!operations!private! !
!LRUDictionary categoriesFor: #youngest!accessing!public! !

!LRUDictionary class methodsFor!

new
	"private -- it doesn't make sense to create an LRU collection with no stated
	capacity"

	self shouldNotImplement.!

new: anInteger
	"answer a new instance with the given capacity, a default
	comparison policy, and no eviction block"

	^ (self basicNew: anInteger)
		initialize;
		yourself.!

new: anInteger evictionBlock: a1Block
	"answer a new instance with the given capacity, a default search policy,
	and the given eviction block"

	^ (self new: anInteger)
		evictionBlock: a1Block;
		yourself.
!

new: anInteger evictionBlock: a1Block searchPolicy: aSearchPolicy
	"answer a new instance with the given capacity, eviction block, and search policy"

	^ (self new: anInteger)
		evictionBlock: a1Block;
		searchPolicy: aSearchPolicy;
		yourself.
!

new: anInteger searchPolicy: aSearchPolicy
	"answer a new instance with the given capacity and search policy"

	^ (self new: anInteger)
		searchPolicy: aSearchPolicy;
		yourself.
! !
!LRUDictionary class categoriesFor: #new!instance creation!private! !
!LRUDictionary class categoriesFor: #new:!instance creation!public! !
!LRUDictionary class categoriesFor: #new:evictionBlock:!instance creation!public! !
!LRUDictionary class categoriesFor: #new:evictionBlock:searchPolicy:!instance creation!public! !
!LRUDictionary class categoriesFor: #new:searchPolicy:!instance creation!public! !

"Binary Globals"!

"Resources"!

