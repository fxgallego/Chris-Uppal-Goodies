| package |
package := Package name: 'CU 3-Way Sorting'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

A bunch of methods and classes which use a three-way comparison as the basis of sorting.

The idea is partly that the result of a comparison has three possible values so modelling it as a boolean is simply stupid.  Hence I hope this facility will make comparison code significantly more transparent.

The main part of the idea, though, is that because Smalltalk can''t know enough about the comparison block and how it relates to equality, identity, etc, it is not possible to use the best sorting algorithms. This package provides two new kinds of sorted collections (as well as some useful supporting code).   One of these is like SortedCollection but uses three way comparison to allow it to use the 3-way Quicksort algorithm (and make better use of sentinels than the "traditional" SortedCollection).  The other (<TODO>as yet unwritten</TODO>) is a variant which allows several sort criteria to be provided.  This allows us to make sorting, say, by #lastname, #firstname, #date-of-birth significantly easier *and* more efficient.  It is easier because the sorted collection knows about the different criteria so you don''t have to pack complicated combinations of criteria into one sort block.  It is more efficient because once two objects are known to have the same #lastname, the sort algorithm can then sort on #firstname without continually having to recompare the #lastnames.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '1.01'.


package classNames
	add: #ComparesLess;
	add: #ComparesMore;
	add: #ComparesSame;
	add: #ComparisonResult;
	add: #ThreeWaySortedCollection;
	yourself.

package methodNames
	add: #Association -> #<=>;
	add: #Character -> #<=>;
	add: #Collection -> #asThreeWaySortedCollection;
	add: #Collection -> #asThreeWaySortedCollection:;
	add: #Date -> #<=>;
	add: #HashedPair -> #<=>;
	add: #Object -> #<=>;
	add: #SequenceableCollection -> #<=>;
	add: #SequenceableCollection -> #threeWayCompareElementsWith:from:to:;
	add: #SequenceableCollection -> #threeWayDictionaryCompareWith:;
	add: #String -> #<=>;
	add: #String -> #threeWayCaseInsensitiveCompareWith:;
	add: #String -> #threeWayTrueCompareWith:;
	add: #Time -> #<=>;
	add: #TimeStamp -> #<=>;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Hashed Pair';
	add: 'CU String Extensions';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package setManualPrerequisites: #(
	'CU String Extensions').

package!

"Class Definitions"!

Object subclass: #ComparisonResult
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'instance'!
SortedCollection variableSubclass: #ThreeWaySortedCollection
	instanceVariableNames: 'sortSelector radixBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ComparisonResult subclass: #ComparesLess
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ComparisonResult subclass: #ComparesMore
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ComparisonResult subclass: #ComparesSame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Association methodsFor!

<=> anAssociation
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' anAssociation."
#CUadded.

	^ self key <=> anAssociation key.
! !
!Association categoriesFor: #<=>!comparing!public! !

!Character methodsFor!

<=> aCharacter
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aCharacter."
#CUadded.

	^ self asInteger <=> aCharacter asInteger.

! !
!Character categoriesFor: #<=>!comparing!public! !

!Collection methodsFor!

asThreeWaySortedCollection
	"Answer a <ThreeWaySortedCollection> of the same size as the receiver
	whose elements are those of the receiver, with the order of the result
	being determined by the default sort block (see ThreeWaySortedCollection).
	Exceptions may occur if any of the elements of the receiver are not
	appropriate parameters for the default sort block."
#CUadded.

	^ (ThreeWaySortedCollection new: self approxSize)
		addAll: self; 
		yourself.!

asThreeWaySortedCollection: sortBlock
	"Answer a <ThreeWaySortedCollection> whose elements are those of the receiver,
	sorted according to  the <dynadicValuable> argument, sortBlock which is required to answer
	instances of ComparisonResult.
	Exceptions may occur if any of the elements of the receiver are not
	appropriate parameters for sortBlock.
	Note: The argument does not need to be a BlockClosure, it must simply
	understand the #value:value: message from the dyadic valuable protocol."
#CUadded.

	^(ThreeWaySortedCollection new: self approxSize)
		sortBlock: sortBlock;
		addAll: self;
		yourself! !
!Collection categoriesFor: #asThreeWaySortedCollection!converting!public! !
!Collection categoriesFor: #asThreeWaySortedCollection:!converting!public! !

!Date methodsFor!

<=> aDate 
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aDate."
#CUadded.

	^ self asDays <=> aDate asDays.

! !
!Date categoriesFor: #<=>!comparing!public! !

!HashedPair methodsFor!

<=> aHashedPair
	"answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aHashedPair"
#CUadded.

	^ (first <=> aHashedPair first)
		ifSame: [second <=> aHashedPair second].
! !
!HashedPair categoriesFor: #<=>!comparing!public! !

!Object methodsFor!

<=> aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand.
	Note: this should really be provided by the VM with #(< <= >= >) implemented on top."
#CUadded.

	"we assume that we are likely to be not equal, so we are better off testing for
	an inequality.  In some case, e.g. Floats, #< is faster than #> so we use that"
	self < aComparand ifTrue: [^ ComparesLess instance].
	aComparand < self ifTrue: [^ ComparesMore instance].
	^ ComparesSame instance.
! !
!Object categoriesFor: #<=>!comparing!public! !

!SequenceableCollection methodsFor!

<=> aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand according to the order defined by comparing the size
	first, then receiver's elements with those of aComparand."
#CUadded.

	"compare by size then by elements only if the sizes are the same"
	^ (self size <=> aComparand size)
		ifSame: [self threeWayCompareElementsWith: aComparand from: 1 to: self size].
!

threeWayCompareElementsWith: aComparand from: aStartIndex to: aStopIndex
	"Answer one of the three ComparisonResults according as the elements of the receiver
	in the range (aStartIndex to: aStopIndex) are 'more than', 'same as', or 'less than' those
	of aComparand.
	Note that it is assumed that both collections contain elements in that range."
#CUadded.

	aStartIndex to: aStopIndex do:
		[:i || compare |
		compare := (self at: i) <=> (aComparand at: i).
		compare ifNotSame: [^ compare]].

	^ ComparisonResult same.

!

threeWayDictionaryCompareWith: aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand accoding to the dictionary order induced by elementwise
	comparison."
#CUadded.

	^ (self threeWayCompareElementsWith: aComparand from: 1 to: (self size min: aComparand size))
		ifSame: [self size <=> aComparand size].
! !
!SequenceableCollection categoriesFor: #<=>!comparing!public! !
!SequenceableCollection categoriesFor: #threeWayCompareElementsWith:from:to:!comparing!public! !
!SequenceableCollection categoriesFor: #threeWayDictionaryCompareWith:!comparing!public! !

!String methodsFor!

<=> aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand accoding to the dictionary collation sequence induced
	by the receiver's character encoding."
#CUadded.

	^ (self strcmp: aComparand) <=> 0.!

threeWayCaseInsensitiveCompareWith: aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand ignoring case, according to the implementation
	defined collation sequence (see _collate:)."
#CUadded.

	^ (self _collate: aComparand) <=> 0.
!

threeWayTrueCompareWith: aComparand
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aComparand according to the collation sequence defined by
	#trueCompare:"
#CUadded.

	^ (self trueCompare: aComparand) <=> 0.! !
!String categoriesFor: #<=>!comparing!public! !
!String categoriesFor: #threeWayCaseInsensitiveCompareWith:!comparing!public! !
!String categoriesFor: #threeWayTrueCompareWith:!comparing!public! !

!Time methodsFor!

<=> aTime
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aTime."
#CUadded.

	^ self asMilliseconds <=> aTime asMilliseconds.

! !
!Time categoriesFor: #<=>!comparing!public! !

!TimeStamp methodsFor!

<=> aTimeStamp
	"Answer one of the three ComparisonResults according as the receiver is 'more than',
	'same as', or 'less than' aTimeStamp."
#CUadded.

	^ (self date <=> aTimeStamp date)
		ifSame: [self time <=> aTimeStamp time].
! !
!TimeStamp categoriesFor: #<=>!comparing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ComparisonResult guid: (GUID fromString: '{85A1DBB2-2C4A-48A7-9A42-E677138F035E}')!
ComparisonResult comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

Three objects which are rather like the Boolean true and false, but are designed to hold the results of a comparison operation.

There are three concrete subclasses of ComparisonResult, each follows the Singleton pattern (the distinguished instances are accessed by #instance on their class-sides).  Also the class-side accessors, #more, #same, and #less answer the same instances.

There are a bunch of control-flow methods (akin to Boolean>>ifTrue:ifFalse:) which should be self-explanatory.

There is one oddity which is worth mentioning.  Some of the control flow methods don''t define actions for every possible case, these are:
	#ifMore:
	#ifNotMore:
	#ifMore:ifLess:
	#ifSame:
	#ifNotSame:
	#ifLess:
	#ifNotLess:
In these cases the result of the evaluation is *defined to be the receiver* in the case where no action is taken.  This is a bit of a hack, but it makes combining tests very convenient without requiring a load more methods.  For instance to compare two object by their #first aspect and then (only if the #firsts are the same) the #second, we can express it as:

	(o1 first <=> o2 first)
		ifSame: [o1 second <=> o2 second].

'!
!ComparisonResult categoriesForClass!Unclassified! !
!ComparisonResult methodsFor!

deepCopy
	"answer the receiver to maintain the Singleton pattern"

	^ self.!

ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: [self] ifSame: [self] ifLess: aLess0Block.!

ifLess: aLess0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: anElse0Block ifSame: anElse0Block ifLess: aLess0Block.!

ifMore: aMore0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: aMore0Block ifSame: [self] ifLess: [self].!

ifMore: aMore0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: aMore0Block ifSame: anElse0Block ifLess: anElse0Block.!

ifMore: aMore0Block ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks or the receiver"

	^ self ifMore: aMore0Block ifSame: [self] ifLess: aLess0Block.!

ifMore: aMore0Block ifSame: aSame0Block ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the three blocks"

	self subclassResponsibility.!

ifNotLess: aNotLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: aNotLess0Block ifSame: aNotLess0Block ifLess: [self].!

ifNotLess: aNotLess0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: aNotLess0Block ifSame: aNotLess0Block ifLess: anElse0Block.!

ifNotMore: aNotMore0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: [self] ifSame: aNotMore0Block ifLess: aNotMore0Block.!

ifNotMore: aNotMore0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: anElse0Block ifSame: aNotMore0Block ifLess: aNotMore0Block.!

ifNotSame: aNotSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: aNotSame0Block ifSame: [self] ifLess: aNotSame0Block.
!

ifNotSame: aNotSame0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: aNotSame0Block ifSame: anElse0Block ifLess: aNotSame0Block.
!

ifSame: aSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	^ self ifMore: [self] ifSame: aSame0Block ifLess: [self].
!

ifSame: aSame0Block else: anElse0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the two blocks"

	^ self ifMore: anElse0Block ifSame: aSame0Block ifLess: anElse0Block.
!

inverted
	"answer what the result would have been if the operands had been reversed"

	self subclassResponsibility.!

isLess
	"answer whether the receiver represents 'less than'"

	^ false.
!

isMore
	"answer whether the receiver represents 'more than'"

	^ false.
!

isSame
	"answer whether the receiver represents 'same as'"

	^ false.
!

shallowCopy
	"answer the receiver to maintain the Singleton pattern"

	^ self.
!

stbSaveOn: anSTBOutFiler
	"save out a binary representation of the receiver to anSTBOutFiler.
	Overriden to maintain the Singleton pattern"

	anSTBOutFiler
		saveObject: self
		as: (STBSingletonProxy for: self class using: #instance).! !
!ComparisonResult categoriesFor: #deepCopy!copying!public! !
!ComparisonResult categoriesFor: #ifLess:!control flow!public! !
!ComparisonResult categoriesFor: #ifLess:else:!control flow!public! !
!ComparisonResult categoriesFor: #ifMore:!control flow!public! !
!ComparisonResult categoriesFor: #ifMore:else:!control flow!public! !
!ComparisonResult categoriesFor: #ifMore:ifLess:!control flow!public! !
!ComparisonResult categoriesFor: #ifMore:ifSame:ifLess:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotLess:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotLess:else:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotMore:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotMore:else:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotSame:!control flow!public! !
!ComparisonResult categoriesFor: #ifNotSame:else:!control flow!public! !
!ComparisonResult categoriesFor: #ifSame:!control flow!public! !
!ComparisonResult categoriesFor: #ifSame:else:!control flow!public! !
!ComparisonResult categoriesFor: #inverted!operations!public! !
!ComparisonResult categoriesFor: #isLess!public!testing! !
!ComparisonResult categoriesFor: #isMore!public!testing! !
!ComparisonResult categoriesFor: #isSame!public!testing! !
!ComparisonResult categoriesFor: #shallowCopy!copying!public! !
!ComparisonResult categoriesFor: #stbSaveOn:!binary filing!public! !

!ComparisonResult class methodsFor!

instance
	"answer the single distinguished instance of the receiver"

	^ instance.!

less
	"answer the distinguished instance that represents the fact that a first comparand
	is less than a second"

	^ ComparesLess instance.!

more
	"answer the distinguished instance that represents the fact that a first comparand
	is more than a second"

	^ ComparesMore instance.!

new
	"private -- we use a singleton pattern so instances should not be created"

	self shouldNotImplement.!

same
	"answer the distinguished instance that represents the fact that a first comparand
	is neither more nor less than than a second"

	^ ComparesSame instance.! !
!ComparisonResult class categoriesFor: #instance!accessing!public! !
!ComparisonResult class categoriesFor: #less!constants!public! !
!ComparisonResult class categoriesFor: #more!constants!public! !
!ComparisonResult class categoriesFor: #new!instance creation!private! !
!ComparisonResult class categoriesFor: #same!constants!public! !

ThreeWaySortedCollection guid: (GUID fromString: '{A7910556-ECF0-45B8-B7E7-68743AED5864}')!
ThreeWaySortedCollection comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

'!
!ThreeWaySortedCollection categoriesForClass!Unclassified! !
!ThreeWaySortedCollection methodsFor!

addAnsweringIndex: anObject
	"add anObject as a new element of the receiver, inserting it in the
	correct place to maintain the ascending ordering of the receiver
	as determined by sortBlock.
	Answers the index at which anObject was added.
	Note that we use a binary search to give better average performance"

	Error notYetImplemented.
!

asSortedCollection
	"answer a <SortedCollection> containing the same elements as the receiver
	with the 'same' sort block"

	"note that we have to create a new sort block which wraps the three-way one we
	use as a boolean-values comparison.  This is not likely to be very useful, but is better
	than nothing"
	^ (SortedCollection new: self approxSize)
		sortBlock: [:e1 :e2 | (sortBlock value: e1 value: e2) isLess];
		addAll: self; 
		yourself.
!

asThreeWaySortedCollection
	"answer a <ThreeWaySortedCollection> containing the same elements as the receiver
	with the same sort block"

	^ self.
!

compare: anObject with: anotherObject

	^ sortBlock value: anObject value: anotherObject.!

compare: anObject withAt: anIndex 

	^ self compare: anObject with: (self basicAt: anIndex).
!

compareAt: anIndex with: anObject

	^ self compare: (self basicAt: anIndex) with: anObject.!

compareAt: anIndex withAt: anotherIndex

	^ self compare: (self basicAt: anIndex) with: (self basicAt: anotherIndex).!

copyEmpty: anInteger
	"private -- overriden to preserve the sortSelector and radixBlock"

	^(super copyEmpty: anInteger)
		sortSelector: sortSelector radixBlock: radixBlock;
		yourself.
!

exchangeAt: anIndex withAt: anotherIndex
	| tmp |

	tmp := self basicAt: anIndex.
	self basicAt: anIndex put: (self basicAt: anotherIndex).
	self basicAt: anotherIndex put: tmp.!

heapsortFrom: start to: stop
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	Heap sort is slower than quicksort by a constant factor, but is always O(log n) even in the
	worst case"

	Error notYetImplemented.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.
	self useThreeWayQuicksort.
!

insertsortFrom: start to: stop
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	This is the method of choice for very short intervals, but is slow for longer ranges unless
	they are known to be almost sorted already"

	Error notYetImplemented.
!

intosortFrom: aStartIndex to: aStopIndex
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	Intosort is a hybrid of quicksort and heapsort that normally performs as quicksort, but
	converts to heapsort if it detects that it is going quadratic"

	Error notYetImplemented.!

mergesortFrom: start to: stop
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock"

	Error notYetImplemented.
!

quicksortFrom: aStartIndex to: aStopIndex
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	We use the Quicksort with 3-way partitioning algorithm, as described in:
		Algorithms in C, 3rd Edition, Parts 1-4
		Robert Sedgewick
		ISBN 0-201-31452-5
	(which is *well* worth getting, BTW).
	See section 7.6, Page 325"

	Error notYetImplemented.
!

radixsortFrom: aStartIndex to: aStopIndex
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	We use a radix sort where we invoke the radixBlock supplied earlier to create a temporary
	table in which we will keep occurrence counts keyed by our elements.  Note that this is only
	usable if the table's idea of what consitiutes the 'same' key matches the sort block we are
	using.  Also this technique can convert elements that were merely equal into ones which are
	identical.  Often this is unacceptable.
	This can be *very* fast if we have many duplicate keys, or if the keys are known to be drawn
	from a smallish fixed population"

	| dict keys i |

	dict := radixBlock value.
	keys := OrderedCollection new.
	aStartIndex to: aStopIndex do:
		[:i || elem count |
		elem := self basicAt: i.
		count := dict at: elem ifAbsent: [keys addLast: elem. 0].
		count := count + 1.
		dict at: elem put: count].

	i := aStartIndex.
	(keys asThreeWaySortedCollection: sortBlock) do:
		[:elem || count |
		count := dict at: elem.	"if this fails then we weren't given the right sort of dictionary"
		count timesRepeat: [self basicAt: i put: elem. i := i + 1]].

"	self assert: [aStopIndex + 1 = i].	"!

reSort
	"private -- resort the entire contents of the receiver into the order specified
	by the current sortBlock"

	self perform: sortSelector with: firstIndex with: lastIndex.
!

shellsortFrom: aStartIndex to: aStopIndex
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.
	Shell sort is somewhat slower than quicksort, but is quick for partially
	sorted files, and doesn't have a bad worst case"

	Error notYetImplemented.!

sortSelector
	"private -- answer the receiver's sortSelector"

	^ sortSelector.
!

sortSelector: aSelector
	"private -- set the receiver's sortSelector to aSelector"

	self sortSelector: aSelector radixBlock: nil.
!

sortSelector: aSelector radixBlock: aRadixBlock
	"private -- set the receiver's sortSelector and radixBlock"

	sortSelector := aSelector.
	radixBlock := aRadixBlock.
!

threeWayQuicksortFrom: aStartIndex to: aStopIndex
	"private -- sort our elements in the range (aStartIndex to: aStopIndex) to be nondescending
	according to sortBlock.

	We use the 'Quicksort with 3-way partitioning' algorithm; this implementation is based on
	that presented in:
		Algorithms in C, 3rd Edition, Parts 1-4
		Robert Sedgewick
		ISBN 0-201-31452-5
	(which is *well* worth getting, BTW).
	See section 7.6, Page 325.

	Note that I've added a median-of-three pivot selection step, and a couple of bugfixes
	to the original version.

	Each iteration first splits the target range into 5 sections:
		[left, p]		elements 'same as' pivot
		(p, i)		elements 'less than' pivot
		[i, j]		elements still to be partitioned
		(j, q)		elements 'more than' pivot
		[q, right]	more elements 'same as' pivot, including the pivot itself which is at [right].

	Once the partitioning is done, the two outlying sections are folded into the middle to leave a
	three-way partition"

	| left right middle i j p q pivot cip cjp clr clm cmr tmp |

	left := aStartIndex.
	right := aStopIndex.

	#CUtodo. "try using insert sort for small cases"

	"pick off easy cases"
	right <= left ifTrue: [^ self].
	(clr := self compareAt: left withAt: right) ifMore:
		[self exchangeAt: left withAt: right.
		clr := clr inverted].
	right-left = 1 ifTrue: [^ self].
	middle := (left + right) bitShift: -1.
	(clm := self compareAt: left withAt: middle)
		ifMore:
			[self exchangeAt: left withAt: middle.
			tmp := cmr. cmr := clr. clr := tmp.
			clm := clm inverted]
		else:
			[cmr := self compareAt: middle withAt: right].
	cmr ifMore:
		[self exchangeAt: middle withAt: right.
		tmp := clr. clr := clm. clm := tmp.
		cmr := cmr inverted].
	right-left = 2 ifTrue: [^ self].

	"put the median (which is now in the middle) on the right since it will be the pivot"
	self exchangeAt: middle withAt: right.
	pivot := self basicAt: right.
	p := i := left -1.	"will be incremented before use"
	q := j := right.		"will be decremented before use"

	"note that we 'know' the relationship between the endpoints and the pivot
	(because we saved them) so we can use that to improve the initial edges
	of the partitions"
	self exchangeAt: middle withAt: right-1.
	clm
		ifMore: []
		ifSame: [p := i := left]
		ifLess: [i := left].
	cmr
		ifMore: []
		ifSame: [q := j := right-1]
		ifLess: [j := right-1].

	"now for the main partitioning loop"
	[
		"advance i to the first element not less than pivot, similarly retreat j to the last element
		not more.  We can use the pivot as a sentinel.  Note that we store the comparison
		results since we'll use them later"
		[i := i+1. (cip := self compareAt: i with: pivot) isLess] whileTrue.
		[j := j-1.  (cjp := self compareAt: j with: pivot) isMore and: [j > i]] whileTrue.
		i < j.
	] whileTrue: [
		"since [i] >= pivot >= [j], we know we can swap them; that exchanges the meanings of the saved
		comparisons, cip and cjp, but we allow for that"
		self exchangeAt: i withAt: j.
		cjp ifSame: [p := p+1. self exchangeAt: p withAt: i].
		cip ifSame: [q := q-1.  self exchangeAt: q withAt: j].
	].

	"bugfix: if the i and j converge on the last remaining element, and it's a 'same', then the above
	loop will exit without updating q to reflect that.  Correct for this case"
	i = j ifTrue: [cip ifSame: [q := q-1. self exchangeAt: q withAt: j]].

	"we now have first partitioning as mentioned above; i is the index of the first of the
	'more than' elements"

"	left to: p do: [:each | self assert: [(self compareAt: each with: pivot) isSame]].
	p+1 to: i-1 do: [:each | self assert: [(self compareAt: each with: pivot) isLess]].
	i to: q-1 do: [:each | self assert: [(self compareAt: each with: pivot) isMore]].
	q to: right-1 do: [:each | self assert: [(self compareAt: each with: pivot) isSame]].
"
	"swap the pivot into the 'middle'"
	self exchangeAt: i withAt: right.
	j := i-1. i := i+1.

	"swap the 'same as' elements into the 'middle'.
	bugfix: the original presentation made 1 too few loops"
	#CUtodo.  "this does more exchanges than it needs to"
	left  to: p        do: [:k | self exchangeAt: k withAt: j.  j := j-1].
	q    to: right-1 do: [:k | self exchangeAt: k withAt: i.  i := i+1].

"	left to: j do: [:each | self assert: [(self compareAt: each with: pivot) isLess]].
	j+1 to: i-1 do: [:each | self assert: [(self compareAt: each with: pivot) isSame]].
	i to: right do: [:each | self assert: [(self compareAt: each with: pivot) isMore]].
"
	#CUtodo. "iterate on larger case"
	self threeWayQuicksortFrom: left to: j.
	self threeWayQuicksortFrom: i to: right.
!

useInsertsort
	"tell the receiver to use insert sort"

	sortSelector := #insertsortFrom:to:.!

useIntrosort
	"tell the receiver to use intro sort"

	self sortSelector: #introsortFrom:to:.!

useMergesort
	"tell the receiver to use merge sort"

	self sortSelector: #mergesortFrom:to:.!

useQuicksort
	"tell the receiver to use standard 2-way quicksort"

	self sortSelector: #quicksortFrom:to:.!

useRadixsort: aRadixBlock
	"tell the receiver to use radix sort.
	aRadixBlock is required to be a <niladicValuable> which will answer an object
	which can be used to store a key to count mapping.  I.e. it must support
	#at:ifAbsent: and #at:put: with integer values, and any of our elements as
	keys (e.g. if the receiver contains integers, then the radix block might answer
	an array)"

	self sortSelector: #radixsortFrom:to: radixBlock: aRadixBlock.!

useShellsort
	"tell the receiver to use shellsort"

	self sortSelector: #shellsortFrom:to:.!

useThreeWayQuicksort
	"tell the receiver to use 3-way partitioned quicksort.
	This is the default"

	self sortSelector: #threeWayQuicksortFrom:to:.! !
!ThreeWaySortedCollection categoriesFor: #addAnsweringIndex:!adding!public! !
!ThreeWaySortedCollection categoriesFor: #asSortedCollection!converting!public! !
!ThreeWaySortedCollection categoriesFor: #asThreeWaySortedCollection!converting!public! !
!ThreeWaySortedCollection categoriesFor: #compare:with:!comparing!private! !
!ThreeWaySortedCollection categoriesFor: #compare:withAt:!comparing!private! !
!ThreeWaySortedCollection categoriesFor: #compareAt:with:!comparing!private! !
!ThreeWaySortedCollection categoriesFor: #compareAt:withAt:!comparing!private! !
!ThreeWaySortedCollection categoriesFor: #copyEmpty:!copying!private! !
!ThreeWaySortedCollection categoriesFor: #exchangeAt:withAt:!operations!private! !
!ThreeWaySortedCollection categoriesFor: #heapsortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #initialize!initializing!private! !
!ThreeWaySortedCollection categoriesFor: #insertsortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #intosortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #mergesortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #quicksortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #radixsortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #reSort!operations!private! !
!ThreeWaySortedCollection categoriesFor: #shellsortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #sortSelector!accessing!private! !
!ThreeWaySortedCollection categoriesFor: #sortSelector:!accessing!private! !
!ThreeWaySortedCollection categoriesFor: #sortSelector:radixBlock:!accessing!private! !
!ThreeWaySortedCollection categoriesFor: #threeWayQuicksortFrom:to:!algorithms!private! !
!ThreeWaySortedCollection categoriesFor: #useInsertsort!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useIntrosort!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useMergesort!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useQuicksort!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useRadixsort:!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useShellsort!accessing!public! !
!ThreeWaySortedCollection categoriesFor: #useThreeWayQuicksort!accessing!public! !

!ThreeWaySortedCollection class methodsFor!

caseInsensitiveSortBlock
	"qnswer a dyadic valuable which can be used to sort Strings into ascending
	order on a case *IN*sensitive basis"

	^ [:a :b | a threeWayCaseInsensitiveCompareWith: b].!

caseSensitiveSortBlock
	"qnswer a dyadic valuable which can be used to sort Strings into ascending
	order on a case sensitive basis"

	"note that the default three-way sort for strings *is* case-sensitive, but
	we may as well stick with the same (slow) collation order that is used
	by our superclass definition"
	^ [:a :b | a threeWayTrueCompareWith: b].!

check: aCollection

	| mine theirs |

	mine := aCollection asThreeWaySortedCollection.
	theirs := aCollection asSortedCollection.

	self assert: [mine asArray = theirs asArray].!

checkAll: aCollectionOfTests

	aCollectionOfTests do: [:each | self check: each].!

value: anObject1 value: anObject2
	"private -- implement this part of the <dyadicValuable> protocol in order that
	the receiver can be its own default sort 'block' which improves sorting
	performance"

	^ anObject1 <=> anObject2.! !
!ThreeWaySortedCollection class categoriesFor: #caseInsensitiveSortBlock!constants!public! !
!ThreeWaySortedCollection class categoriesFor: #caseSensitiveSortBlock!constants!public! !
!ThreeWaySortedCollection class categoriesFor: #check:!development!private! !
!ThreeWaySortedCollection class categoriesFor: #checkAll:!development!private! !
!ThreeWaySortedCollection class categoriesFor: #value:value:!evaluating!private! !

ComparesLess guid: (GUID fromString: '{F4BA4ACC-4A39-4320-BBDD-D8802AA8A9A8}')!
ComparesLess comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org
'!
!ComparesLess categoriesForClass!Unclassified! !
!ComparesLess methodsFor!

ifMore: aMore0Block ifSame: aSame0Block ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the three blocks"

	^ aLess0Block value.!

ifNotSame: aNotSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ aNotSame0Block value.
!

ifSame: aSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ self.
!

inverted
	"answer what the result would have been if the operands had been reversed"

	^ ComparesMore instance.!

isLess
	"answer whether the receiver represents 'less than'"

	^ true.
!

printOn: aStream
	"append a textual representation of the receiver to aStream"
	
	aStream nextPutAll: 'less'.! !
!ComparesLess categoriesFor: #ifMore:ifSame:ifLess:!control flow!public! !
!ComparesLess categoriesFor: #ifNotSame:!control flow!public! !
!ComparesLess categoriesFor: #ifSame:!control flow!public! !
!ComparesLess categoriesFor: #inverted!operations!public! !
!ComparesLess categoriesFor: #isLess!public!testing! !
!ComparesLess categoriesFor: #printOn:!printing!public! !

!ComparesLess class methodsFor!

initialize
	"private -- class initialisation.
		self initialize.
	"

	instance := self basicNew.!

uninitialize
	"private -- class tear-down.
		self uninitialize.
	"

	instance := nil.! !
!ComparesLess class categoriesFor: #initialize!initialization!private! !
!ComparesLess class categoriesFor: #uninitialize!initialization!private! !

ComparesMore guid: (GUID fromString: '{898F5AB5-D485-4428-BDF5-881227384F16}')!
ComparesMore comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org
'!
!ComparesMore categoriesForClass!Unclassified! !
!ComparesMore methodsFor!

ifMore: aMore0Block ifSame: aSame0Block ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the three blocks"

	^ aMore0Block value.!

ifNotSame: aNotSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ aNotSame0Block value.
!

ifSame: aSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ self.
!

inverted
	"answer what the result would have been if the operands had been reversed"

	^ ComparesLess instance.
!

isMore
	"answer whether the receiver represents 'more than'"

	^ true.
!

printOn: aStream
	"append a textual representation of the receiver to aStream"
	
	aStream nextPutAll: 'more'.! !
!ComparesMore categoriesFor: #ifMore:ifSame:ifLess:!control flow!public! !
!ComparesMore categoriesFor: #ifNotSame:!control flow!public! !
!ComparesMore categoriesFor: #ifSame:!control flow!public! !
!ComparesMore categoriesFor: #inverted!operations!public! !
!ComparesMore categoriesFor: #isMore!public!testing! !
!ComparesMore categoriesFor: #printOn:!printing!public! !

!ComparesMore class methodsFor!

initialize
	"private -- class initialisation.
		self initialize.
	"

	instance := self basicNew.!

uninitialize
	"private -- class tear-down.
		self uninitialize.
	"

	instance := nil.! !
!ComparesMore class categoriesFor: #initialize!initialization!private! !
!ComparesMore class categoriesFor: #uninitialize!initialization!private! !

ComparesSame guid: (GUID fromString: '{B40D3FCF-01E2-4736-ADC0-EADE128C189B}')!
ComparesSame comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org
'!
!ComparesSame categoriesForClass!Unclassified! !
!ComparesSame methodsFor!

ifMore: aMore0Block ifSame: aSame0Block ifLess: aLess0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating one of the three blocks"

	^ aSame0Block value.!

ifNotSame: aNotSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ self.
!

ifSame: aSame0Block
	"depending on whether the receiver represents 'more than', 'less than' or
	'same as', answer the result of evaluating the block or the receiver"

	"overridden for efficiency since this is often called"
	^ aSame0Block value.
!

inverted
	"answer what the result would have been if the operands had been reversed"

	^ self.!

isSame
	"answer whether the receiver represents 'same as'"

	^ true.
!

printOn: aStream
	"append a textual representation of the receiver to aStream"
	
	aStream nextPutAll: 'same'.! !
!ComparesSame categoriesFor: #ifMore:ifSame:ifLess:!control flow!public! !
!ComparesSame categoriesFor: #ifNotSame:!control flow!public! !
!ComparesSame categoriesFor: #ifSame:!control flow!public! !
!ComparesSame categoriesFor: #inverted!operations!public! !
!ComparesSame categoriesFor: #isSame!public!testing! !
!ComparesSame categoriesFor: #printOn:!printing!public! !

!ComparesSame class methodsFor!

initialize
	"private -- class initialisation.
		self initialize.
	"

	instance := self basicNew.!

uninitialize
	"private -- class tear-down.
		self uninitialize.
	"

	instance := nil.! !
!ComparesSame class categoriesFor: #initialize!initialization!private! !
!ComparesSame class categoriesFor: #uninitialize!initialization!private! !

"Binary Globals"!

"Resources"!

