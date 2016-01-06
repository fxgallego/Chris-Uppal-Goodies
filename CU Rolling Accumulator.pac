| package |
package := Package name: 'CU Rolling Accumulator'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002-2005.
chris.uppal@metagnostic.org

RollingAccumulator is a Collection that acts pretty much like an Array except that it also supports #add, and as items are added to the end they are rolled off the front.  It''s implemented as a circular buffer inside.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

2.00
-	Trivial optimisation in #resize:withAll:.

1.00
-	First release.

'.

package basicPackageVersion: '2.00'.


package classNames
	add: #RollingAccumulator;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

SequenceableCollection variableSubclass: #RollingAccumulator
	instanceVariableNames: 'last'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

RollingAccumulator guid: (GUID fromString: '{2E943091-3D7D-4965-B5D8-F24B08ADC5C7}')!
RollingAccumulator comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

One of these acts like an array of fixed size.  It supports the normal indexing and enumerating protocols.

However it also supports #add: which has the effect of discarding the first element of the array, moving everything else down by one element, and adding the new element at the end.

Of course, it''s not implemented by moving every element, we have the usual circular buffer structure.

	-- chris'!
!RollingAccumulator categoriesForClass!Unclassified! !
!RollingAccumulator methodsFor!

add: anObject
	"add anObject to the receiver, discarding the oldest value held"

	last := last + 1.
	last > self size ifTrue: [last := 1].

	^ self basicAt: last put: anObject.!

at: anInteger
	"answer the value at the given index, since we discard elements as we add
	new ones, that normally means the oldest-but-(aninteger-1)th element"

	^ self basicAt: (self translateIndex: anInteger).

!

at: anInteger put: anObject
	"set the value at the given index, since we discard elements as we add
	new ones, that normally means replacing the oldest-but-(aninteger-1)th
	element"

	^ self basicAt: (self translateIndex: anInteger) put: anObject.

!

copyEmpty: anInteger
	"private -- overridden to ensure that 'last' is copied over"

	^ (super copyEmpty: anInteger)
		last: anInteger;
		yourself.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"the underlying method for all enumerations of <SequenceableCollection>"

	| start stop index |

	aStartIndex > aStopIndex ifTrue: [^ self].
	aStartIndex < 1 ifTrue: [self errorSubscriptBounds: aStartIndex].
	aStartIndex > self size ifTrue: [self errorSubscriptBounds: aStartIndex].	"stop index is tested in the loop, not upfront"

	index := aStartIndex - 1.
	start := self translateIndex: aStartIndex.
	stop := self translateIndex: aStopIndex.

	start > stop ifTrue:
		[start to: self size do: [:i | a2Block value: (index := index+1) value: (self basicAt: i)].
		stop := start-1.
		start := 1].

	^ start to: stop do: [:i | a2Block value: (index := index+1) value: (self basicAt: i)].!

initialize
	"private -- establish a coherent inital state"

	last := self size.

	"thow an exception here rather than testing on every access in later life"
	self size < 1 ifTrue: [self errorSubscriptBounds: 1].!

last: anInteger
	"private -- only used when copying"

	last := anInteger.!

resize: anInteger
	"if anInteger is > our current size then the new slots will be initialised to nil,
	otherwise the oldest slots will be discarded"

	self resize: anInteger withAll: nil.

!

resize: anInteger withAll: anObject
	"if anInteger is > our current size then the extra slots (which will be created as if they were
	the oldest ones) will be initialised to anObject, otherwise the oldest slots will be discarded"

	| copy toAdd toKill |

	anInteger < 1 ifTrue: [self errorSubscriptBounds: 1].
	anInteger = self size ifTrue: [^ self].

	toAdd := (anInteger - self size) max: 0.
	toKill := (self size - anInteger) max: 0.

	copy := self asArray.
	self basicResize: anInteger.

	last := 0.
	1 to: toAdd do: [:i | self basicAt: (last := last+1) put: anObject].
	toKill+1 to: copy size do: [:i | self basicAt: (last := last+1) put: (copy at: i)].
!

size
	"answer the number of elements in the collection"

	<primitive: 62>
	^ self primitiveFailed .!

species
	"answer the kind of collection to be used when copying the receiver"

	^ Array.!

translateIndex: anInteger
	"private -- answer the real index corresponding to the logical index, anInteger"

	| index |

	anInteger < 1 ifTrue: [self errorSubscriptBounds: anInteger].

	index := last + anInteger.
	index > self size ifTrue:
		[index := index - self size.
		index > last ifTrue: [self errorSubscriptBounds: anInteger]].

	^ index.! !
!RollingAccumulator categoriesFor: #add:!adding!public! !
!RollingAccumulator categoriesFor: #at:!accessing!public! !
!RollingAccumulator categoriesFor: #at:put:!accessing!public! !
!RollingAccumulator categoriesFor: #copyEmpty:!copying!private! !
!RollingAccumulator categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!RollingAccumulator categoriesFor: #initialize!initializing!private! !
!RollingAccumulator categoriesFor: #last:!copying!initializing!private! !
!RollingAccumulator categoriesFor: #resize:!mutating!public! !
!RollingAccumulator categoriesFor: #resize:withAll:!mutating!public! !
!RollingAccumulator categoriesFor: #size!accessing!public! !
!RollingAccumulator categoriesFor: #species!constants!public! !
!RollingAccumulator categoriesFor: #translateIndex:!helpers!private! !

!RollingAccumulator class methodsFor!

new
	"answer a new instance of the receiver with the default number of slots all set to nil"

	^ self new: 1.	"this would be 0, but sizes < 1 are not allowed"!

new: anInteger
	"Answer a new instance of the receiver with anInteger elements which are all nil"

	^ (self basicNew: anInteger)
		initialize;
		yourself.!

new: anInteger withAll: anObject
	"Answer a new instance of the receiver with anInteger elements which are all 
	the argument, anObject."

	^ (self new: anInteger)
		atAllPut: anObject;
		yourself.! !
!RollingAccumulator class categoriesFor: #new!instance creation!public! !
!RollingAccumulator class categoriesFor: #new:!instance creation!public! !
!RollingAccumulator class categoriesFor: #new:withAll:!instance creation!public! !

"Binary Globals"!

"Resources"!

