| package |
package := Package name: 'CU Hashed Pair'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Class of "pair" objects, these are optimised for use as keys in dictionaries to allow/simulate dictionaries with 2 or more keys.

Also adds a few loose methods to Dictionary that use hashed pairs to implement a simple two-dimensional lookup scheme.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package classNames
	add: #HashedPair;
	add: #IdentityHashedPair;
	yourself.

package methodNames
	add: #Dictionary -> #at:at:;
	add: #Dictionary -> #at:at:ifAbsent:;
	add: #Dictionary -> #at:at:ifAbsentPut:;
	add: #Dictionary -> #at:at:ifPresent:;
	add: #Dictionary -> #at:at:put:;
	add: #Dictionary -> #combineKey:with:;
	add: #Dictionary -> #removeKey:key:;
	add: #Dictionary -> #removeKey:key:ifAbsent:;
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

Object subclass: #HashedPair
	instanceVariableNames: 'first second hash'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
HashedPair subclass: #IdentityHashedPair
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Dictionary methodsFor!

at: key1 at: key2
	"Answer the value keyed by the HashedPair, (key1, key2), in the receiver.
	If not found report an error."

#CUadded.
	^ self at: (self combineKey: key1 with: key2).
!

at: key1 at: key2 ifAbsent: operation
	"Answer the value keyed by the HashedPair, (key1, key2).  If key is not found
	answer the result of evaluating the <niladicValuable>, operation."

#CUadded.
	^ self at: (self combineKey: key1 with: key2) ifAbsent: operation.
!

at: key1 at: key2 ifAbsentPut: operation
	"Answer the value of the receiver keyed by the HashedPair, (key1, key2).
	If that is not one of the receiver's keys, then add the result of evaluating 
	the <niladicValuable>, operation, at key, and answer that result."

#CUadded.
	^ self at: (self combineKey: key1 with: key2) ifAbsentPut: operation.

!

at: key1 at: key2 ifPresent: operation
	"Answer the result of evaluating the monadic valuable, operation, if the
	HashedPair, (key1, key2), is the key of an element in the receiver, with that
	element as its argument. If the key is not present, then answer nil."

#CUadded.
	^ self at: (self combineKey: key1 with: key2) ifPresent: operation.
!

at: key1 at: key2 put: newElement
	"Store the <Object> argument newElement at the HashedPair
	(key1, key2), in the receiver. Answer newElement."
#CUadded.

	^ self at: (self combineKey: key1 with: key2) put: newElement.!

combineKey: key1 with: key2
	"Private - Answers a new key created by combining the two given keys, this is used for the
	'two-dimensional' accessors, #at:at:, etc."

#CUadded.
	^ (HashedPair first: key1 second: key2).
!

removeKey: key1 key: key2
	"Remove the value keyed by the HashedPair, (key1, key2).  If key is not found
	report an error, otherwise answer the value just removed."

#CUadded.
	^ self removeKey: (self combineKey: key1 with: key2).
!

removeKey: key1 key: key2 ifAbsent: operation
	"Remove the value keyed by the HashedPair, (key1, key2).  If key is not found
	answer the result of evaluating the <niladicValuable>, operation, otherwise
	answer the value just removed."

#CUadded.
	^ self removeKey: (self combineKey: key1 with: key2) ifAbsent: operation.
! !
!Dictionary categoriesFor: #at:at:!accessing!public! !
!Dictionary categoriesFor: #at:at:ifAbsent:!accessing!public! !
!Dictionary categoriesFor: #at:at:ifAbsentPut:!accessing!public! !
!Dictionary categoriesFor: #at:at:ifPresent:!accessing!public! !
!Dictionary categoriesFor: #at:at:put:!accessing!public! !
!Dictionary categoriesFor: #combineKey:with:!accessing!helpers!private! !
!Dictionary categoriesFor: #removeKey:key:!public!removing! !
!Dictionary categoriesFor: #removeKey:key:ifAbsent:!public!removing! !

"End of package definition"!

"Source Globals"!

"Classes"!

HashedPair guid: (GUID fromString: '{1A866B2C-E2C5-41F6-8990-19233C7F63EF}')!
HashedPair comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Class of "pair" objects, these are optimised for use as keys in dictionaries, hence are normally (but not necessarily) immutable.

Since we expect to be immutable, we pre-calculate our hash and use it for both the standard #hash method and as an optimisation
when computing #=.

(BTW, since we do all this precomputation, a triple created by nesting pairs is also pretty efficient as a key).'!
!HashedPair categoriesForClass!Unclassified! !
!HashedPair methodsFor!

<= aHashedPair
	"answer whether the receiver is 'less than or equal to' aHashedPair"

	first < aHashedPair first ifTrue: [^ true].
	aHashedPair first < first ifTrue: [^ false].
	^ second <= aHashedPair second.
!

= other
	"answer true iff the receiver is deemed to be the same as the <Object> other"

	"implementation note: 
	We don't use a shortcut == test since most instances of this
	class are expected to be short-lived.
	We do do a shortcut comparison of the hash, since we expect this
	to be quicker, normally, than the two equality tests"
	^ self class == other class
		and: [hash = other hash
			and: [first = other first
				and: [second = other second]]].
!

computeHash
	"private -- compute the hash (doesn't use the cached hash)"

	| h1 h2 lowbit |

	h1 := self firstHash.
	h2 := self secondHash.

	"rotate low bit of h1 to bit pos 29 (avoids creating a BigInteger)"
	lowbit := h1 bitAnd: 1.
	h1 := h1 bitAnd: ##((1 bitShift: 29) - 1).	"avoid sign extension for SmallIntegers"
	h1 := (h1 bitShift: -1) bitOr: (lowbit bitShift: 29).

	^ h1 bitXor: h2.!

copy
	"overridden to do a shallow copy with no post copy step"

	^ self shallowCopy.!

displayOn: aStream
	"append a short textual description of the receiver to aStream as a user
	would want to see it."

	aStream
		nextPutAll: $(;
		display: first;
		space;
		display: second;
		nextPutAll: $).
!

first
	"answer the receiver's first element"

	^ first.
!

first: anObject
	"set the receiver's first element to anObject and recompute the hash"

	first := anObject.
	self rehash.!

first: anObject second: anotherObject
	"set both the receiver's elements and recompute the hash"

	first := anObject.
	second := anotherObject.
	self rehash.!

firstHash
	"answer the hash of our first element"

	^ first hash.!

hash
	"answer the receiver's hash, note that this is pre-computed (in #first:second:)"

	^ hash.
!

postCopy
	"hook this to rehash ourselves after a deep copy"

	self rehash.

!

printOn: aStream
	"append a short textual description of the receiver to aStream"

	aStream
		nextPutAll: self class name;
		nextPutAll: ' first: ';
		print: first;
		nextPutAll: ' second: ';
		print: second.!

rehash
	"recompute our hash, can be used when the hash value of one of
	our elements has changed"

	hash := self computeHash.!

second
	"answer the receiver's second (other) element"

	^ second.
!

second: anObject
	"set the receiver's other element to anObject and recompute the hash"

	second := anObject.
	self rehash.!

secondHash
	"answer the hash of our second element"

	^ second hash.!

stbFixup: anSTBInFiler at: newObjectIndex
	"hook this to rehash ourselves on load from STB"

	self rehash.

	^ self.! !
!HashedPair categoriesFor: #<=!comparing!public! !
!HashedPair categoriesFor: #=!comparing!public! !
!HashedPair categoriesFor: #computeHash!accessing!helpers!initializing!private! !
!HashedPair categoriesFor: #copy!copying!public! !
!HashedPair categoriesFor: #displayOn:!displaying!public! !
!HashedPair categoriesFor: #first!accessing!public! !
!HashedPair categoriesFor: #first:!accessing!public! !
!HashedPair categoriesFor: #first:second:!accessing!initializing!public! !
!HashedPair categoriesFor: #firstHash!accessing!helpers!initializing!public! !
!HashedPair categoriesFor: #hash!accessing!public! !
!HashedPair categoriesFor: #postCopy!copying!public! !
!HashedPair categoriesFor: #printOn:!printing!public! !
!HashedPair categoriesFor: #rehash!operations!public! !
!HashedPair categoriesFor: #second!accessing!public! !
!HashedPair categoriesFor: #second:!accessing!public! !
!HashedPair categoriesFor: #secondHash!accessing!helpers!initializing!public! !
!HashedPair categoriesFor: #stbFixup:at:!binary filing!public! !

!HashedPair class methodsFor!

first: anObject second: anotherObject
	"answer a new instance with the given elements"

	^ (super new)
		first: anObject second: anotherObject;
		yourself.!

new
	"private -- use #first:second:"

	^ self shouldNotImplement.! !
!HashedPair class categoriesFor: #first:second:!instance creation!public! !
!HashedPair class categoriesFor: #new!instance creation!private! !

IdentityHashedPair guid: (GUID fromString: '{C0EA0849-A3BA-41E8-82C8-C88E4907F26F}')!
IdentityHashedPair comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Slight variant on HashedPair that uses identity hashing/comparison.'!
!IdentityHashedPair categoriesForClass!Unclassified! !
!IdentityHashedPair methodsFor!

= other
	"answer true iff the receiver is deemed to be the same as the <Object> other"

	"implementation note: 
	We don't use a shortcut == test since most instances of this
	class are expected to be short-lived.
	We don't bother to compare the hashes, since we are doing identity
	comparison of the objects themselves"
	^ self class == other class
		and: [first == other first
			and: [second == other second]].
!

firstHash
	"answer the hash of our first element"

	^ first identityHash.!

secondHash
	"answer the hash of our second element"

	^ second identityHash.! !
!IdentityHashedPair categoriesFor: #=!comparing!public! !
!IdentityHashedPair categoriesFor: #firstHash!accessing!helpers!initializing!public! !
!IdentityHashedPair categoriesFor: #secondHash!accessing!helpers!initializing!public! !

"Binary Globals"!

"Resources"!

