| package |
package := Package name: 'CU Extra Collections'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

Some additional Collections implementations.  They are:

BitArray and BooleanArray:
	Arrays of 1-bit quatities implemented as bitmaps.  Also allows some fast logical/boolean
	operations (xor, etc) on pairs of such arrays.

IntegerSet:
	A specialised Set for holding integers from a small(ish) range.  Implemented as a bitmap.
	Includes fast implementations of set operations (union, etc).

KeyedSet and KeyedIdentitySet:
	Specialised Sets that don''t send #hash (or #identityHash) to their elements, but
	to the result of applying  a pluggable Block (<monadicValuable>) to the element.
	The equality comparison (or identity comparison) between items is the same as for
	a normal Set.
	The point of this is that it allows (at the cost of perhaps increasing the nunber of
	collisions between items) to find and enumerate *several* items that share some
	property.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.06'.


package classNames
	add: #BitArray;
	add: #BitmapAbstract;
	add: #BooleanArray;
	add: #IntegerSet;
	add: #KeyedIdentitySet;
	add: #KeyedSet;
	add: #STBKeyedSetProxy;
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

ArrayedCollection variableByteSubclass: #BitmapAbstract
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BitmapAbstract variableByteSubclass: #BitArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BitmapAbstract variableByteSubclass: #BooleanArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Set variableSubclass: #KeyedSet
	instanceVariableNames: 'keyBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractSet variableSubclass: #IntegerSet
	instanceVariableNames: 'offset first last bitmap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IdentitySet variableSubclass: #KeyedIdentitySet
	instanceVariableNames: 'keyBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STBCollectionProxy subclass: #STBKeyedSetProxy
	instanceVariableNames: 'keyBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BitmapAbstract guid: (GUID fromString: '{78D65124-7B02-4E64-8A2F-B70F151D7D39}')!
BitmapAbstract comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Abstract class provding the implementation shared between BitArray and BooleanArray.'!
!BitmapAbstract categoriesForClass!Unclassified! !
!BitmapAbstract methodsFor!

& aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise AND of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy andIn: aBitmap]
		ifFalse: [aBitmap copy andIn: self].!

| aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise OR of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy orIn: aBitmap]
		ifFalse: [aBitmap copy orIn: self].
!

<< anInteger
	"answer a copy of the receiver shifted left by anInteger places"

	^ self shiftedBy: anInteger negated.!

= anObject
	"answer whether we are #= to anObject.  The implementation is the same as
	ByteArray since we can use the VM primitive for fast comparison.

	Primitive failure reasons:
		0 -	comperand is not the same class as the receiver."

	<primitive: 55>
	^ false.!

>> anInteger
	"answer a copy of the receiver shifted right by anInteger places"

	^ self shiftedBy: anInteger.!

andIn: aBitmap
	"set the elements of the receiver to be the elementwise AND of the original elements and those
	of aBitmap"

	| count |

	count := self basicSize min: aBitmap basicSize.

	1 to: count do: [:i | self basicAt: i put: ((self basicAt: i) bitAnd: (aBitmap basicAt: i))].
	count + 1 to: self basicSize do: [:i | self basicAt: i put: 0].
!

bitAnd: aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise AND of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy andIn: aBitmap]
		ifFalse: [aBitmap copy andIn: self].!

bitInvert
	"answer a copy of the receiver with each element inverted"

	^ self inverted.!

bitOr: aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise OR of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy orIn: aBitmap]
		ifFalse: [aBitmap copy orIn: self].!

bitShift: anInteger
	"answer a copy of the receiver shifted left by anInteger places.
	NB1: should be called #bitShifted: for consistancy.
	NB2: this shift is the opposite direction to #shiftedBy:"

	^ self shiftedBy: anInteger negated.
!

bitXor: aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise XOR of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy xorIn: aBitmap]
		ifFalse: [aBitmap copy xorIn: self].!

bytes
	"answer a ByteArray holding the same data as ourself in the sense
	that:
		(self class fromBytes: self bytes) = self
	will be true"

	| size |

	size := self basicSize.
	^ self
		replaceBytesOf: (ByteArray new: size) 
		from: 1 to: size startingAt: 1.!

byteSize
	"answer how many bytes are used by our packed representation"

	^ self basicSize.!

clearAll
	"clear (as oposed to set) the values held in the receiver"

	1 to: self basicSize do: [:i | self basicAt: i put: 0].!

clearAt: anIndex
	"clear (as opposed to set) the value stored at anIndex"

	| index mask byte bit |

	index := (anIndex - 1 bitShift: -3) + 1.
	bit := anIndex - 1 bitAnd: 7.

	byte := (self basicAt: index) bitAnd: (1 bitShift: bit) bitInvert.

	self basicAt: index put: byte.
!

clearFrom: anIndex to: anotherIndex
	"clear (as opposed to set) the values held in the receiver between the given
	indices"

	| start stop index |

	anIndex > anotherIndex ifTrue: [^ self].

	start := 1 + (anIndex + 6 bitShift: -3).	"index of first *whole* byte we want to treat"
	stop := anotherIndex  bitShift: -3.		"	,,  last	,,"

	index := (start - 1 bitShift: 3) min: anotherIndex.
	anIndex to: index do: [:i | self clearAt: i].
	index = anotherIndex ifTrue: [^ self].

	start to: stop do: [:i | self basicAt: i put: 0].

	index := 9 + (stop - 1 bitShift: 3).
	index to: anotherIndex do: [:i | self clearAt: i].
!

countEntries
	"answer how many elements are set"

	^ self countEntriesFrom: 1 to: self size.!

countEntriesFrom: anIndex to: anotherIndex
	"answer how many elements are set between the given indices"

	| count start stop byte |

	count := 0.

	anotherIndex < anIndex ifTrue: [^ count].

	"basic count by bytes, including all the bits in the bytes at both ends"
	start := (anIndex - 1 bitShift: -3) + 1.
	stop := (anotherIndex - 1 bitShift: -3) + 1.
	start to: stop do:
		[:i || bits |
		byte := self basicAt: i.
		bits := self class bitsInByte: byte.
		count := count + bits].

	"allow for bits we didn't want from the start"
	byte := self basicAt: start.
	1 to: (anIndex - 1 bitAnd: 7) do: [:i | count := count - (byte bitAt: i)].

	"or the end"
	byte := self basicAt: stop.
	(anotherIndex - 1 bitAnd: 7) + 2 to: 8 do: [:i | count := count - (byte bitAt: i)].

	^ count.!

firstIndex
	"answer the index of the first element that is set, or self size + 1 if there are none"

	| bit |

	bit := self lowBit.
	^ bit = 0
		ifTrue: [self size + 1]
		ifFalse: [bit].!

firstIndexAfter: anIndex
	"answer the index of the first element *after* anIndex that is set, or self size + 1 if there are none"

	^ self firstIndexFrom: anIndex + 1.!

firstIndexFrom: anIndex
	"answer the index of the first element *at or after* anInteger that is set, or self size + 1 if there are none"

	| start index |

	anIndex > self size ifTrue: [^ self size + 1].
	anIndex <= 1 ifTrue: [^ self firstIndex].

	start := 1 + (anIndex + 6 bitShift: -3).	"index of first *whole* byte we want to treat"

	index := 1 + (start - 1 bitShift: 3).
	anIndex to: index - 1 do: [:i | (self isSetAt: i) ifTrue: [^ i]].

	start to: self basicSize do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		byte = 0 ifFalse: [^ (basicIndex - 1 bitShift: 3) + byte lowBit]].

	^ self size + 1.
!

hash
	"answer the hash value of the receiver.  We use the same implementation
	as ByteArray is order to use the VM's primitive hashing"

	<primitive: 106>
	^ VMLibrary default hashBytes: self count: self basicSize.!

highBit
	"answer the index of the last element that is set, or zero if there are none"

	self basicSize to: 1 by: -1 do:
		[:index || byte |
		byte := self basicAt: index.
		byte = 0 ifFalse: [^ (index - 1 bitShift: 3) + byte highBit]].

	^ 0.!

invert
	"invert each element of the receiver"

	1 to: self basicSize do: [:i | self basicAt: i put: 255 - (self basicAt: i)].!

inverted
	"answer a copy of the receiver with each element inverted"

	^ self copy
		invert;
		yourself.!

isSetAt: anIndex
	"answer whether the entry at anIndex is set (as opposed to cleared)"

	| index |

	"this complicated expression is slightly faster than the clearer version
	using locals"
	index := anIndex - 1.
	^ (self basicAt: 1 + (index bitShift: -3)) allMask: (1 bitShift: (index bitAnd: 7)).
!

lastIndex
	"answer the index of the last element that is set, or zero if there are none"

	^ self highBit.!

lastIndexBefore: anIndex
	"answer the index of the last element *before* anInteger that is set, or zero if there are none"

	^ self lastIndexTo: anIndex - 1.!

lastIndexTo: anIndex
	"answer the index of the last element *at or before* anInteger that is set, or zero if there are none"

	| start stop index |

	anIndex < 1 ifTrue: [^ 0].
	anIndex >= self size ifTrue: [^ self lastIndex].

	stop := anIndex bitShift: -3.

	index := 9 + (stop - 1 bitShift: 3).
	anIndex to: index by: -1 do: [:i | (self isSetAt: i) ifTrue: [^ i]].

	stop to: 1 by: -1 do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		byte = 0 ifFalse: [^ (basicIndex - 1 bitShift: 3) + byte highBit]].

	^ 0.
!

lowBit
	"answer the index of the first element that is set, or zero if there are none"

	1 to: self basicSize do:
		[:index || byte |
		byte := self basicAt: index.
		byte = 0 ifFalse: [^ (index - 1 bitShift: 3) + byte lowBit]].

	^ 0.!

not
	"answer a copy of the receiver with each element inverted"

	^ self inverted.!

orIn: aBitmap
	"set the elements of the receiver to be the elementwise OR of the original elements and those
	of aBitmap"

	| count |

	count := self basicSize min: aBitmap basicSize.

	1 to: count do: [:i | self basicAt: i put: ((self basicAt: i) bitOr: (aBitmap basicAt: i))].!

replaceBytesOf: aByteObject from: start to: stop startingAt: fromStart
	"private -- standard method for transfering bytes from one variable
	byte object to another, normally double dispatched from #replaceFrom:to:with:startingAt:

	Primitive Failure Reasons:
		0 	- fromStart is not a SmallInteger.
		1	- stop is not a SmallInteger.
		2	- start is not a SmallInteger.
		3	- aByteObject is not a byte object
		4	- 'from' or 'to' interval is out-of-bounds
	"

	| fromOffset |
	<primitive: 142>
	fromOffset := fromStart - start.
	stop to: start by: -1 do: [:i | aByteObject at: i put: (self basicAt: i + fromOffset)].
	^aByteObject!

resize: aSize
	"change the size of the receiver to be at aSize.
	Note that we round up the new size to the nearest multiple of 8.
	Any new elements will be initialised to 0"

	| newSize |

	newSize := aSize + 7 bitShift: -3.

	newSize = self basicSize ifFalse: [super resize: newSize].!

setAll
	"set (as opposed to clear) the values held in the receiver"

	1 to: self basicSize do: [:i | self basicAt: i put: 255].!

setAt: anIndex
	"set (as opposed to clear) the value stored at anIndex"

	| index mask byte bit |

	index := (anIndex - 1 bitShift: -3) + 1.
	bit := anIndex - 1 bitAnd: 7.

	byte := (self basicAt: index) bitOr: (1 bitShift: bit).

	self basicAt: index put: byte.
!

setFrom: anIndex to: anotherIndex
	"set (as opposed to clear) the values held in the receiver between the given
	indices"

	| start stop index |

	anIndex > anotherIndex ifTrue: [^ self].

	start := 1 + (anIndex + 6 bitShift: -3).	"index of first *whole* byte we want to treat"
	stop := anotherIndex  bitShift: -3.		"	,,  last	,,"

	index := (start - 1 bitShift: 3) min: anotherIndex.
	anIndex to: index do: [:i | self setAt: i].
	index = anotherIndex ifTrue: [^ self].

	start to: stop do: [:i | self basicAt: i put: 255].

	index := 9 + (stop - 1 bitShift: 3).
	index to: anotherIndex do: [:i | self setAt: i].
!

shiftBy: anInteger
	"shift the elements of the receiver right by anInteger places.
	Note that the shift will grow the receiver by anInteger,
	if that is positive, or shrink it otherwise"

	anInteger < 0 ifTrue: [^ self shiftDownBy: anInteger negated].
	anInteger > 0 ifTrue: [^ self shiftUpBy: anInteger].

	"otherwise there's nothing to do"
!

shiftDownBy: anInteger
	"private -- shift the elements of the receiver down (left) by anInteger places,
	this will shrink the receiver, dropping the first anInteger elements.
	NB: anInteger *must* be strictly positive"

	| oldSize newSize |

	oldSize := self size.
	newSize := oldSize - anInteger.

	newSize <= 0 ifTrue: [^ self resize: 0].

	(anInteger bitAnd: 7) = 0
		ifTrue:
			["fast bytewise copy"
			| bytes |
			bytes := anInteger + 7 bitShift: -3.
			1 to: self basicSize - bytes do: [:i | self basicAt: i put: (self basicAt: i + bytes)]]
		ifFalse:
			["slow bitwise copy"
			1 to: newSize do: [:i | self at: i put: (self at: i + anInteger)]].

	self resize: newSize.				"the actual new size may be > newSize"
	self clearFrom: newSize + 1 to: self size.!

shiftedBy: anInteger
	"answer a copy of the receiver with each element shifted right by anInteger"

	^ self copy
		shiftBy: anInteger;
		yourself.
!

shiftUpBy: anInteger
	"private -- shift the elements of the receiver up (right) by anInteger places.
	This will cause the receiver to grow by anInteger elements, zero-padding
	at the start of the array.
	NB: anInteger *must* be strictly positive"

	| oldSize newSize |

	oldSize := self size.
	newSize := oldSize + anInteger.
	self resize: newSize.				"the actual new size may be > newSize"

	(anInteger bitAnd: 7) = 0
		ifTrue:
			["fast bytewise copy"
			| bytes |
			bytes := anInteger + 7 bitShift: -3.
			self basicSize to: bytes+1 by: -1 do: [:i | self basicAt: i put: (self basicAt: i - bytes)].
			1 to: self basicSize - bytes do: [:i | self basicAt: i put: 0]]
		ifFalse:
			["slow bitwise copy"
			newSize to: anInteger+1 by: -1 do: [:i | self at: i put: (self at: i - anInteger)].
			self clearFrom: 1 to: self size - anInteger].!

size
	"answer how many elements we hold.
	Note that this will always be a multiple of 8"

	^ super size bitShift: 3.!

species
	"answer the kind of Collection that should result from enumerations such as #collect:
	and #select: when applied against the receiver"

	^ Array.!

xor: aBitmap
	"answer a new Bitmap of the same size as the receiver, with elements that are the
	elementwise XOR of those of the receiver and those of aBitmap"

	^ self basicSize > aBitmap basicSize
		ifTrue: [self copy xorIn: aBitmap]
		ifFalse: [aBitmap copy xorIn: self].
!

xorIn: aBitmap
	"set the elements of the receiver to be the elementwise XOR of the original elements and those
	of aBitmap"

	| count |

	count := self basicSize min: aBitmap basicSize.

	1 to: count do: [:i | self basicAt: i put: ((self basicAt: i) bitXor: (aBitmap basicAt: i))].! !
!BitmapAbstract categoriesFor: #&!logical operations!public! !
!BitmapAbstract categoriesFor: #|!logical operations!public! !
!BitmapAbstract categoriesFor: #<<!bit manipulation!public! !
!BitmapAbstract categoriesFor: #=!comparing!primitives!public! !
!BitmapAbstract categoriesFor: #>>!bit manipulation!public! !
!BitmapAbstract categoriesFor: #andIn:!operations!public! !
!BitmapAbstract categoriesFor: #bitAnd:!bit manipulation!public! !
!BitmapAbstract categoriesFor: #bitInvert!bit manipulation!public! !
!BitmapAbstract categoriesFor: #bitOr:!bit manipulation!public! !
!BitmapAbstract categoriesFor: #bitShift:!bit manipulation!public! !
!BitmapAbstract categoriesFor: #bitXor:!bit manipulation!public! !
!BitmapAbstract categoriesFor: #bytes!accessing!public! !
!BitmapAbstract categoriesFor: #byteSize!accessing!public! !
!BitmapAbstract categoriesFor: #clearAll!operations!public! !
!BitmapAbstract categoriesFor: #clearAt:!operations!public! !
!BitmapAbstract categoriesFor: #clearFrom:to:!operations!public! !
!BitmapAbstract categoriesFor: #countEntries!accessing!public! !
!BitmapAbstract categoriesFor: #countEntriesFrom:to:!accessing!public! !
!BitmapAbstract categoriesFor: #firstIndex!public!searching! !
!BitmapAbstract categoriesFor: #firstIndexAfter:!public!searching! !
!BitmapAbstract categoriesFor: #firstIndexFrom:!public!searching! !
!BitmapAbstract categoriesFor: #hash!comparing!primitives!public! !
!BitmapAbstract categoriesFor: #highBit!bit manipulation!public! !
!BitmapAbstract categoriesFor: #invert!operations!public! !
!BitmapAbstract categoriesFor: #inverted!bit manipulation!logical operations!public! !
!BitmapAbstract categoriesFor: #isSetAt:!accessing!public!testing! !
!BitmapAbstract categoriesFor: #lastIndex!public!searching! !
!BitmapAbstract categoriesFor: #lastIndexBefore:!public!searching! !
!BitmapAbstract categoriesFor: #lastIndexTo:!public!searching! !
!BitmapAbstract categoriesFor: #lowBit!bit manipulation!public! !
!BitmapAbstract categoriesFor: #not!logical operations!public! !
!BitmapAbstract categoriesFor: #orIn:!operations!public! !
!BitmapAbstract categoriesFor: #replaceBytesOf:from:to:startingAt:!double dispatch!primitives!private! !
!BitmapAbstract categoriesFor: #resize:!operations!public! !
!BitmapAbstract categoriesFor: #setAll!accessing!public! !
!BitmapAbstract categoriesFor: #setAt:!operations!public! !
!BitmapAbstract categoriesFor: #setFrom:to:!operations!public! !
!BitmapAbstract categoriesFor: #shiftBy:!operations!public! !
!BitmapAbstract categoriesFor: #shiftDownBy:!operations!private! !
!BitmapAbstract categoriesFor: #shiftedBy:!bit manipulation!public! !
!BitmapAbstract categoriesFor: #shiftUpBy:!operations!private! !
!BitmapAbstract categoriesFor: #size!accessing!public! !
!BitmapAbstract categoriesFor: #species!accessing!public! !
!BitmapAbstract categoriesFor: #xor:!logical operations!public! !
!BitmapAbstract categoriesFor: #xorIn:!operations!public! !

!BitmapAbstract class methodsFor!

bitsInByte: aByte
	"answer how many bits are set in the given byte"

	^ self bitsInByteArray at: aByte + 1.!

bitsInByteArray
	"answer an array mapping bytes (+1) to the number of bits in them"

	^ ##(
		(0 to: 255) collect:
			[:byte | (1 to: 8) inject: 0 into: [:count :bit | count + (byte bitAt: bit)]]
	)!

fromBytes: aByteArray
	"answer a new instance with the same binary data as aByteArray; note that its
	#size will be 8 times greater than that of aByteArray"

	^ aByteArray
		replaceBytesOf: (self basicNew: aByteArray size)
		from: 1
		to: aByteArray size
		startingAt: 1.
!

new: aSize
	"answer a new instance holding aSize elements, all of
	which are set to 0.  Note that the size will be rounded
	up to the nearest multiple of 8"

	^ self basicNew: (aSize + 7 bitShift: -3).! !
!BitmapAbstract class categoriesFor: #bitsInByte:!helpers!public! !
!BitmapAbstract class categoriesFor: #bitsInByteArray!helpers!public! !
!BitmapAbstract class categoriesFor: #fromBytes:!instance creation!public! !
!BitmapAbstract class categoriesFor: #new:!instance creation!public! !

BitArray guid: (GUID fromString: '{35E5EC37-917E-4331-871B-95DC23587841}')!
BitArray comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

BitArray uses a packed (bitmap) representation to hold an array of 0 or 1 values.

It also provides bitwise operations similar to those on Integer, etc.

Most of the implementation is inherited from the superclass.'!
!BitArray categoriesForClass!Unclassified! !
!BitArray methodsFor!

at: anIndex
	"answer the 0 or 1 value stored at anIndex"

	| index byte bit |

	#CUtodo.  "should we just:
			^ (#isSetAt: anInteger) ifTrue: [1] ifFalse: [0].
		"

	index := anIndex - 1.
	byte := self basicAt: 1 + (index bitShift: -3).
	bit := byte bitShift: (0 - (index bitAnd: 7)).

	^ bit bitAnd: 1.!

at: anIndex put: anInteger
	"set the 0 or 1 value stored at anIndex"

	"overriden for efficiency"
	anInteger = 0
		ifTrue: [self clearAt: anIndex]
		ifFalse: [anInteger = 1
			ifTrue: [self setAt: anIndex]
			ifFalse: [self errorCantHold: anInteger]].
	^ anInteger.!

atAllPut: anInteger
	"replace every element of the receiver with anInteger, which must be
	either 0 or 1"

	"overriden for efficiency"
	anInteger = 0
		ifTrue: [self clearAll]
		ifFalse: [anInteger = 1
			ifTrue: [self setAll]
			ifFalse: [self errorCantHold: anInteger]].!

do: a1Block
	"evaluate the <monadicValuable> argument, operation, for each of the elements of the 
	receiver in index order, answering the receiver"

	"overridden for efficiency and because ArrayedCollection does so (which means our
	overriden version of #from:to:keysAndValuesDo: doesn't get called)"
	1 to: self basicSize do:
		[:index || byte |
		byte := self basicAt: index.
		7 timesRepeat:
			[a1Block value: (byte bitAnd: 1).
			byte := byte bitShift: -1].
		a1Block value: (byte bitAnd: 1)].!

keysAndValuesDo: a2Block
	"private -- evaluate a2Block for each index and element of the receiver in the given range"

	| index |

	"overriden for simplicity"
	index := 1.
	1 to: self basicSize do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		8 timesRepeat: 
			[a2Block value: index value: (byte bitAnd: 1).
			byte := byte bitShift: -1.
			index := index + 1]].!

uncheckedFrom: anIndex to: anotherIndex keysAndValuesDo: a2Block
	"private -- evaluate a2Block for each index and element of the receiver in the given range"

	| start stop index |

	anIndex > anotherIndex ifTrue: [^ self].

	index := (anIndex + 6 bitAnd: -8) + 1.	"index of first bit in first whole byte"
	super
		uncheckedFrom: anIndex
		to: (index - 1 min: anotherIndex)
		keysAndValuesDo: a2Block.

	start := 1 + (anIndex + 6 bitShift: -3).	"index of first whole byte"
	stop := anotherIndex bitShift: -3.		"index of last whole byte"
	start to: stop do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		8 timesRepeat: 
			[a2Block value: index value: (byte bitAnd: 1).
			byte := byte bitShift: -1.
			index := index + 1]].

	super
		uncheckedFrom: index
		to: anotherIndex
		keysAndValuesDo: a2Block.
! !
!BitArray categoriesFor: #at:!accessing!public! !
!BitArray categoriesFor: #at:put:!accessing!public! !
!BitArray categoriesFor: #atAllPut:!accessing!public! !
!BitArray categoriesFor: #do:!enumerating!public! !
!BitArray categoriesFor: #keysAndValuesDo:!enumerating!private! !
!BitArray categoriesFor: #uncheckedFrom:to:keysAndValuesDo:!enumerating!private! !

!BitArray class methodsFor!

fromFloat: aFloat
	"answer a new instance with the same binary data as aFloat"

	| bytes |

	"we assume that all Floats are 'doubles'"
	bytes := (ByteArray new: aFloat size)
			doubleAtOffset: 0 put: aFloat;
			yourself.

	^ self fromBytes: bytes.!

fromInteger: anInteger
	"answer a new instance with the same binary data as anInteger"

	^ self fromBytes: anInteger asByteArray.! !
!BitArray class categoriesFor: #fromFloat:!instance creation!public! !
!BitArray class categoriesFor: #fromInteger:!instance creation!public! !

BooleanArray guid: (GUID fromString: '{D48A8A49-5790-43DD-9C11-CA05B34FBF7B}')!
BooleanArray comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

BooleanArray uses a packed (bitmap) representation to hold an array of Boolean values.

It also provides logical operations similar to those on Boolean but applied elementwise to create a new BooleanArray.

Most of the implementation is inherited from the superclass.
'!
!BooleanArray categoriesForClass!Unclassified! !
!BooleanArray methodsFor!

at: anIndex
	"answer the Boolean value stored at anIndex"

	| index |

	#CUtodo.  "should we just:
			^ #isSetAt: anInteger.
		"

	"this complicated expression is slightly faster than the clearer version
	using locals"
	index := anIndex - 1.
	^ (self basicAt: 1 + (index bitShift: -3)) allMask: (1 bitShift: (index bitAnd: 7)).
!

at: anIndex put: aBool
	"set the Boolean value stored at anIndex"

	aBool
		ifTrue: [self setAt: anIndex]
		ifFalse: [self clearAt: anIndex].

	^ aBool.!

atAllPut: aBool
	"replace every element of the receiver with aBool"

	aBool
		ifTrue: [self setAll]
		ifFalse: [self clearAll].!

do: a1Block
	"evaluate the <monadicValuable> argument, operation, for each of the elements of the 
	receiver in index order, answering the receiver"

	"overridden for efficiency and because ArrayedCollection does so (which means our
	overriden version of #from:to:keysAndValuesDo: doesn't get called)"
	1 to: self basicSize do:
		[:index || byte |
		byte := self basicAt: index.
		#[1 2 4 8 16 32 64 128] do:
			[:bit | a1Block value: (byte allMask: bit)]].!

keysAndValuesDo: a2Block
	"evaluate a2Block for each index and element of the receiver"

	| index |

	"overriden for simplicity"
	index := 1.
	1 to: self basicSize do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		#(1 2 4 8 16 32 64 128) do:
			[:bit |
			a2Block value: index value: (byte allMask: bit).
			index := index + 1]].!

uncheckedFrom: anIndex to: anotherIndex keysAndValuesDo: a2Block
	"private -- evaluate a2Block for each index and element of the receiver in the given range"

	| start stop index |

	anIndex > anotherIndex ifTrue: [^ self].

	index := (anIndex + 6 bitAnd: -8) + 1.	"index of first bit in first whole byte"
	super
		uncheckedFrom: anIndex
		to: (index - 1 min: anotherIndex)
		keysAndValuesDo: a2Block.

	start := 1 + (anIndex + 6 bitShift: -3).	"index of first whole byte"
	stop := anotherIndex bitShift: -3.		"index of last whole byte"
	start to: stop do:
		[:basicIndex || byte |
		byte := self basicAt: basicIndex.
		#(1 2 4 8 16 32 64 128) do:
			[:bit |
			a2Block value: index value: (byte allMask: bit).
			index := index + 1]].

	super
		uncheckedFrom: index
		to: anotherIndex
		keysAndValuesDo: a2Block.
! !
!BooleanArray categoriesFor: #at:!accessing!public! !
!BooleanArray categoriesFor: #at:put:!accessing!public! !
!BooleanArray categoriesFor: #atAllPut:!accessing!public! !
!BooleanArray categoriesFor: #do:!enumerating!public! !
!BooleanArray categoriesFor: #keysAndValuesDo:!enumerating!public! !
!BooleanArray categoriesFor: #uncheckedFrom:to:keysAndValuesDo:!enumerating!private! !

KeyedSet guid: (GUID fromString: '{E1B4AED0-0BBB-4694-A51D-D732750600A4}')!
KeyedSet comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A KeyedSet is a Set that uses a key instead of just sending #hash to its elements.  I.e. it evaluates its #keyBlock with the element as parameter, and then sends #hash to the result.  The comparison between elements uses #= just as for a normal Set.

We require that if two elements are #=, then they must have #= keys.

The important extra ability that this gives (over just using a PluggableSet) is that we can then implement #elementsWithKey: and its relations, and so quickly enumerate all the elements that have the same value for the ''key''.  (Albeit at some potential cost in the number of hash collisions between items in the set).'!
!KeyedSet categoriesForClass!Unclassified! !
!KeyedSet methodsFor!

elementsWithKey: aKey
	"answer a Set of each element that has (up to equality) the same key as aKey"

	| answer |

	answer := self newSelection.
	self elementsWithKey: aKey do: [:each | answer add: each].

	^ answer.!

elementsWithKey: aKey do: a1Block
	"evaluate a1Block for each element that has (up to equality)
	the same key as aKey"

	| capacity index element |

	capacity := self basicSize.
	index := self hashKey: aKey max: capacity.
	[element := self basicAt: index.
	element isNil
		ifTrue: [^ self]
		ifFalse: [aKey = (keyBlock value: element)
			ifTrue: [a1Block value: element]].
	index := index \\ capacity + 1]
		repeat.
!

elementsWithKey: aKey do: a1Block ifNone: a0Block
	"answer the last result of evaluating a1Block for each element that has (up to equality)
	the same key as aKey.  If there are none, then of evaluating a0Block"

	| last none |

	none := true.
	self elementsWithKey: aKey do: [:each | none := false. last := a1Block value: each].
	^ none
		ifTrue: [a0Block value]
		ifFalse: [last].!

hash: anObject max: anInteger

	^ self hashKey: (keyBlock value: anObject) max: anInteger.!

hashKey: aKey max: anInteger

	^ aKey hash \\ anInteger + 1.!

keyBlock
	"answer the <monadicValuable> that is applied to elements in order to obtain a 'key'
	to hash"

	^ keyBlock.!

keyBlock: a1Block
	"private -- set the <monadicValuable> that is applied to elements in order to obtain a 'key'
	to hash"

	keyBlock := a1Block.!

preResize: newMe
	"private -- we are about to be resized into newMe; we must also copy the keyBlock"

	newMe keyBlock: keyBlock.!

species
	"answer the class of object to be used when copying the receiver with #collect:, etc."

	^ Set.
!

stbSaveOn: anSTBOutFiler
	"overriden to use an appropriate proxy"

	anSTBOutFiler saveObject: self as: (STBKeyedSetProxy for: self)! !
!KeyedSet categoriesFor: #elementsWithKey:!public!searching! !
!KeyedSet categoriesFor: #elementsWithKey:do:!enumerating!public! !
!KeyedSet categoriesFor: #elementsWithKey:do:ifNone:!enumerating!public! !
!KeyedSet categoriesFor: #hash:max:!private!searching! !
!KeyedSet categoriesFor: #hashKey:max:!private!searching! !
!KeyedSet categoriesFor: #keyBlock!accessing!public! !
!KeyedSet categoriesFor: #keyBlock:!initializing!private! !
!KeyedSet categoriesFor: #preResize:!adding!private! !
!KeyedSet categoriesFor: #species!constants!public! !
!KeyedSet categoriesFor: #stbSaveOn:!binary filing!public! !

!KeyedSet class methodsFor!

new: anInteger withKeyBlock: a1Block
	"answer a new instance with the given suggested size, and which uses the given key block"

	^ (self new: anInteger)
		keyBlock: a1Block;
		yourself.!

withKeyBlock: a1Block
	"answer a new instance that uses the given key block"

	^ (self new)
		keyBlock: a1Block;
		yourself.! !
!KeyedSet class categoriesFor: #new:withKeyBlock:!instance creation!public! !
!KeyedSet class categoriesFor: #withKeyBlock:!instance creation!public! !

IntegerSet guid: (GUID fromString: '{DF42C886-681F-4C84-B71A-D23E9D541460}')!
IntegerSet comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

A kind of set that is able to hold only integers from a small(-ish) range.  It is implemented as a bitmap internally, and so can be very fast for many operations.

Of course, this is not a suitable representation for large sparse sets, since it would use a great deal of space.  However if the density of the set occupation (the number of elements divided by the range of possible elements) is higher than about 1/64 then it will be faster *and* more space efficient than a "vanilla" Set (with a 1/2 load factor).

Note that we have a specialised implementation of the set-theoretic operations (union, intersection, etc) which makes direct use the internal bitmaps to gain speed.'!
!IntegerSet categoriesForClass!Unclassified! !
!IntegerSet methodsFor!

- aComparand
	"overriden to use the optimised form if possible"

	^ (aComparand isKindOf: self class)
		ifTrue: [self fastDifference: aComparand]
		ifFalse: [super - aComparand].!

add: anInteger
	"one of the root methods for <Collection>"

	| index |

	self ensureCoverageOf: anInteger.

	index := anInteger + offset.
	first > index ifTrue: [first := index].
	last < index ifTrue: [last := index].
	bitmap at: index put: true.

	^ anInteger.
!

addAll: aCollection
	"overriden for efficiency"

	| low high |

	low := first - offset.
	high := last - offset.
	aCollection do:
		[:each |
		each < low ifTrue: [low := each].
		each > high ifTrue: [high := each]].

	self ensureCoverageFrom: low to: high.

	first := low + offset.
	last := high + offset.
	aCollection do: [:each | bitmap at: each + offset put: true].

	^ aCollection.
!

addFrom: anInteger to: anotherInteger
	"add all the integers from the given range"

	| from to |

	anInteger > anotherInteger ifTrue: [^ self].

	self ensureCoverageFrom: anInteger to: anotherInteger.

	from := anInteger + offset.
	to := anotherInteger + offset.

	first := first min: from.
	last := last max: to.
	bitmap setFrom: from to: to.!

approxSize
	"private -- answer the approximate size of the receiver"

	^ last - first + 1 min: 0.!

averageProbesPerElement
	"overriden since the inherited implementation makes no sense.
	Of course, neither does this, but it looks good..."

	^ 1.!

basicFirst
	"private -- anwer the first element in this collection or the sentiel value if we are empty"

	^ first - offset.!

basicLast
	"private -- anwer the last element in this collection of 0 if we are empty"

	^ last - offset.!

bitmap
	"private -- answer our bitmap"

	^ bitmap.!

bitmap: aBitmap
	"private -- set our internal bitmap to that given"

	bitmap := aBitmap.!

capacity
	"answer how many Integers we have room for (including the ones we already have)"

	^ bitmap size.!

checkFirstAndLast
	"private -- look to see whether our first and last fields are correct.  If not
	then fix them and see if we need to shrink any"

	| shrunk index |

	shrunk := false.

	index := bitmap firstIndexFrom: first.
	index = first ifFalse: [first := index. shrunk := true].

	index := bitmap lastIndexTo: last.
	index = last ifFalse: [last := index. shrunk := true].

	shrunk ifTrue: [self maybeShrink].
!

coverage
	"answer what range of integers we could hold without growing our underling bitmap"

	| start |

	start := 1 + offset negated.

	^ start to: start + self capacity - 1.!

coverFrom: anInteger to: anotherInteger
	"private -- set the range we cover to that specified (or possibly a little more
	to allow for rounding)"

	| isEmpty newOffset offsetDelta newSize sizeDelta |

	"NB: the rounding here needs to be kep in synch with the calculations in #wastage"

	isEmpty := self isEmpty.

	newOffset := 1 - (anInteger bitAnd: -32).		"round down to multiple of 32"
	offsetDelta := newOffset - offset.
	offsetDelta = 0 ifFalse:
		[isEmpty ifFalse: [bitmap shiftBy: offsetDelta].
		first := first + offsetDelta.
		last := last + offsetDelta.
		offset := newOffset].

	newSize := (anotherInteger + offset + 31) bitAnd: -32.		"round up to multiple of 32"
	sizeDelta := newSize - bitmap size.
	sizeDelta = 0 ifFalse:
		[bitmap resize: newSize.
		first > newSize ifTrue: [first := newSize].
		last > newSize ifTrue: [last := newSize]].

	isEmpty ifTrue:
		[first := bitmap size + 1.
		last := 0].!

density
	"answer how densly populated we are"

	^ self capacity = 0
		ifTrue: [0]
		ifFalse: [self size asFloat / self capacity].!

difference: aComparand
	"overriden to use the optimised form if possible"

	^ (aComparand isKindOf: self class)
		ifTrue: [self fastDifference: aComparand]
		ifFalse: [super difference: aComparand].!

do: a1Block
	"one of the root methods for <Collection>"

	bitmap from: first to: last keysAndValuesDo: [:i :b | b ifTrue: [a1Block value: i - offset]].!

ensureCoverageFrom: anInteger to: anotherInteger
	"private -- ensure that we cover the given range"

	(anInteger + offset >= 1 and: [anotherInteger + offset <= bitmap size]) ifFalse:
		[| from to |
		from := anInteger min: first - offset.
		to := anotherInteger max: last - offset.
		self coverFrom: from to: to].!

ensureCoverageOf: anInteger
	"private -- ensue that anInteger is included in our coverage"

	self ensureCoverageFrom: anInteger to: anInteger.!

errorNotFound
	"private -- raise a NotFoundError indicating that it was impossible to remove an
	arbitrary object because we are empty"

	^ NotFoundError new
		receiver: self;
		signal.!

fastDifference: anIntegerSet
	"answer the set-theoretic difference of this set and anIntegerSet"

	| newBitmap new |

	newBitmap := (bitmap inverted)
				orIn: (anIntegerSet bitmap >> (offset - anIntegerSet offset));
				invert;
				yourself.

	new := self class
			bitmap: newBitmap
			offset: offset
			first: self basicFirst
			last: self basicLast.

	new checkFirstAndLast.	"may make it shrink too"

	^ new.
!

fastIntersection: anIntegerSet
	"answer the set-theoretic intersection of this set and anIntegerSet"

	| newOffset newBitmap new |

	offset > anIntegerSet offset ifTrue: [^ anIntegerSet fastIntersection: self].

	newOffset := anIntegerSet offset.
	newBitmap := (bitmap copy)
				shiftBy: (newOffset  - offset);
				andIn: anIntegerSet bitmap;
				yourself.

	new := self class
			bitmap: newBitmap
			offset: newOffset
			first: (self basicFirst min: anIntegerSet basicFirst)			"we could tighten these by reversing #max: and"
			last: (self basicLast max: anIntegerSet basicLast).			"#min: but that would suppress the shrinking"

	new checkFirstAndLast.	"may make it shrink too"

	^ new.
!

fastSymmetricDifference: anIntegerSet
	"answer the set-theoretic symmetric difference of this set and anIntegerSet"

	| newOffset newBitmap new |

	offset > anIntegerSet offset ifTrue: [^ anIntegerSet fastSymmetricDifference: self].

	newOffset := anIntegerSet offset.
	newBitmap := (bitmap copy)
				shiftBy: (newOffset  - offset);
				xorIn: anIntegerSet bitmap;
				yourself.

	new := self class
			bitmap: newBitmap
			offset: newOffset
			first: (self basicFirst min: anIntegerSet basicFirst)
			last: (self basicLast max: anIntegerSet basicLast).

	new checkFirstAndLast.	"may make it shrink too"

	^ new.!

fastUnion: anIntegerSet
	"answer the set-theoretic union of this set and anIntegerSet"

	| newOffset newBitmap |

	offset > anIntegerSet offset ifTrue: [^ anIntegerSet fastUnion: self].

	newOffset := anIntegerSet offset.
	newBitmap := (bitmap copy)
				shiftBy: (newOffset  - offset);
				orIn: anIntegerSet bitmap;
				yourself.

	^ self class
		bitmap: newBitmap
		offset: newOffset
		first: (self basicFirst min: anIntegerSet basicFirst)
		last: (self basicLast max: anIntegerSet basicLast).!

fill
	"private -- add every element that we cover to this set"

	first := 1.
	last := bitmap size.
	bitmap setAll.!

first: anInteger
	"private -- set our record of what element is first"

	first := anInteger + offset.!

firstIfNone: a0Block
	"answer the first (i.e least) element in the set, or the result of evaluating
	a0Block if we are empty"

	^ first <= last
		ifTrue: [first - offset]
		ifFalse: [a0Block value].!

includes: anInteger
	"one of the root methods for <Set>.
	NB: this is a cheating implementation since:
	a)	it will throw errors rather than answer false with a non integer argument;
	b)	it will not admit that it contains a Float even if that float is #= to one of our elements"

	| index |

	index := anInteger + offset.
	index < first ifTrue: [^ false].
	index > last ifTrue: [^ false].

	^ bitmap at: index.
!

initialize
	"private -- establish a coherent initial state"

	bitmap := BooleanArray new: 0.		"will be reallocated almost immediately"
	offset := 0.
	first := 1.
	last := 0.
!

intersection: aComparand
	"overriden to use the optimised form if possible"

	^ (aComparand isKindOf: self class)
		ifTrue: [self fastIntersection: aComparand]
		ifFalse: [super intersection: aComparand].!

isEmpty
	"answer whether we have no elements"

	"overriden for efficiency (!!)"
	^ last < first.!

last: anInteger
	"private -- set our record of what element is last"

	last := anInteger + offset.	!

lastIfNone: a0Block
	"answer the last (i.e. greatest) element in the set, or the result of evaluating
	a0Block if we are empty"

	^ first <= last
		ifTrue: [last - offset]
		ifFalse: [a0Block value].!

maybeShrink
	"private -- look to see if it's likely that we can usefully be shrunk, and if so then do it"

	self wastage >= 64 ifTrue: [self shrink].!

offset
	"private -- answer our offset"

	^ offset.!

offset: anInteger
	"private -- set our internal offset to that given"

	offset := anInteger.!

remove: anInteger ifAbsent: a0Block
	"one of the root methods for <Collection>"

	| index |

	index := anInteger + offset.
	(index < first or: [index > last or: [(bitmap at: index) not]])
		ifTrue: [^ a0Block value].

	bitmap at: index put: false.

	self checkFirstAndLast.

	^ anInteger.!

removeFirst
	"remove the first (i.e least) element from this set, answering the element, or throw
	an error if we're empty"

	^ self removeFirstIfAbsent: [self errorNotFound].!

removeFirstIfAbsent: a0Block
	"remove the first (i.e least) element from this set, answering the element, or the
	result of evaluating a0Block if we are emty"

	| removed |

	last < first ifTrue: [^ a0Block value].

	bitmap at: first put: false.
	removed := first - offset.

	first := bitmap firstIndexAfter: first.
	self maybeShrink.

	^ removed.!

removeFrom: anInteger
	"ensure that we hold only elements that are less than anInteger"

	self removeFrom: anInteger to: (self lastIfNone: [^ self]).!

removeFrom: anInteger to: anotherInteger
	"ensure that we hold no elements in the given range"

	| from to shrunk |

	from := anInteger + offset max: first.
	to := anotherInteger + offset min: last.
	from > to ifTrue: [^ self].

	bitmap clearFrom: from to: to.

	self checkFirstAndLast.!

removeLast
	"remove the last (i.e greatest) element from this set, answering the element, or throw
	an error if we're empty"

	^ self removeLastIfAbsent: [self errorNotFound].!

removeLastIfAbsent: a0Block
	"remove the last (i.e greatest) element from this set, answering the element, or the
	result of evaluating a0Block if we are emty"

	| removed |

	last < first ifTrue: [^ a0Block value].

	bitmap at: last put: false.
	removed := last - offset.

	last := bitmap lastIndexBefore: last.
	self maybeShrink.

	^ removed.!

removeTo: anInteger
	"ensure that we hold only elements that are greater than anInteger"

	self removeFrom: (self firstIfNone: [^ self]) to: anInteger.!

shrink
	"reduce the space we consume to the convenient minumum"

	self coverFrom: (self firstIfNone: [0]) to: (self firstIfNone: [0]).!

size
	"one of the root methods for <Collection>"

	^ bitmap countEntriesFrom: first to: last.!

symmetricDifference: aComparand
	"overriden to use the optimised form if possible"

	^ (aComparand isKindOf: self class)
		ifTrue: [self fastSymmetricDifference: aComparand]
		ifFalse: [super symmetricDifference: aComparand].!

union: aComparand
	"overriden to use the optimised form if possible"

	^ (aComparand isKindOf: self class)
		ifTrue: [self fastUnion: aComparand]
		ifFalse: [super union: aComparand].
!

wastage
	"answer how many entries could be recovered by a #shrink operation"

	"this calculation needs to be kept in synch with #coverFrom:to:"
	^ (first bitAnd: -32) + (bitmap size - (last + 31 bitAnd: -32)).! !
!IntegerSet categoriesFor: #-!public!set operations! !
!IntegerSet categoriesFor: #add:!adding!public! !
!IntegerSet categoriesFor: #addAll:!adding!public! !
!IntegerSet categoriesFor: #addFrom:to:!adding!public! !
!IntegerSet categoriesFor: #approxSize!accessing!private! !
!IntegerSet categoriesFor: #averageProbesPerElement!development!public!statistics! !
!IntegerSet categoriesFor: #basicFirst!accessing!private! !
!IntegerSet categoriesFor: #basicLast!accessing!private! !
!IntegerSet categoriesFor: #bitmap!accessing!private! !
!IntegerSet categoriesFor: #bitmap:!initializing!private! !
!IntegerSet categoriesFor: #capacity!accessing!public! !
!IntegerSet categoriesFor: #checkFirstAndLast!helpers!private! !
!IntegerSet categoriesFor: #coverage!accessing!public! !
!IntegerSet categoriesFor: #coverFrom:to:!helpers!private! !
!IntegerSet categoriesFor: #density!accessing!public! !
!IntegerSet categoriesFor: #difference:!public!set operations! !
!IntegerSet categoriesFor: #do:!enumerating!public! !
!IntegerSet categoriesFor: #ensureCoverageFrom:to:!helpers!private! !
!IntegerSet categoriesFor: #ensureCoverageOf:!helpers!private! !
!IntegerSet categoriesFor: #errorNotFound!exceptions!private! !
!IntegerSet categoriesFor: #fastDifference:!public!set operations! !
!IntegerSet categoriesFor: #fastIntersection:!public!set operations! !
!IntegerSet categoriesFor: #fastSymmetricDifference:!public!set operations! !
!IntegerSet categoriesFor: #fastUnion:!public!set operations! !
!IntegerSet categoriesFor: #fill!operations!private! !
!IntegerSet categoriesFor: #first:!initializing!private! !
!IntegerSet categoriesFor: #firstIfNone:!accessing!public! !
!IntegerSet categoriesFor: #includes:!public!searching!testing! !
!IntegerSet categoriesFor: #initialize!initializing!private! !
!IntegerSet categoriesFor: #intersection:!public!set operations! !
!IntegerSet categoriesFor: #isEmpty!public!testing! !
!IntegerSet categoriesFor: #last:!initializing!private! !
!IntegerSet categoriesFor: #lastIfNone:!accessing!public! !
!IntegerSet categoriesFor: #maybeShrink!helpers!private! !
!IntegerSet categoriesFor: #offset!accessing!private! !
!IntegerSet categoriesFor: #offset:!initializing!private! !
!IntegerSet categoriesFor: #remove:ifAbsent:!public!removing! !
!IntegerSet categoriesFor: #removeFirst!public!removing! !
!IntegerSet categoriesFor: #removeFirstIfAbsent:!public!removing! !
!IntegerSet categoriesFor: #removeFrom:!public!removing! !
!IntegerSet categoriesFor: #removeFrom:to:!public!removing! !
!IntegerSet categoriesFor: #removeLast!public!removing! !
!IntegerSet categoriesFor: #removeLastIfAbsent:!public!removing! !
!IntegerSet categoriesFor: #removeTo:!public!removing! !
!IntegerSet categoriesFor: #shrink!operations!public! !
!IntegerSet categoriesFor: #size!accessing!public! !
!IntegerSet categoriesFor: #symmetricDifference:!public!set operations! !
!IntegerSet categoriesFor: #union:!public!set operations! !
!IntegerSet categoriesFor: #wastage!accessing!public! !

!IntegerSet class methodsFor!

bitmap: aBooleanArray offset: anOffset first: anInteger last: anotherInteger
	"private -- answer a new instance with the given internal data"

	^ (self basicNew: 0)
		bitmap: aBooleanArray;
		offset: anOffset;
		first: anInteger;
		last: anotherInteger;
		yourself.!

from: anInteger to: anotherInteger
	"answer a new instance initially configured to cover the given range"

	^ (self basicNew: 0)
		initialize;
		coverFrom: anInteger to: anotherInteger;
		yourself.
!

new
	"answer a new instance of the receiver with the default initial capacity"

	"we may as well start with a 32-bit bitmap's worth..."
	^ self new: 32.!

new: count
	"answer a new instance of the receiver with an initial capacity to hold
	integers in the range (0 to: count - 1)"

	^ self from: 0 to: count - 1.!

range: anInterval
	"answer a new instance initially configured to cover the given range"

	^ self from: anInterval start to: anInterval stop.!

withAllFrom: anInteger to: anotherInteger
	"answer a new instance containing all the integers in the given range"

	^ (self from: anInteger to: anotherInteger)
		addFrom: anInteger to: anotherInteger;
		yourself.
! !
!IntegerSet class categoriesFor: #bitmap:offset:first:last:!instance creation!private! !
!IntegerSet class categoriesFor: #from:to:!instance creation!public! !
!IntegerSet class categoriesFor: #new!instance creation!public! !
!IntegerSet class categoriesFor: #new:!instance creation!public! !
!IntegerSet class categoriesFor: #range:!instance creation!public! !
!IntegerSet class categoriesFor: #withAllFrom:to:!instance creation!public! !

KeyedIdentitySet guid: (GUID fromString: '{9CC0946A-A821-4768-99E4-952FF3B5BAD5}')!
KeyedIdentitySet comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

A KeyedSet is an IdentitySet that uses a key instead of just sending #identityHash to its elements.  I.e. it evaluates its #keyBlock with the element as parameter, and then sends #hash (*NOT* #identityHash) to the result.   Items themselves are compared using identity just as for an IdentitySet.

The important extra ability that this gives is that we can provide fast implementations fo #elementsWithKey: etc.'!
!KeyedIdentitySet categoriesForClass!Unclassified! !
!KeyedIdentitySet methodsFor!

elementsWithKey: aKey
	"answer a Set of each element that has (up to equality, *NOT* identity)
	the same key as aKey"

	| answer |

	answer := self newSelection.
	self elementsWithKey: aKey do: [:each | answer add: each].

	^ answer.!

elementsWithKey: aKey do: a1Block
	"evaluate a1Block for each element that has (up to equality, *NOT* identity)
	the same key as aKey"

	| capacity index element |

	capacity := self basicSize.
	index := self hashKey: aKey max: capacity.
	[element := self basicAt: index.
	element isNil
		ifTrue: [^ self]
		ifFalse: [aKey = (keyBlock value: element)
			ifTrue: [a1Block value: element]].
	index := index \\ capacity + 1]
		repeat.
!

elementsWithKey: aKey do: a1Block ifNone: a0Block
	"answer the last result of evaluating a1Block for each element that has (up to equality, *NOT* identity)
	the same key as aKey.  If there are none, then of evaluating a0Block"

	| last none |

	none := true.
	self elementsWithKey: aKey do: [:each | none := false. last := a1Block value: each].
	^ none
		ifTrue: [a0Block value]
		ifFalse: [last].!

hash: anObject max: anInteger

	^ self hashKey: (keyBlock value: anObject) max: anInteger.!

hashKey: aKey max: anInteger

	^ aKey hash \\ anInteger + 1.!

keyBlock
	"answer the <monadicValuable> that is applied to elements in order to obtain a 'key'
	to hash"

	^ keyBlock.!

keyBlock: a1Block
	"private -- set the <monadicValuable> that is applied to elements in order to obtain a 'key'
	to hash"

	keyBlock := a1Block.!

preResize: newMe
	"private -- we are about to be resized into newMe; we must also copy the key block"

	newMe keyBlock: keyBlock.!

species
	"answer the class of object to be used when copying the receiver with #collect:, etc."

	^ IdentitySet.
!

stbSaveOn: anSTBOutFiler
	"overriden to use an appropriate proxy"

	anSTBOutFiler saveObject: self as: (STBKeyedSetProxy for: self)! !
!KeyedIdentitySet categoriesFor: #elementsWithKey:!public!searching! !
!KeyedIdentitySet categoriesFor: #elementsWithKey:do:!enumerating!public! !
!KeyedIdentitySet categoriesFor: #elementsWithKey:do:ifNone:!enumerating!public! !
!KeyedIdentitySet categoriesFor: #hash:max:!public!searching! !
!KeyedIdentitySet categoriesFor: #hashKey:max:!private!searching! !
!KeyedIdentitySet categoriesFor: #keyBlock!accessing!public! !
!KeyedIdentitySet categoriesFor: #keyBlock:!initializing!private! !
!KeyedIdentitySet categoriesFor: #preResize:!adding!private! !
!KeyedIdentitySet categoriesFor: #species!constants!public! !
!KeyedIdentitySet categoriesFor: #stbSaveOn:!binary filing!public! !

!KeyedIdentitySet class methodsFor!

new: anInteger withKeyBlock: a1Block
	"answer a new instance with the given suggested size, and which uses the given key block"

	^ (self new: anInteger)
		keyBlock: a1Block;
		yourself.!

withKeyBlock: a1Block
	"answer a new instance that uses the given key block"

	^ (self new)
		keyBlock: a1Block;
		yourself.! !
!KeyedIdentitySet class categoriesFor: #new:withKeyBlock:!instance creation!public! !
!KeyedIdentitySet class categoriesFor: #withKeyBlock:!instance creation!public! !

STBKeyedSetProxy guid: (GUID fromString: '{279F10BD-C4AB-4219-83F2-2BA22CA38C8B}')!
STBKeyedSetProxy comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Used by Ked[Identiy]Sets to implement STB-ing.'!
!STBKeyedSetProxy categoriesForClass!Unclassified! !
!STBKeyedSetProxy methodsFor!

keyBlock: anObject

	keyBlock := anObject.!

value

	^ (class withKeyBlock: keyBlock)
		addAll: array;
		yourself.! !
!STBKeyedSetProxy categoriesFor: #keyBlock:!accessing!public! !
!STBKeyedSetProxy categoriesFor: #value!converting!public! !

!STBKeyedSetProxy class methodsFor!

for: aKeyedSet

	^ (super for: aKeyedSet)
		keyBlock: aKeyedSet keyBlock;
		yourself! !
!STBKeyedSetProxy class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

