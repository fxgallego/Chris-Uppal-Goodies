| package |
package := Package name: 'CU Stream Extensions'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004-2005.
chris.uppal@metagnostic.org

A few loose methods for Read and WriteStream that should be in the base image, but -- for some reason -- aren''t.

(For such trivia, the following seems a bit petty, I admit!!)

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

2.00
-	Added a few more methods.
-	Added big-endian versions of the methods.

1.00
-	First release.
'.

package basicPackageVersion: '2.00'.


package methodNames
	add: #PositionableStream -> #nextBeDOUBLE;
	add: #PositionableStream -> #nextBeDWORD;
	add: #PositionableStream -> #nextBeFLOAT;
	add: #PositionableStream -> #nextBeQWORD;
	add: #PositionableStream -> #nextBeSDWORD;
	add: #PositionableStream -> #nextBeSQWORD;
	add: #PositionableStream -> #nextBeSWORD;
	add: #PositionableStream -> #nextBeWORD;
	add: #PositionableStream -> #nextFLOAT;
	add: #PositionableStream -> #nextQWORD;
	add: #PositionableStream -> #nextSQWORD;
	add: #WriteStream -> #nextBeDOUBLEPut:;
	add: #WriteStream -> #nextBeDWORDPut:;
	add: #WriteStream -> #nextBeFLOATPut:;
	add: #WriteStream -> #nextBeQWORDPut:;
	add: #WriteStream -> #nextBeSDWORDPut:;
	add: #WriteStream -> #nextBeSQWORDPut:;
	add: #WriteStream -> #nextBeSWORDPut:;
	add: #WriteStream -> #nextBeWORDPut:;
	add: #WriteStream -> #nextDOUBLEPut:;
	add: #WriteStream -> #nextDWORDPut:;
	add: #WriteStream -> #nextFLOATPut:;
	add: #WriteStream -> #nextQWORDPut:;
	add: #WriteStream -> #nextSQWORDPut:;
	add: #WriteStream -> #nextSWORDPut:;
	add: #WriteStream -> #nextWORDPut:;
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


"Global Aliases"!


"Loose Methods"!

!PositionableStream methodsFor!

nextBeDOUBLE
	"Answer a double precision floating pointer number constructed from the big-endian
	representation contained in the next 8 bytes on the receiver."

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes doubleAtOffset: 0!

nextBeDWORD
	"Answer a 32-bit unsigned integer constructed from the big-endian
	representation contained in the next 4 bytes on the receiver."

#CUadded.
	^ (((self next bitShift: 24)
		bitOr: (self next bitShift: 16))
			bitOr: (self next bitShift: 8))
				bitOr: self next.!

nextBeFLOAT
	"Answer a single precision floating pointer number constructed from the big-endian
	representation contained in the next 4 bytes on the receiver."

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes floatAtOffset: 0!

nextBeQWORD
	"Answer a 64-bit unsigned integer constructed from the big-endian
	representation contained in the next 8 bytes on the receiver."

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes qwordAtOffset: 0!

nextBeSDWORD
	"Answer a 32-bit signed integer constructed from the big-endian
	representation contained in the next 4 bytes on the receiver."

	| bytes |

#CUadded.
	bytes := (ByteArray new: 4)
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sdwordAtOffset: 0!

nextBeSQWORD
	"Answer a 64-bit signed integer constructed from the big-endian
	representation contained in the next 8 bytes on the receiver."

	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			at: 8 put: (self next);
			at: 7 put: (self next);
			at: 6 put: (self next);
			at: 5 put: (self next);
			at: 4 put: (self next);
			at: 3 put: (self next);
			at: 2 put: (self next);
			at: 1 put: (self next);
			yourself.
	^ bytes sqwordAtOffset: 0!

nextBeSWORD
	"Answer a 16-bit signed integer constructed from the big-endian
	representation contained in the next 2 bytes on the receiver."

	| int |
#CUadded.

	int := self nextBeWORD.
	^ int >= 16r8000
		ifTrue: [int - 16r10000]
		ifFalse: [int].!

nextBeWORD
	"Answer a 16-bit unsigned integer constructed from the big-endian
	representation contained in the next 2 bytes on the receiver."

#CUadded.
	^ (self next bitShift: 8) bitOr: self next.
!

nextFLOAT
	"Answer a single precision floating pointer number constructed from the host system
	representation contained in the next 4 bytes on the receiver."
#CUadded.
	^(self next: 4) floatAtOffset: 0!

nextQWORD
	"Answer a 64-bit unsigned integer constructed from the host system
	representation contained in the next 8 bytes on the receiver."
#CUadded.
	^(self next: 8) qwordAtOffset: 0!

nextSQWORD
	"Answer a 64-bit signed integer constructed from the host system
	representation contained in the next 8 bytes on the receiver."
#CUadded.
	^(self next: 8) sqwordAtOffset: 0! !
!PositionableStream categoriesFor: #nextBeDOUBLE!binary filing!public! !
!PositionableStream categoriesFor: #nextBeDWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextBeFLOAT!binary filing!public! !
!PositionableStream categoriesFor: #nextBeQWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextBeSDWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextBeSQWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextBeSWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextBeWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextFLOAT!binary filing!public! !
!PositionableStream categoriesFor: #nextQWORD!binary filing!public! !
!PositionableStream categoriesFor: #nextSQWORD!binary filing!public! !

!WriteStream methodsFor!

nextBeDOUBLEPut: aFloat
	"Append a 64-bit IEEE floating point value in big-endian format (aka
	'network byte order')  as the next 8 bytes on the receiver."

	| bytes |
#CUadded.
	bytes := (ByteArray new: 8)
			doubleAtOffset: 0 put: aFloat;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^aFloat.!

nextBeDWORDPut: anInteger
	"Append a 32-bit unsigned integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 4 bytes on the receiver."
	| bytes |

#CUadded.
	"rather surprisingly, this implementation is faster then eirther bit-twiddling to
	avoid creating the temporary array, or unrolling the loop"
	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#dwordAtOffset:put: doesn't check for negative numbers"
	bytes := (ByteArray new: 4)
			dwordAtOffset: 0 put: anInteger;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeFLOATPut: aFloat
	"Append a 32-bit IEEE floating point value in big-endian format (aka
	'network byte order')  as the next 4 bytes on the receiver."

	| bytes |
#CUadded.
	bytes := (ByteArray new: 4)
			floatAtOffset: 0 put: aFloat;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^aFloat.!

nextBeQWORDPut: anInteger
	"Append a 64-bit unsigned integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 8 bytes on the receiver."
	| bytes |

#CUadded.
	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#qwordAtOffset:put: doesn't check for negative numbers"
	bytes := (ByteArray new: 8)
			qwordAtOffset: 0 put: anInteger;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeSDWORDPut: anInteger
	"Append a 32-bit signed integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 4 bytes on the receiver."
	| bytes |

#CUadded.
	"rather surprisingly, this implementation is faster then eirther bit-twiddling to
	avoid creating the temporary array, or unrolling the loop"
	bytes := (ByteArray new: 4)
			sdwordAtOffset: 0 put: anInteger;
			yourself.
	4 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^anInteger.
!

nextBeSQWORDPut: anInteger
	"Append a 64-bit signed integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 8 bytes on the receiver."
	| bytes |

#CUadded.
	bytes := (ByteArray new: 8)
			sqwordAtOffset: 0 put: anInteger;
			yourself.
	8 to: 1 by: -1 do: [:i | self nextPut: (bytes at: i)].

	^ anInteger.
!

nextBeSWORDPut: anInteger
	"Append a 16-bit signed integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 2 bytes on the receiver."

#CUadded.
	(anInteger >= -16r8000 and: [anInteger < 16r8000]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: ((anInteger bitShift: -8) bitAnd: 16rFF);	
		nextPut: (anInteger bitAnd: 16rFF).

	^ anInteger.
!

nextBeWORDPut: anInteger
	"Append a 16-bit unsigned integer in 2's complement, big-endian (aka 'network byte order')
	representation as the next 2 bytes on the receiver."

#CUadded.
	(anInteger >= 0 and: [anInteger <= 16rFFFF]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitShift: -8);
		nextPut: (anInteger bitAnd: 16rFF).

	^ anInteger.
!

nextDOUBLEPut: aFloat
	"Append a 64-bit IEEE floating point value 
	as the next 8 bytes on the receiver."

#CUadded.
	self nextPutAll: 
		((ByteArray new: 8)
			doubleAtOffset: 0 put: aFloat;
			yourself).

	^aFloat.!

nextDWORDPut: anInteger
	"Append a 32-bit unsigned integer in 2's complement representation 
	as the next 4 bytes on the receiver."

#CUadded.
	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#dwordAtOffset:put: doesn't check for negative numbers"
	self nextPutAll: 
		((ByteArray new: 4)
			dwordAtOffset: 0 put: anInteger;
			yourself).

	^anInteger.!

nextFLOATPut: aFloat
	"Append a 32-bit IEEE floating point value 
	as the next 4 bytes on the receiver."

#CUadded.
	self nextPutAll: 
		((ByteArray new: 4)
			floatAtOffset: 0 put: aFloat;
			yourself).

	^aFloat.!

nextQWORDPut: anInteger
	"Append a 64-bit unsigned integer in 2's complement representation 
	as the next 8 bytes on the receiver."

#CUadded.
	(anInteger < 0) ifTrue: [^ self errorCantHold: anInteger].	"#qwordAtOffset:put: doesn't check for negative numbers"
	self nextPutAll: 
		((ByteArray new: 8)
			qwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextSQWORDPut: anInteger
	"Append a 64-bit signed integer in 2's complement representation 
	as the next 8 bytes on the receiver."

#CUadded.
	self nextPutAll: 
		((ByteArray new: 8)
			sqwordAtOffset: 0 put: anInteger;
			yourself).

	^ anInteger.!

nextSWORDPut: anInteger
	"Append a 16-bit signed integer in 2's complement representation 
	as the next 2 bytes on the receiver."

#CUadded.
	(anInteger >= -16r8000 and: [anInteger < 16r8000]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitAnd: 16rFF);
		nextPut: ((anInteger bitShift: -8) bitAnd: 16rFF).

	^ anInteger.
!

nextWORDPut: anInteger
	"Append a 16-bit unsigned integer in 2's complement representation 
	as the next 2 bytes on the receiver."

#CUadded.
	(anInteger >= 0 and: [anInteger <= 16rFFFF]) ifFalse: [^ self errorCantHold: anInteger].
	self
		nextPut: (anInteger bitAnd: 16rFF);
		nextPut: (anInteger bitShift: -8).

	^ anInteger.

! !
!WriteStream categoriesFor: #nextBeDOUBLEPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeDWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeFLOATPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeQWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeSDWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeSQWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeSWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextBeWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextDOUBLEPut:!binary filing!public! !
!WriteStream categoriesFor: #nextDWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextFLOATPut:!binary filing!public! !
!WriteStream categoriesFor: #nextQWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextSQWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextSWORDPut:!binary filing!public! !
!WriteStream categoriesFor: #nextWORDPut:!binary filing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

