| package |
package := Package name: 'CU Stream Extensions Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Some tests for the Streams extesion methods.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '1.02'.


package classNames
	add: #StreamExtensionsTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #StreamExtensionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StreamExtensionsTest guid: (GUID fromString: '{028C3B42-6E9D-47E6-A8A9-8EAC69DB9F82}')!
StreamExtensionsTest comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org
'!
!StreamExtensionsTest categoriesForClass!Unclassified! !
!StreamExtensionsTest methodsFor!

cantHoldException

	"sigh..."
	^ Error.!

testCantHold: aNumber or: anotherNumber selector: aWriteSelector

	| stream bytes |

	stream := ByteArray writeStream.
	self
		should:	[stream perform: aWriteSelector with: aNumber]			raise: self cantHoldException;
		shouldnt:	[stream perform: aWriteSelector with: aNumber+1]			raise: self cantHoldException;
		should:	[stream perform: aWriteSelector with: anotherNumber]	raise: self cantHoldException;
		shouldnt:	[stream perform: aWriteSelector with: anotherNumber-1]	raise: self cantHoldException;
		yourself.

!

testCantHoldBigEndianDWORDs

	self
		testCantHold: -1
		or: 16r100000000
		selector: #nextBeDWORDPut:.!

testCantHoldBigEndianQWORDs

	self
		testCantHold: -1
		or: 16r10000000000000000
		selector: #nextBeQWORDPut:.!

testCantHoldBigEndianSDWORDs

	self
		testCantHold: -16r80000001
		or: 16r80000000
		selector: #nextBeSDWORDPut:.!

testCantHoldBigEndianSQWORDs

	self
		testCantHold: -16r8000000000000001
		or: 16r8000000000000000
		selector: #nextBeSQWORDPut:.!

testCantHoldBigEndianSWORDs

	self
		testCantHold: -16r8001
		or: 16r8000
		selector: #nextBeSWORDPut:.!

testCantHoldBigEndianWORDs

	self
		testCantHold: -1
		or: 16r10000
		selector: #nextBeWORDPut:.!

testCantHoldDWORDs

	self
		testCantHold: -1
		or: 16r100000000
		selector: #nextDWORDPut:.!

testCantHoldQWORDs

	self
		testCantHold: -1
		or: 16r10000000000000000
		selector: #nextQWORDPut:.!

testCantHoldSDWORDs

	self
		testCantHold: -16r80000001
		or: 16r80000000
		selector: #nextSDWORDPut:.!

testCantHoldSQWORDs

	self
		testCantHold: -16r8000000000000001
		or: 16r8000000000000000
		selector: #nextSQWORDPut:.!

testCantHoldSWORDs

	self
		testCantHold: -16r8001
		or: 16r8000
		selector: #nextSWORDPut:.!

testCantHoldWORDs

	self
		testCantHold: -1
		or: 16r10000
		selector: #nextWORDPut:.!

testManyBigEndianDOUBLEs

	self
		testOver: (-100000000 to: 100000000 by: 1234.56)
		writeSelector: #nextBeDOUBLEPut:
		readSelector: #nextBeDOUBLE
		bytesPerValue: 8.!

testManyBigEndianDWORDs

	self
		testOver: (0 to: 16rFFFFFFFF by: 10001)
		writeSelector: #nextBeDWORDPut:
		readSelector: #nextBeDWORD
		bytesPerValue: 4.!

testManyBigEndianFLOATs

	self
		testOver: (-1000000 to: 1000000 by: 12.5)		"restricted range to avoid ruounding errors"
		writeSelector: #nextBeFLOATPut:
		readSelector: #nextBeFLOAT
		bytesPerValue: 4.!

testManyBigEndianQWORDs

	self
		testOver: (0 to: 16rFFFFFFFFFFFFFFFF by: 100000000000001)
		writeSelector: #nextBeQWORDPut:
		readSelector: #nextBeQWORD
		bytesPerValue: 8.!

testManyBigEndianSDWORDs

	self
		testOver: (-16r80000000 to: 16r7FFFFFFF by: 10001)
		writeSelector: #nextBeSDWORDPut:
		readSelector: #nextBeSDWORD
		bytesPerValue: 4.!

testManyBigEndianSQWORDs

	self
		testOver: (-16r8000000000000000 to: 16r7FFFFFFFFFFFFFFF  by: 100000000000001)
		writeSelector: #nextBeSQWORDPut:
		readSelector: #nextBeSQWORD
		bytesPerValue: 8.!

testManyBigEndianSWORDs

	self
		testOver: (-16r8000 to: 16r7FFF)
		writeSelector: #nextBeSWORDPut:
		readSelector: #nextBeSWORD
		bytesPerValue: 2.!

testManyDOUBLEs

	self
		testOver: (-100000000 to: 100000000 by: 1234.56)
		writeSelector: #nextDOUBLEPut:
		readSelector: #nextDOUBLE
		bytesPerValue: 8.!

testManyDWORDs

	self
		testOver: (0 to: 16rFFFFFFFF by: 10001)
		writeSelector: #nextDWORDPut:
		readSelector: #nextDWORD
		bytesPerValue: 4.!

testManyFLOATs

	self
		testOver: (-1000000 to: 1000000 by: 12.5)		"restricted range to avoid ruounding errors"
		writeSelector: #nextFLOATPut:
		readSelector: #nextFLOAT
		bytesPerValue: 4.!

testManyQWORDs

	self
		testOver: (0 to: 16rFFFFFFFFFFFFFFFF by: 100000000000001)
		writeSelector: #nextQWORDPut:
		readSelector: #nextQWORD
		bytesPerValue: 8.!

testManySDWORDs

	self
		testOver: (-16r80000000 to: 16r7FFFFFFF by: 10001)
		writeSelector: #nextSDWORDPut:
		readSelector: #nextSDWORD
		bytesPerValue: 4.!

testManySQWORDs

	self
		testOver: (-16r8000000000000000 to: 16r7FFFFFFFFFFFFFFF  by: 100000000000001)
		writeSelector: #nextSQWORDPut:
		readSelector: #nextSQWORD
		bytesPerValue: 8.!

testManySWORDs

	self
		testOver: (-16r8000 to: 16r7FFF)
		writeSelector: #nextSWORDPut:
		readSelector: #nextSWORD
		bytesPerValue: 2.!

testManyWORDs

	self
		testOver: (0 to: 16rFFFF)
		writeSelector: #nextWORDPut:
		readSelector: #nextWORD
		bytesPerValue: 2.!

testOver: anInterval writeSelector: aWriteSelector readSelector: aReadSelector bytesPerValue: anInteger

	| stream bytes |

	stream := ByteArray writeStream.
	anInterval do: [:each | stream perform: aWriteSelector with: each].

	bytes := stream contents.
	self should: [bytes size = (anInterval size * anInteger)].

	stream := bytes readStream.
	anInterval do: [:each || next | next := stream perform: aReadSelector. self should: [next = each]].
	self should: [stream atEnd].
! !
!StreamExtensionsTest categoriesFor: #cantHoldException!constants!public! !
!StreamExtensionsTest categoriesFor: #testCantHold:or:selector:!helpers!private! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianSDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianSQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianSWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldBigEndianWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldSDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldSQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldSWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testCantHoldWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianDOUBLEs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianFLOATs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianSDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianSQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyBigEndianSWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyDOUBLEs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyFLOATs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManySDWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManySQWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManySWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testManyWORDs!public!unit tests! !
!StreamExtensionsTest categoriesFor: #testOver:writeSelector:readSelector:bytesPerValue:!helpers!private! !

!StreamExtensionsTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].! !
!StreamExtensionsTest class categoriesFor: #allTestSelectors!accessing!public! !

"Binary Globals"!

"Resources"!

