| package |
package := Package name: 'CU BZip2 Streams Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A few basic tests for the Stream-like wrappers for bzip2 compression.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '1.01'.


package classNames
	add: #BZipReadStreamTest;
	add: #BZipStreamTest;
	add: #BZipWriteStreamsTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU BZip2 Base';
	add: 'CU BZip2 Streams';
	add: 'CU ZLib Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #BZipStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'BZip2Constants ZLib1Constants'
	classInstanceVariableNames: ''!
BZipStreamTest subclass: #BZipReadStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BZipStreamTest subclass: #BZipWriteStreamsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BZipStreamTest guid: (GUID fromString: '{2CC16D3B-BF6C-452A-9722-CA7CBF5D87B2}')!
BZipStreamTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

NB: these tests only attempt to confirm that data compressed by this library can be decompressed by it.

I have done a little testing against external data, and it seems to be OK, and *with luck* any regression that affects both compression and decompression will be caught by the (few) predefined test cases.

The tests take a minute or two to run, that''s because there are so many oppurtunities for off-by-one errors and similar in all the buffering code, so I''ve attempted to be quite exhaustive in testing different combinations of data length and buffer (etc) size.'!
!BZipStreamTest categoriesForClass!Unclassified! !
!BZipStreamTest methodsFor!

binaryData

	^ self class binaryData.!

check: aDictionary against: anotherDictionary

	| in out |

	in := aDictionary at: #uncompressed.
	out := anotherDictionary at: #uncompressed.
	self should: [in = out].

	self checkableData do:
		[:each |
		in := aDictionary at: each.
		out := anotherDictionary at: each.
		self should: [in = out]].!

checkableData

	^ #(
		#compressedSize
		#uncompressedSize
	).!

compressBinary: aByteArray

	| stream |

	stream := BZip2CompressingWriteStream forBinary.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aByteArray.!

compressBinary: aByteArray blockSize: anInteger

	| stream |

	stream := BZip2CompressingWriteStream forBinaryWithBlockSize: anInteger.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aByteArray.
!

compressText: aString

	| stream |

	stream := BZip2CompressingWriteStream forText.

	stream nextPutAll: aString.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aString.!

compressText: aString blockSize: anInteger

	| stream |

	stream := BZip2CompressingWriteStream forTextWithBlockSize: anInteger.

	stream nextPutAll: aString.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aString.
!

predefinedTestCases

	^ self class predefinedTestCases.!

recordDataFrom: aZLibStream
	| data |

	"must close before getting chechsums, etc"
	aZLibStream close.

	data := LookupTable new.
	self checkableData do: [:each | data at: each put: (aZLibStream perform: each)].

	^ data.!

recordDataFrom: aZLibStream compressed: aByteArray uncompressed: aByteArrayOrString

	^ (self recordDataFrom: aZLibStream uncompressed: aByteArrayOrString)
		at: #compressed put: aByteArray;
		yourself.
!

recordDataFrom: aZLibStream uncompressed: aByteArrayOrString

	^ (self recordDataFrom: aZLibStream)
		at: #uncompressed put: aByteArrayOrString;
		yourself.!

testBinary: aByteArray

	| c u |

	c := self compressBinary: aByteArray.
	u := self uncompressBinary: (c at: #compressed).

	self check: c against: u.
!

testBinary: aByteArray blockSize: anInteger

	| c u |

	c := self compressBinary: aByteArray blockSize: anInteger.
	u := self uncompressBinary: (c at: #compressed).

	self check: c against: u.!

testBinary1

	self binaryData do: [:each | self testBinary: each blockSize: 1].
!

testBinary2

	self binaryData do: [:each | self testBinary: each blockSize: 2].
!

testBinary3

	self binaryData do: [:each | self testBinary: each blockSize: 3].
!

testBinary4

	self binaryData do: [:each | self testBinary: each blockSize: 4].
!

testBinary5

	self binaryData do: [:each | self testBinary: each blockSize: 5].
!

testBinary6

	self binaryData do: [:each | self testBinary: each blockSize: 6].
!

testBinary7

	self binaryData do: [:each | self testBinary: each blockSize: 7].
!

testBinary8

	self binaryData do: [:each | self testBinary: each blockSize: 8].
!

testBinary9

	self binaryData do: [:each | self testBinary: each blockSize: 9].
!

testBinaryDefault

	self binaryData do: [:each | self testBinary: each].
!

testBufferSizes: a4Array

	self binaryData do: [:each | self testBufferSizes: a4Array on: each].!

testOddBufferSizes

	| sizes |

	"not too many cases since each test is slow"
	sizes := #(		"compress in	compress out		decompress in		decompress out"
			(	23		17			11						37	)
			(	2		4			8						16	)
		).	

	sizes do: [:each | self testBufferSizes: each].
!

testText: aString

	| c u |

	c := self compressText: aString.
	u := self uncompressText: (c at: #compressed).

	self check: c against: u.
!

testText: aString blockSize: anInteger

	| c u |

	c := self compressText: aString blockSize: anInteger.
	u := self uncompressText: (c at: #compressed).

	self check: c against: u.
!

testText1

	self textData do: [:each | self testText: each blockSize: 1].
!

testText2

	self textData do: [:each | self testText: each blockSize: 2].
!

testText3

	self textData do: [:each | self testText: each blockSize: 3].
!

testText4

	self textData do: [:each | self testText: each blockSize: 4].
!

testText5

	self textData do: [:each | self testText: each blockSize: 5].
!

testText6

	self textData do: [:each | self testText: each blockSize: 6].
!

testText7

	self textData do: [:each | self testText: each blockSize: 7].
!

testText8

	self textData do: [:each | self testText: each blockSize: 9].
!

testText9

	self textData do: [:each | self testText: each blockSize: 9].
!

testTextDefault

	self textData do: [:each | self testText: each].
!

testZeros

	self zeros do: [:each | self testBinary: each].
!

textData

	^ self class textData.
!

textDatum

	^ self class textDatum.
!

uncompressBinary: aDictionary

	self subclassResponsibility.!

uncompressText: aByteArray

	self subclassResponsibility.!

zeros

	^ self class zeros.! !
!BZipStreamTest categoriesFor: #binaryData!constants!public! !
!BZipStreamTest categoriesFor: #check:against:!helpers!private! !
!BZipStreamTest categoriesFor: #checkableData!constants!public! !
!BZipStreamTest categoriesFor: #compressBinary:!helpers!private! !
!BZipStreamTest categoriesFor: #compressBinary:blockSize:!helpers!private! !
!BZipStreamTest categoriesFor: #compressText:!helpers!private! !
!BZipStreamTest categoriesFor: #compressText:blockSize:!helpers!private! !
!BZipStreamTest categoriesFor: #predefinedTestCases!constants!public! !
!BZipStreamTest categoriesFor: #recordDataFrom:!helpers!private! !
!BZipStreamTest categoriesFor: #recordDataFrom:compressed:uncompressed:!helpers!private! !
!BZipStreamTest categoriesFor: #recordDataFrom:uncompressed:!helpers!private! !
!BZipStreamTest categoriesFor: #testBinary:!public!test helpers! !
!BZipStreamTest categoriesFor: #testBinary:blockSize:!public!test helpers! !
!BZipStreamTest categoriesFor: #testBinary1!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary2!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary3!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary4!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary5!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary6!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary7!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary8!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinary9!public!unit tests! !
!BZipStreamTest categoriesFor: #testBinaryDefault!public!unit tests! !
!BZipStreamTest categoriesFor: #testBufferSizes:!public!test helpers! !
!BZipStreamTest categoriesFor: #testOddBufferSizes!public!unit tests! !
!BZipStreamTest categoriesFor: #testText:!public!test helpers! !
!BZipStreamTest categoriesFor: #testText:blockSize:!public!test helpers! !
!BZipStreamTest categoriesFor: #testText1!public!unit tests! !
!BZipStreamTest categoriesFor: #testText2!public!unit tests! !
!BZipStreamTest categoriesFor: #testText3!public!unit tests! !
!BZipStreamTest categoriesFor: #testText4!public!unit tests! !
!BZipStreamTest categoriesFor: #testText5!public!unit tests! !
!BZipStreamTest categoriesFor: #testText6!public!unit tests! !
!BZipStreamTest categoriesFor: #testText7!public!unit tests! !
!BZipStreamTest categoriesFor: #testText8!public!unit tests! !
!BZipStreamTest categoriesFor: #testText9!public!unit tests! !
!BZipStreamTest categoriesFor: #testTextDefault!public!unit tests! !
!BZipStreamTest categoriesFor: #testZeros!public!unit tests! !
!BZipStreamTest categoriesFor: #textData!constants!public! !
!BZipStreamTest categoriesFor: #textDatum!constants!public! !
!BZipStreamTest categoriesFor: #uncompressBinary:!helpers!private! !
!BZipStreamTest categoriesFor: #uncompressText:!helpers!private! !
!BZipStreamTest categoriesFor: #zeros!constants!public! !

!BZipStreamTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].!

binaryData

	| data stream |

	stream := ByteArray writeStream.
	data := Array writeStream.
	data nextPut: #[].		"NB: not #nextPutAll:"
	0 to: 1333 do:
		[:i |
		stream nextPut: (i & 16rFF).
		data nextPut: stream contents].

	^ data contents.!

isAbstract

	^self = ##(self).!

predefinedTestCases

	"these were all compressed by the bzip2.exe utility with default compression options"
	^ #(
		(
			''
			#[66 90 104 57 23 114 69 56 80 144 0 0 0 0]
		)
		(
			'x'
			#[66 90 104 57 49 65 89 38 83 89 119 75 176 20 0 0 0 0 128 0 64 32 0 33 24 70 130 238 72 167 10 18 14 233 118 2 128]
		)
		(
			'aaa'
			#[66 90 104 57 49 65 89 38 83 89 31 229 223 206 0 0 1 1 0 32 0 32 0 48 140 20 24 187 146 41 194 132 128 255 46 254 112] 
		)
		(	'xysxysxysxysxysxysxysxys'
			 #[66 90 104 57 49 65 89 38 83 89 94 19 22 156 0 0 7 128 128 8 96 32 0 48 204 5 65 133 24 113 119 36 83 133 9 5 225 49 105 192]
		)
		(
			'abcdefghijklmnopqrstuvwxyz'
			#[66 90 104 57 49 65 89 38 83 89 119 191 147 150 0 0 0 1 128 63 255 255 240 32 0 34 154 52 1 160 1 161 64 0 1 147 32 193 75 22 53 207 132 101 50 165 109 124 103 91 231 124 127 23 114 69 56 80 144 119 191 147 150] 
		)
		(
			'abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz'
			#[66 90 104 57 49 65 89 38 83 89 146 190 244 75 0 0 2 1 128 63 255 255 240 32 0 80 160 0 0 201 144 34 168 26 13 50 1 163 74 165 137 106 92 151 166 9 4 197 50 76 211 68 138 106 146 73 166 201 186 112 156 167 73 218 120 148 79 83 228 170 126 46 228 138 112 161 33 37 125 232 150]
		)
	).!

textData

	| words stream data w |

	words := #( The cat sat on the mat ).
	stream := String writeStream.
	data := Array writeStream.
	w := 0.
	1333 timesRepeat:
		[w := w + 1.
		w <= words size
			ifTrue: [stream space]
			ifFalse: [w := 1. stream cr].
		stream nextPutAll: (words at: w).
		data nextPut: stream contents].

	^ data contents.!

textDatum

	"small bit of 'poetry' from the *wonderfully* entertaining
	book
		The Butterfly Kid
		Chester Anserson
	"

	^ 'The high wheel turned a stormwind and the wise
were powerfull against it, and they died.
Who could predict the new life tearing
on the sundering claws of the wind ?'.
!

zeros

	| data stream |

	stream := ByteArray writeStream.
	data := Array writeStream.
	713 timesRepeat:
		[stream nextPut: 0.		"NB: not #nextPutAll:"
		data nextPut: stream contents].

	^ data contents.! !
!BZipStreamTest class categoriesFor: #allTestSelectors!accessing!public! !
!BZipStreamTest class categoriesFor: #binaryData!constants!public! !
!BZipStreamTest class categoriesFor: #isAbstract!public!testing! !
!BZipStreamTest class categoriesFor: #predefinedTestCases!constants!public! !
!BZipStreamTest class categoriesFor: #textData!constants!public! !
!BZipStreamTest class categoriesFor: #textDatum!constants!public! !
!BZipStreamTest class categoriesFor: #zeros!constants!public! !

BZipReadStreamTest guid: (GUID fromString: '{A112FE8C-19F3-46E5-A4E3-3ED5E05A7B3D}')!
BZipReadStreamTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!BZipReadStreamTest categoriesForClass!Unclassified! !
!BZipReadStreamTest methodsFor!

testBufferSizes: a4Array on: aByteArray

	| compressed decompressor decompressed |

	compressed := BZip2CompressingWriteStream compressBinary: aByteArray.

	decompressor := (BZip2DecompressingReadStream forBinaryOn: compressed readStream)
				inBufferSize: (a4Array at: 3);
				outBufferSize: (a4Array at: 4);
				yourself.

	decompressed := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = decompressed size].
	self should: [decompressor compressedSize = compressed size].

	self should: [aByteArray = decompressed].!

testFragmentedCompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := BZip2CompressingWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

	out := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testFragmentedDecompress

	| in compressor compressed decompressor outStream out |

	in := self textDatum.

	compressor := BZip2CompressingWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

	outStream := String writeStream.
	decompressor do: [:each | outStream nextPut: each].
	decompressor close.
	out := outStream contents.

	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testNextLine

	| lines compressed decompressor |

	lines := self textDatum subStrings: String lineDelimiter.
	compressed := BZip2CompressingWriteStream compressText: self textDatum.
	decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

	lines do:
		[:each || next |
		self shouldnt: [decompressor atEnd].
		next := decompressor nextLine.
		self should: [each = next]].

		self should: [decompressor atEnd].

	decompressor close.!

testNextWord

	| words compressed decompressor |

	words := self textDatum subStrings.
	compressed := BZip2CompressingWriteStream compressText: self textDatum.
	decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

	words do:
		[:each || next |
		self shouldnt: [decompressor atEnd].
		next := decompressor nextWord.
		self should: [each = next]].

		self should: [decompressor atEnd].

	decompressor close.!

testNoSkipBack

	| compressed decompressor |

	compressed := BZip2CompressingWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := BZip2DecompressingReadStream forBinaryOn: compressed readStream.

	decompressor skipBackLimit: 0.

	self should: [decompressor skip: -1] raise: BoundsError.
	self should: [decompressor next = 1].
	self should: [decompressor skip: -1] raise: BoundsError.
	self should: [decompressor next = 2].
	self should: [decompressor skip: -1] raise: BoundsError.
	self should: [decompressor next = 3].
	self shouldnt: [decompressor skip: 0] raise: BoundsError.
	self should: [decompressor next = 4].
	self shouldnt: [decompressor skip: 10] raise: BoundsError.
	self should: [decompressor next = 15].

	decompressor close.

!

testOddSkipBackLimit

	#(3 4 7 16 31) do:
		[:i |
		self testSkipBackLimit: i.
		self binaryData do: [:each | self testSkipBackLimit: i on: each]].!

testPosition

	| compressor compressed decompressor lastOk firstOk |

	compressor := BZip2CompressingWriteStream forBinary.
	1 to: 1033 do: [:i | compressor nextPut: (i & 16rFF)].
	compressed := compressor contents.

	decompressor := BZip2DecompressingReadStream forBinaryOn: compressed readStream.

	self should: [decompressor position = 0].
	decompressor open.
	self should: [decompressor position = 0].

	1 to: 1033 do:
		[:i |
		decompressor next.
		self should: [decompressor position = i]].

	self should: [decompressor position = 1033].
	self should: [decompressor atEnd].

	decompressor close.

!

testPredefinedTestCases

	self predefinedTestCases do:
		[:case || in compressed decompressor |
		in := case first.
		compressed := case second.

		decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

		in do:
			[:each || next |
			next := decompressor next.
			self should: [next = each]].
		self should: [decompressor atEnd].

		decompressor close].
!

testPushBackUnconsumedInput

	| magic |

	magic := 'A *majic* number' asByteArray.

	self binaryData do:
		[:in || compressed data stream decompressor out unconsumed |
		compressed := BZip2CompressingWriteStream compressBinary: in.
		data := compressed , magic.
		stream := data readStream.
		decompressor := BZip2DecompressingReadStream forBinaryOn: stream.
		out := decompressor upToEnd.
		self should: [out = in].
		decompressor
			close;
			pushBackUnconsumedInput.
		unconsumed := stream upToEnd.
		self should: [unconsumed = magic]].
!

testSetToEnd

	| compressed decompressor nextShouldBe check next |

	compressed := BZip2CompressingWriteStream compressText: self textDatum.
	decompressor := BZip2DecompressingReadStream forTextOn: compressed readStream.

	decompressor setToEnd.

	self should: [decompressor atEnd].
	self should: [decompressor position = self textDatum size].
	self should: [decompressor next] raise: Stream endOfStreamSignal.

	decompressor close.
!

testSkip

	| compressed decompressor nextShouldBe check next |

	compressed := BZip2CompressingWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := BZip2DecompressingReadStream forBinaryOn: compressed readStream.

	"ensure that some seeks are outside the internal buffer"
	decompressor outBufferSize: 3.

	self should: [decompressor skip: -1] raise: BoundsError.

	nextShouldBe := 1.
	#( 0 1 2 7 12 -1 0 3 5) do: [:skip |
		decompressor skip: skip.
		nextShouldBe := nextShouldBe + skip.
		next := decompressor next.
		self should: [next = nextShouldBe].
		nextShouldBe := nextShouldBe + 1].

	self should: [decompressor skip: -10] raise: BoundsError.
	self should: [decompressor skip: 1000] raise: Stream endOfStreamSignal.

	decompressor close.
!

testSkipBackLimit: anInteger

	| compressed decompressor lastOk firstOk |

	compressed := BZip2CompressingWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := BZip2DecompressingReadStream forBinaryOn: compressed readStream.

	decompressor skipBackLimit: anInteger.

	decompressor setToEnd.
	firstOk := decompressor position - anInteger.
	lastOk := decompressor position - 1.

	1 to: firstOk-1 do:
		[:i |
		self should: [decompressor position: i] raise: BoundsError.
		self should: [decompressor position = 255]].

	firstOk to: lastOk do:
		[:i || next |
		self shouldnt: [decompressor position: i] raise: BoundsError.
		self should: [decompressor position = i].
		next := decompressor next.
		self should: [next = (i+1)]].

	self should: [decompressor atEnd].

	decompressor close.
!

testSkipBackLimit: anInteger on: aByteArray

	| compressed decompressor decompressed |

	compressed := BZip2CompressingWriteStream compressBinary: aByteArray.

	decompressor := (BZip2DecompressingReadStream forBinaryOn: compressed readStream)
				skipBackLimit: anInteger;
				yourself.

	decompressed := decompressor upToEnd.
	decompressor close.

	self should: [aByteArray = decompressed].
!

testUnbufferedCompress

	| sizes |

	sizes := Array
			with: 1		"compress in buffer"
			with: 1		"compress out buffer"
			with: BZip2DecompressingReadStream defaultInBufferSize		"decompress in buffer"
			with: BZip2DecompressingReadStream defaultOutBufferSize.		"decompress out buffer"
			

	self testBufferSizes: sizes.!

testUnbufferedDecompress

	| sizes |

	sizes := Array
			with: BZip2CompressingWriteStream defaultInBufferSize		"compress in buffer"
			with: BZip2CompressingWriteStream defaultOutBufferSize	"compress out buffer"
			with: 1		"decompress in buffer"
			with: 1.	"decompress out buffer"
			

	self testBufferSizes: sizes.
!

testUnconsumedInput

	| magic |

	magic := 'A *majic* number' asByteArray.

	self binaryData do:
		[:in || compressed data stream decompressor out unconsumed |
		compressed := BZip2CompressingWriteStream compressBinary: in.
		data := compressed , magic.

		stream := data readStream.
		decompressor := BZip2DecompressingReadStream forBinaryOn: stream.

		out := decompressor upToEnd.
		decompressor close.
		self should: [out = in].

		unconsumed := decompressor unconsumedInput.
		unconsumed := unconsumed , stream upToEnd.
		self should: [unconsumed = magic]].!

uncompressBinary: aByteArray

	| stream |

	stream := BZip2DecompressingReadStream forBinaryOn: aByteArray readStream.

	^ self
		recordDataFrom: stream
		uncompressed: stream upToEnd!

uncompressText: aByteArray

	| stream |

	stream := BZip2DecompressingReadStream forTextOn: aByteArray readStream.

	^ self
		recordDataFrom: stream
		uncompressed: stream upToEnd
! !
!BZipReadStreamTest categoriesFor: #testBufferSizes:on:!public!test helpers! !
!BZipReadStreamTest categoriesFor: #testFragmentedCompress!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testFragmentedDecompress!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testNextLine!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testNextWord!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testNoSkipBack!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testOddSkipBackLimit!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testPosition!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testPredefinedTestCases!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testPushBackUnconsumedInput!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testSetToEnd!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testSkip!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testSkipBackLimit:!public!test helpers! !
!BZipReadStreamTest categoriesFor: #testSkipBackLimit:on:!public!test helpers! !
!BZipReadStreamTest categoriesFor: #testUnbufferedCompress!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testUnbufferedDecompress!public!unit tests! !
!BZipReadStreamTest categoriesFor: #testUnconsumedInput!public!unit tests! !
!BZipReadStreamTest categoriesFor: #uncompressBinary:!helpers!private! !
!BZipReadStreamTest categoriesFor: #uncompressText:!helpers!private! !

BZipWriteStreamsTest guid: (GUID fromString: '{48D2AC28-2A50-4EA3-9632-67AE4CF48AC5}')!
BZipWriteStreamsTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!BZipWriteStreamsTest categoriesForClass!Unclassified! !
!BZipWriteStreamsTest methodsFor!

testBufferSizes: a4Array on: aByteArray

	| compressor compressed decompressor decompressed |

	compressor := (BZip2CompressingWriteStream forBinary)
				inBufferSize: (a4Array at: 1);
				outBufferSize: (a4Array at: 2);
				yourself.

	compressed := compressor
				nextPutAll: aByteArray;
				contents.
	self should: [compressor uncompressedSize = aByteArray size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (BZip2DecompressingWriteStream forBinary)
				inBufferSize: (a4Array at: 3);
				outBufferSize: (a4Array at: 4);
				yourself.

	decompressed := decompressor
				nextPutAll: compressed;
				contents.
	self should: [decompressor uncompressedSize = decompressed size].
	self should: [decompressor compressedSize = compressed size].

	self should: [aByteArray = decompressed].!

testFragmentedCompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := BZip2CompressingWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := BZip2DecompressingWriteStream forText.

	out := decompressor
			nextPutAll: compressed;
			contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testFragmentedDecompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := BZip2CompressingWriteStream forText.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := BZip2DecompressingWriteStream forText.
	compressed do: [:each | decompressor nextPut: each].

	out := decompressor contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testPosition

	| compressor |

	compressor := BZip2CompressingWriteStream forBinary.

	self should: [compressor position = 0].
	compressor open.
	self should: [compressor position = 0].

	1 to: 1003 do:
		[:i |
		compressor nextPut: (i & 16rFF).
		self should: [compressor position = i]].

	compressor close.
!

testPredefinedTestCases

	self predefinedTestCases do:
		[:case || in compressed out |
		in := case first.
		compressed := case second.

		out := BZip2DecompressingWriteStream decompressText: compressed.
		self should: [in = out]].!

testUnbufferedCompress

	| sizes |

	sizes := Array
			with: 1		"compress in buffer"
			with: 1		"compress out buffer"
			with: BZip2DecompressingWriteStream defaultInBufferSize		"decompress in buffer"
			with: BZip2DecompressingWriteStream defaultOutBufferSize.		"decompress out buffer"
			

	self testBufferSizes: sizes.!

testUnbufferedDecompress

	| sizes |

	sizes := Array
			with: BZip2CompressingWriteStream defaultInBufferSize		"compress in buffer"
			with: BZip2CompressingWriteStream defaultOutBufferSize	"compress out buffer"
			with: 1		"decompress in buffer"
			with: 1.	"decompress out buffer"
			

	self testBufferSizes: sizes.
!

uncompressBinary: aByteArray

	| stream |

	stream := BZip2DecompressingWriteStream forBinary.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		uncompressed: stream contents.
!

uncompressText: aByteArray

	| stream |

	stream := BZip2DecompressingWriteStream forText.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		uncompressed: stream contents.! !
!BZipWriteStreamsTest categoriesFor: #testBufferSizes:on:!public!test helpers! !
!BZipWriteStreamsTest categoriesFor: #testFragmentedCompress!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #testFragmentedDecompress!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #testPosition!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #testPredefinedTestCases!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #testUnbufferedCompress!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #testUnbufferedDecompress!public!unit tests! !
!BZipWriteStreamsTest categoriesFor: #uncompressBinary:!helpers!private! !
!BZipWriteStreamsTest categoriesFor: #uncompressText:!helpers!private! !

"Binary Globals"!

"Resources"!

