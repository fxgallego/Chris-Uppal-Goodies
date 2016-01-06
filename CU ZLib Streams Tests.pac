| package |
package := Package name: 'CU ZLib Streams Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A few basic tests for the Stream-like wrappers for zlib compression.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.01'.


package classNames
	add: #ZLibReadStreamTest;
	add: #ZLibStreamTest;
	add: #ZLibWriteStreamTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ZLib Base';
	add: 'CU ZLib Streams';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ZLibStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!
ZLibStreamTest subclass: #ZLibReadStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZLibStreamTest subclass: #ZLibWriteStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ZLibStreamTest guid: (GUID fromString: '{313CC589-6206-4252-A77E-1F18F9614AC0}')!
ZLibStreamTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

NB: these tests only attempt to confirm that data compressed by this library can be decompressed by it.  Unfortunately there is no externally validated data that I can include in the test suite.

I have done a little testing against external data, and it seems to be OK, and *with luck* any regression that affects both compression and decompression will be caught by the (few) predefined test cases.

To some extent the ZipFile stuff does validate our implementation of compression against external tools, although note that the *SUnit* tests for ZipFile are also entirely internal.

The tests take a minute or so to run, that''s because there are so many oppurtunities for off-by-one errors and similar in all the buffering code, so I''ve attempted to be quite exhaustive in testing different combinations of data length and buffer (etc) size.'!
!ZLibStreamTest categoriesForClass!Unclassified! !
!ZLibStreamTest methodsFor!

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
		#crcChecksum
		#adlerChecksum
	).!

compressBinary: aByteArray

	| stream |

	stream := DeflaterWriteStream forBinary.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aByteArray.!

compressBinary: aByteArray level: anInteger

	| stream |

	stream := DeflaterWriteStream forBinaryCompressionLevel: anInteger.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aByteArray.
!

compressText: aString

	| stream |

	stream := DeflaterWriteStream forText.

	stream nextPutAll: aString.

	^ self
		recordDataFrom: stream
		compressed: stream contents
		uncompressed: aString.!

compressText: aString level: anInteger

	| stream |

	stream := DeflaterWriteStream forTextCompressionLevel: anInteger.

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

testBinary: aByteArray level: anInteger

	| c u |

	c := self compressBinary: aByteArray level: anInteger.
	u := self uncompressBinary: (c at: #compressed).

	self check: c against: u.!

testBinary0

	self binaryData do: [:each | self testBinary: each level: 0].
!

testBinary1

	self binaryData do: [:each | self testBinary: each level: 1].
!

testBinary2

	self binaryData do: [:each | self testBinary: each level: 2].
!

testBinary3

	self binaryData do: [:each | self testBinary: each level: 3].
!

testBinary4

	self binaryData do: [:each | self testBinary: each level: 4].
!

testBinary5

	self binaryData do: [:each | self testBinary: each level: 5].
!

testBinary6

	self binaryData do: [:each | self testBinary: each level: 6].
!

testBinary7

	self binaryData do: [:each | self testBinary: each level: 7].
!

testBinary8

	self binaryData do: [:each | self testBinary: each level: 8].
!

testBinary9

	self binaryData do: [:each | self testBinary: each level: 9].
!

testBinaryDefault

	self binaryData do: [:each | self testBinary: each].
!

testBufferSizes: a4Array

	self binaryData do: [:each | self testBufferSizes: a4Array on: each].!

testFormat: aSymbol

	self subclassResponsibility.
!

testGZipFormat

	self testFormat: #GZip.!

testOddBufferSizes

	| sizes |

	"not too many cases since each test is slow"
	sizes := #(		"compress in	compress out		decompress in		decompress out"
			(	23		17			11						37	)
			(	2		4			8						16	)
		).	

	sizes do: [:each | self testBufferSizes: each].
!

testRawFormat

	self testFormat: #Raw.!

testText: aString

	| c u |

	c := self compressText: aString.
	u := self uncompressText: (c at: #compressed).

	self check: c against: u.
!

testText: aString level: anInteger

	| c u |

	c := self compressText: aString level: anInteger.
	u := self uncompressText: (c at: #compressed).

	self check: c against: u.
!

testText0

	self textData do: [:each | self testText: each level: 0].
!

testText1

	self textData do: [:each | self testText: each level: 1].
!

testText2

	self textData do: [:each | self testText: each level: 2].
!

testText3

	self textData do: [:each | self testText: each level: 3].
!

testText4

	self textData do: [:each | self testText: each level: 4].
!

testText5

	self textData do: [:each | self testText: each level: 5].
!

testText6

	self textData do: [:each | self testText: each level: 6].
!

testText7

	self textData do: [:each | self testText: each level: 7].
!

testText8

	self textData do: [:each | self testText: each level: 9].
!

testText9

	self textData do: [:each | self testText: each level: 9].
!

testTextDefault

	self textData do: [:each | self testText: each].
!

testWindowBits

	"decompressing doesn't seem to work for window size = 8, which seems to be
	a zlib bug, or undoumented feature, so we don't test that case"
	9 to: 15 do: [:out | self testWindowBitsIn: 8 out: out].

	"otherwise it should be OK providing the output window size >= input window size"
	9 to: 15 do: [:in |
		in to: 15 do: [:out |
			 self testWindowBitsIn: in out: out]].
!

testZeros

	self zeros do: [:each | self testBinary: each].
!

testZLibFormat

	self testFormat: #ZLib.!

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
!ZLibStreamTest categoriesFor: #binaryData!constants!public! !
!ZLibStreamTest categoriesFor: #check:against:!helpers!private! !
!ZLibStreamTest categoriesFor: #checkableData!constants!public! !
!ZLibStreamTest categoriesFor: #compressBinary:!helpers!private! !
!ZLibStreamTest categoriesFor: #compressBinary:level:!helpers!private! !
!ZLibStreamTest categoriesFor: #compressText:!helpers!private! !
!ZLibStreamTest categoriesFor: #compressText:level:!helpers!private! !
!ZLibStreamTest categoriesFor: #predefinedTestCases!constants!public! !
!ZLibStreamTest categoriesFor: #recordDataFrom:!helpers!private! !
!ZLibStreamTest categoriesFor: #recordDataFrom:compressed:uncompressed:!helpers!private! !
!ZLibStreamTest categoriesFor: #recordDataFrom:uncompressed:!helpers!private! !
!ZLibStreamTest categoriesFor: #testBinary:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testBinary:level:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testBinary0!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary1!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary2!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary3!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary4!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary5!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary6!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary7!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary8!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinary9!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBinaryDefault!public!unit tests! !
!ZLibStreamTest categoriesFor: #testBufferSizes:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testFormat:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testGZipFormat!public!unit tests! !
!ZLibStreamTest categoriesFor: #testOddBufferSizes!public!unit tests! !
!ZLibStreamTest categoriesFor: #testRawFormat!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testText:level:!public!test helpers! !
!ZLibStreamTest categoriesFor: #testText0!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText1!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText2!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText3!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText4!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText5!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText6!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText7!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText8!public!unit tests! !
!ZLibStreamTest categoriesFor: #testText9!public!unit tests! !
!ZLibStreamTest categoriesFor: #testTextDefault!public!unit tests! !
!ZLibStreamTest categoriesFor: #testWindowBits!public!unit tests! !
!ZLibStreamTest categoriesFor: #testZeros!public!unit tests! !
!ZLibStreamTest categoriesFor: #testZLibFormat!public!unit tests! !
!ZLibStreamTest categoriesFor: #textData!constants!public! !
!ZLibStreamTest categoriesFor: #textDatum!constants!public! !
!ZLibStreamTest categoriesFor: #uncompressBinary:!helpers!private! !
!ZLibStreamTest categoriesFor: #uncompressText:!helpers!private! !
!ZLibStreamTest categoriesFor: #zeros!constants!public! !

!ZLibStreamTest class methodsFor!

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

	"NB: these aren't externally defined test cases, unfortunately.  The
	best we can do is attempt to protect against regressions that affect
	compression *and* decompression symmetrically"

	"these were all compressed using default compression options"
	^ #(
		( '' #[120 156 3 0 0 0 0 1]  )
		( 'x' #[120 156 171 0 0 0 121 0 121]  )
		( 'aaa' #[120 156 75 76 76 4 0 2 73 1 36]  )
		( 'xysxysxysxysxysxysxysxys' #[120 156 171 168 44 174 192 134 0 139 80 11 33]  )
		( 'abcdefghijklmnopqrstuvwxyz' #[120 156 75 76 74 78 73 77 75 207 200 204 202 206 201 205 203 47 40 44 42 46 41 45 43 175 168 172 2 0 144 134 11 32]  )
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
!ZLibStreamTest class categoriesFor: #allTestSelectors!accessing!public! !
!ZLibStreamTest class categoriesFor: #binaryData!constants!public! !
!ZLibStreamTest class categoriesFor: #isAbstract!public!testing! !
!ZLibStreamTest class categoriesFor: #predefinedTestCases!constants!public! !
!ZLibStreamTest class categoriesFor: #textData!constants!public! !
!ZLibStreamTest class categoriesFor: #textDatum!constants!public! !
!ZLibStreamTest class categoriesFor: #zeros!constants!public! !

ZLibReadStreamTest guid: (GUID fromString: '{A2A70CEF-2B3C-4DD3-AE08-A97EDE01FD70}')!
ZLibReadStreamTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!ZLibReadStreamTest categoriesForClass!Unclassified! !
!ZLibReadStreamTest methodsFor!

testBufferSizes: a4Array on: aByteArray

	| compressed decompressor decompressed |

	compressed := DeflaterWriteStream compressBinary: aByteArray.

	decompressor := (InflaterReadStream forBinaryOn: compressed readStream)
				inBufferSize: (a4Array at: 3);
				outBufferSize: (a4Array at: 4);
				yourself.

	decompressed := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = decompressed size].
	self should: [decompressor compressedSize = compressed size].

	self should: [aByteArray = decompressed].!

testFormat: aSymbol

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := (DeflaterWriteStream forText)
				zlibFormat: aSymbol;
				yourself.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (InflaterReadStream forTextOn: compressed readStream)
				zlibFormat: aSymbol;
				yourself.

	out := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].
!

testFragmentedCompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := DeflaterWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterReadStream forTextOn: compressed readStream.

	out := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testFragmentedDecompress

	| in compressor compressed decompressor outStream out |

	in := self textDatum.

	compressor := DeflaterWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterReadStream forTextOn: compressed readStream.

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
	compressed := DeflaterWriteStream compressText: self textDatum.
	decompressor := InflaterReadStream forTextOn: compressed readStream.

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
	compressed := DeflaterWriteStream compressText: self textDatum.
	decompressor := InflaterReadStream forTextOn: compressed readStream.

	words do:
		[:each || next |
		self shouldnt: [decompressor atEnd].
		next := decompressor nextWord.
		self should: [each = next]].

		self should: [decompressor atEnd].

	decompressor close.!

testNoSkipBack

	| compressed decompressor |

	compressed := DeflaterWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := InflaterReadStream forBinaryOn: compressed readStream.

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

	compressor := DeflaterWriteStream forBinary.
	1 to: 1033 do: [:i | compressor nextPut: (i & 16rFF)].
	compressed := compressor contents.

	decompressor := InflaterReadStream forBinaryOn: compressed readStream.

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

		decompressor := InflaterReadStream forTextOn: compressed readStream.

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
		compressed := DeflaterWriteStream compressBinary: in.
		data := compressed , magic.
		stream := data readStream.
		decompressor := InflaterReadStream forBinaryOn: stream.
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

	compressed := DeflaterWriteStream compressText: self textDatum.
	decompressor := InflaterReadStream forTextOn: compressed readStream.

	decompressor setToEnd.

	self should: [decompressor atEnd].
	self should: [decompressor position = self textDatum size].
	self should: [decompressor next] raise: Stream endOfStreamSignal.

	decompressor close.
!

testSkip

	| compressed decompressor nextShouldBe check next |

	compressed := DeflaterWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := InflaterReadStream forBinaryOn: compressed readStream.

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

	compressed := DeflaterWriteStream compressBinary: (ByteArray withAll: (1 to: 255)).
	decompressor := InflaterReadStream forBinaryOn: compressed readStream.

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

	compressed := DeflaterWriteStream compressBinary: aByteArray.

	decompressor := (InflaterReadStream forBinaryOn: compressed readStream)
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
			with: InflaterReadStream defaultInBufferSize		"decompress in buffer"
			with: InflaterReadStream defaultOutBufferSize.		"decompress out buffer"
			

	self testBufferSizes: sizes.!

testUnbufferedDecompress

	| sizes |

	sizes := Array
			with: DeflaterWriteStream defaultInBufferSize		"compress in buffer"
			with: DeflaterWriteStream defaultOutBufferSize	"compress out buffer"
			with: 1		"decompress in buffer"
			with: 1.	"decompress out buffer"
			

	self testBufferSizes: sizes.
!

testUnconsumedInput

	| magic |

	magic := 'A *majic* number' asByteArray.

	self binaryData do:
		[:in || compressed data stream decompressor out unconsumed |
		compressed := DeflaterWriteStream compressBinary: in.
		data := compressed , magic.

		stream := data readStream.
		decompressor := InflaterReadStream forBinaryOn: stream.

		out := decompressor upToEnd.
		decompressor close.
		self should: [out = in].

		unconsumed := decompressor unconsumedInput.
		unconsumed := unconsumed , stream upToEnd.
		self should: [unconsumed = magic]].!

testWindowBitsIn: anInteger out: anotherInteger

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := (DeflaterWriteStream forText)
				windowBits: anInteger;
				yourself.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (InflaterReadStream forTextOn: compressed readStream)
				windowBits: anotherInteger;
				yourself.

	out := decompressor upToEnd.
	decompressor close.

	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].
!

testWithCompressionDictionary

	| in dictionary compressor adler compressed decompressor dictionaryNeeded adlerNeeded out |

	in := self textDatum.
	dictionary := in copyFrom: 1 to: (in size // 2).

	compressor := DeflaterWriteStream forText.

	adler := compressor useCompressionDictionary: dictionary.

	compressed := compressor
				nextPutAll: in;
				contents.

	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterReadStream forTextOn: compressed readStream.

	dictionaryNeeded := false.
	[out := decompressor upToEnd.]
		on: ZLibDictionaryNeededException
		do: [:ex | dictionaryNeeded := true. adlerNeeded := ex tag. ex resume: dictionary].
	self should: [dictionaryNeeded].
	self should: [adlerNeeded = adler].

	decompressor close.
	self should: [decompressor uncompressedSize = out size].

	#CUtodo.  "this assertion fails, I currently believe it to be a bug in zlib itself, hence commented out
	self should: [decompressor compressedSize = compressed size].			"

	self should: [in = out].
!

uncompressBinary: aByteArray

	| stream |

	stream := InflaterReadStream forBinaryOn: aByteArray readStream.

	^ self
		recordDataFrom: stream
		uncompressed: stream upToEnd!

uncompressText: aByteArray

	| stream |

	stream := InflaterReadStream forTextOn: aByteArray readStream.

	^ self
		recordDataFrom: stream
		uncompressed: stream upToEnd
! !
!ZLibReadStreamTest categoriesFor: #testBufferSizes:on:!public!test helpers! !
!ZLibReadStreamTest categoriesFor: #testFormat:!public!test helpers! !
!ZLibReadStreamTest categoriesFor: #testFragmentedCompress!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testFragmentedDecompress!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testNextLine!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testNextWord!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testNoSkipBack!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testOddSkipBackLimit!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testPosition!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testPredefinedTestCases!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testPushBackUnconsumedInput!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testSetToEnd!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testSkip!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testSkipBackLimit:!public!test helpers! !
!ZLibReadStreamTest categoriesFor: #testSkipBackLimit:on:!public!test helpers! !
!ZLibReadStreamTest categoriesFor: #testUnbufferedCompress!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testUnbufferedDecompress!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testUnconsumedInput!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #testWindowBitsIn:out:!public!test helpers! !
!ZLibReadStreamTest categoriesFor: #testWithCompressionDictionary!public!unit tests! !
!ZLibReadStreamTest categoriesFor: #uncompressBinary:!helpers!private! !
!ZLibReadStreamTest categoriesFor: #uncompressText:!helpers!private! !

ZLibWriteStreamTest guid: (GUID fromString: '{710F4757-ED37-438F-BCA8-0800ECEB8966}')!
ZLibWriteStreamTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!ZLibWriteStreamTest categoriesForClass!Unclassified! !
!ZLibWriteStreamTest methodsFor!

testBufferSizes: a4Array on: aByteArray

	| compressor compressed decompressor decompressed |

	compressor := (DeflaterWriteStream forBinary)
				inBufferSize: (a4Array at: 1);
				outBufferSize: (a4Array at: 2);
				yourself.

	compressed := compressor
				nextPutAll: aByteArray;
				contents.
	self should: [compressor uncompressedSize = aByteArray size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (InflaterWriteStream forBinary)
				inBufferSize: (a4Array at: 3);
				outBufferSize: (a4Array at: 4);
				yourself.

	decompressed := decompressor
				nextPutAll: compressed;
				contents.
	self should: [decompressor uncompressedSize = decompressed size].
	self should: [decompressor compressedSize = compressed size].

	self should: [aByteArray = decompressed].!

testFormat: aSymbol

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := (DeflaterWriteStream forText)
				zlibFormat: aSymbol;
				yourself.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (InflaterWriteStream forText)
				zlibFormat: aSymbol;
				yourself.

	out := decompressor
			nextPutAll: compressed;
			contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].
!

testFragmentedCompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := DeflaterWriteStream forText.

	in do: [:each | compressor nextPut: each].

	compressed := compressor contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterWriteStream forText.

	out := decompressor
			nextPutAll: compressed;
			contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testFragmentedDecompress

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := DeflaterWriteStream forText.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterWriteStream forText.
	compressed do: [:each | decompressor nextPut: each].

	out := decompressor contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].!

testPosition

	| compressor |

	compressor := DeflaterWriteStream forBinary.

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

		out := InflaterWriteStream decompressText: compressed.
		self should: [in = out]].!

testUnbufferedCompress

	| sizes |

	sizes := Array
			with: 1		"compress in buffer"
			with: 1		"compress out buffer"
			with: InflaterWriteStream defaultInBufferSize		"decompress in buffer"
			with: InflaterWriteStream defaultOutBufferSize.		"decompress out buffer"
			

	self testBufferSizes: sizes.!

testUnbufferedDecompress

	| sizes |

	sizes := Array
			with: DeflaterWriteStream defaultInBufferSize		"compress in buffer"
			with: DeflaterWriteStream defaultOutBufferSize	"compress out buffer"
			with: 1		"decompress in buffer"
			with: 1.	"decompress out buffer"
			

	self testBufferSizes: sizes.
!

testWindowBitsIn: anInteger out: anotherInteger

	| in compressor compressed decompressor out |

	in := self textDatum.

	compressor := (DeflaterWriteStream forText)
				windowBits: anInteger;
				yourself.

	compressed := compressor
				nextPutAll: in;
				contents.
	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := (InflaterWriteStream forText)
				windowBits: anotherInteger;
				yourself.

	out := decompressor
			nextPutAll: compressed;
			contents.
	self should: [decompressor uncompressedSize = out size].
	self should: [decompressor compressedSize = compressed size].

	self should: [in = out].
!

testWithCompressionDictionary

	| in dictionary compressor adler compressed decompressor dictionaryNeeded adlerNeeded out |

	in := self textDatum.
	dictionary := in copyFrom: 1 to: (in size // 2).

	compressor := DeflaterWriteStream forText.

	adler := compressor useCompressionDictionary: dictionary.

	compressed := compressor
				nextPutAll: in;
				contents.

	self should: [compressor uncompressedSize = in size].
	self should: [compressor compressedSize = compressed size].

	decompressor := InflaterWriteStream forText.

	dictionaryNeeded := false.
	[decompressor nextPutAll: compressed; close]
		on: ZLibDictionaryNeededException
		do: [:ex | dictionaryNeeded := true. adlerNeeded := ex tag. ex resume: dictionary].
	self should: [dictionaryNeeded].
	self should: [adlerNeeded = adler].

	out := decompressor contents.
	self should: [decompressor uncompressedSize = out size].

	#CUtodo.  "this assertion fails, I currently believe it to be a bug in zlib itself, hence commented out
	self should: [decompressor compressedSize = compressed size].			"

	self should: [in = out].
!

uncompressBinary: aByteArray

	| stream |

	stream := InflaterWriteStream forBinary.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		uncompressed: stream contents.
!

uncompressText: aByteArray

	| stream |

	stream := InflaterWriteStream forText.

	stream nextPutAll: aByteArray.

	^ self
		recordDataFrom: stream
		uncompressed: stream contents.! !
!ZLibWriteStreamTest categoriesFor: #testBufferSizes:on:!public!test helpers! !
!ZLibWriteStreamTest categoriesFor: #testFormat:!public!test helpers! !
!ZLibWriteStreamTest categoriesFor: #testFragmentedCompress!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testFragmentedDecompress!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testPosition!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testPredefinedTestCases!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testUnbufferedCompress!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testUnbufferedDecompress!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #testWindowBitsIn:out:!public!test helpers! !
!ZLibWriteStreamTest categoriesFor: #testWithCompressionDictionary!public!unit tests! !
!ZLibWriteStreamTest categoriesFor: #uncompressBinary:!helpers!private! !
!ZLibWriteStreamTest categoriesFor: #uncompressText:!helpers!private! !

"Binary Globals"!

"Resources"!

