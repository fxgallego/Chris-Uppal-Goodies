| package |
package := Package name: 'CU BZip2 Base Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Silly little tests for some of my bzip2 stuff.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package classNames
	add: #BZip2BaseTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU BZip2 Base';
	add: 'CU ZLib Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #BZip2BaseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'BZip2Constants ZLib1Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BZip2BaseTest guid: (GUID fromString: '{C25B3485-3CD0-444A-97ED-F576D43FB517}')!
BZip2BaseTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

These tests of the bzip2 library wrapper are not intended to be anything like exhaustive -- they merely consititue a minimal sanity check for the library wrapper.
'!
!BZip2BaseTest categoriesForClass!Unclassified! !
!BZip2BaseTest methodsFor!

blockSizes

	^ (1 to: 9) asArray.!

data

	| str |

	str := ByteArray writeStream.

	5 timesRepeat: [0 to: 255 do: [:i | str nextPut: i]].

	^ str contents.!

library

	^ BZip2Library default.!

testCompress: aByteArray

	self blockSizes do: [:each | self testCompress: aByteArray blockSize: each].!

testCompress: aByteArray blockSize: anInteger

	self workFactors do: [:each | self testCompress: aByteArray blockSize: anInteger workFactor: each].!

testCompress: aByteObject blockSize: anInteger workFactor: anotherInteger

	| lib in bzIn status compressed out bzOut |

	lib := self library.
	in := aByteObject.

	compressed := ByteArray new: (in size * 5 + 100).
	bzIn := (BZIP2Stream new)
			next_in: in yourAddress;
			avail_in: in size;
			next_out: compressed yourAddress;
			avail_out: compressed size;
			yourself.

	status := lib compressInit: bzIn blockSize: anInteger workFactor: anotherInteger.
	self should: [status = BZ_OK].

	bzIn avail_in > 0 ifTrue:
		[status := lib compressRun: bzIn.
		self should: [status = BZ_RUN_OK]].

	"since the output buffer was 'big enough' this should finish immediately,
	hence will answer Z_STREAM_END rather than BZ_FINISH_OK"
	status := lib compressFinish: bzIn.
	self should: [status = BZ_STREAM_END].

	status := lib compressEnd: bzIn.
	self should: [status = BZ_OK].

	"tempting to assert that compressed size < in size, but of course that won't do"
	compressed := compressed allButLast: bzIn avail_out.

	out := aByteObject class new: (in size max: compressed size) * 5 + 100.
	bzOut := (BZIP2Stream new)
			next_in: compressed yourAddress;
			avail_in: compressed size;
			next_out: out yourAddress;
			avail_out: out size;
			yourself.

	status := lib decompressInit: bzOut small: false.
	self should: [status = BZ_OK].

	"since the output buffer was 'big enough' this should finish immediately,
	hence will answer BZ_STREAM_END rather than BZ_OK"
	status := lib decompress: bzOut.
	status = BZ_OK.
	self should: [status = BZ_STREAM_END].

	status := lib decompressEnd: bzOut.
	self should: [status = BZ_OK].

	out := out allButLast: bzOut avail_out.
	self should: [out = in].
!

testCompress: aByteObject type: aZCode blockSize: anInteger

	| lib in zIn status compressed out zOut |

	lib := self library.
	in := aByteObject.

	compressed := ByteArray new: (lib compressBound: in size).
		zIn := (ZLIBStream new)
				next_in: in yourAddress;
				avail_in: in size;
				next_out: compressed yourAddress;
				avail_out: compressed size;
				data_type: aZCode;
				yourself.

	status := lib deflateInitStream: zIn level: anInteger.
	self should: [status = Z_OK].

	"since the output buffer is 'big enough' this should finish in one go,
	hence will answer Z_STREAM_END rather than Z_OK"
	status := lib deflateStream: zIn flush: Z_FINISH.
	self should: [status = Z_STREAM_END].

	status := lib deflateEndStream: zIn.
	self should: [status = Z_OK].

	"tempting to assert that compressed size < in size, but of course that won't do"
	compressed := compressed allButLast: zIn avail_out.

	out := aByteObject class new: (in size max: compressed size).
	zOut := (ZLIBStream new)
				next_in: compressed yourAddress;
				avail_in: compressed size;
				next_out: out yourAddress;
				avail_out: out size;
				data_type: Z_BINARY;
				yourself.

	status := lib inflateInitStream: zOut.
	self should: [status = Z_OK].

	"since the output buffer is 'big enough' this should finish in one go,
	hence will answer Z_STREAM_END rather than Z_OK"
	status := lib inflateStream: zOut flush: Z_FINISH.
	self should: [status = Z_STREAM_END].

	status := lib inflateEndStream: zOut.
	self should: [status = Z_OK].

	out := out allButLast: zOut avail_out.
	self should: [out = in].
	self should: [zOut adler = zIn adler].
!

testCompressEmpty

	self testCompress: #[].
!

testCompressNormal

	self testCompress: self data.
!

testCompressSmall

	self testCompress: #[128].
!

workFactors

	^ #(0 10 30 100 200 230).! !
!BZip2BaseTest categoriesFor: #blockSizes!constants!public! !
!BZip2BaseTest categoriesFor: #data!constants!public! !
!BZip2BaseTest categoriesFor: #library!constants!public! !
!BZip2BaseTest categoriesFor: #testCompress:!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompress:blockSize:!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompress:blockSize:workFactor:!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompress:type:blockSize:!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompressEmpty!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompressNormal!public!unit tests! !
!BZip2BaseTest categoriesFor: #testCompressSmall!public!unit tests! !
!BZip2BaseTest categoriesFor: #workFactors!constants!public! !

!BZip2BaseTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].! !
!BZip2BaseTest class categoriesFor: #allTestSelectors!accessing!public! !

"Binary Globals"!

"Resources"!

