| package |
package := Package name: 'CU ZLib Base Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

Silly little tests for some of my zlib stuff.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.01'.


package classNames
	add: #ZLibBaseTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ZLib Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ZLibBaseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ZLibBaseTest guid: (GUID fromString: '{739D0023-2ACA-4AF9-8382-735E821E40F4}')!
ZLibBaseTest comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

These tests of the zlib1 library wrapper are not intended to be anything like exhaustive -- they merely consititue a minimal sanity check for the library wrapper.'!
!ZLibBaseTest categoriesForClass!Unclassified! !
!ZLibBaseTest methodsFor!

binaryData

	| str |

	str := ByteArray writeStream.

	5 timesRepeat: [0 to: 255 do: [:i | str nextPut: i]].

	^ str contents.!

compressionLevels

	^ (0 to: 9) , #( -1 ).!

library

	^ ZLib1Library default.!

testCompress: aByteObject type: aZCode compressionLevel: anInteger

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

testCompressBinary

	self testCompressBinary: #[].

	self testCompressBinary: #[128].

	self testCompressBinary: self binaryData.
!

testCompressBinary: aByteArray

	self compressionLevels do: [:each | self testCompressBinary: aByteArray compressionLevel: each].!

testCompressBinary: aByteArray compressionLevel: anInteger

	self testCompress: aByteArray type: Z_BINARY compressionLevel: anInteger.!

testCompressText

	self testCompressText: ''.

	self testCompressText: 'x'.

	self testCompressText: self textData.
!

testCompressText: aString

	self compressionLevels do: [:each | self testCompressText: aString compressionLevel: each].!

testCompressText: aByteArray compressionLevel: anInteger

	self testCompress: aByteArray type: Z_ASCII compressionLevel: anInteger.!

testVersionCompatibility

	self should: [self library checkVersionCompatibility].!

textData

	| str |

	str := String writeStream.

	5 timesRepeat: [0 to: 255 do: [:i | str nextPut: (Character codePoint: i)]].

	^ str contents.! !
!ZLibBaseTest categoriesFor: #binaryData!constants!public! !
!ZLibBaseTest categoriesFor: #compressionLevels!constants!public! !
!ZLibBaseTest categoriesFor: #library!constants!public! !
!ZLibBaseTest categoriesFor: #testCompress:type:compressionLevel:!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressBinary!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressBinary:!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressBinary:compressionLevel:!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressText!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressText:!public!unit tests! !
!ZLibBaseTest categoriesFor: #testCompressText:compressionLevel:!public!unit tests! !
!ZLibBaseTest categoriesFor: #testVersionCompatibility!public!unit tests! !
!ZLibBaseTest categoriesFor: #textData!constants!public! !

!ZLibBaseTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].! !
!ZLibBaseTest class categoriesFor: #allTestSelectors!accessing!public! !

"Binary Globals"!

"Resources"!

