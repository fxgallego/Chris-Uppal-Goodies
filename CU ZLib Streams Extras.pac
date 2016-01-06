| package |
package := Package name: 'CU ZLib Streams Extras'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A few trivial loose methods to supplement the ZLib Streams package.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '1.00'.


package methodNames
	add: #ByteArray -> #compressed;
	add: #ByteArray -> #compressed:;
	add: #ByteArray -> #uncompressedBinary;
	add: #ByteArray -> #uncompressedText;
	add: #String -> #compressed;
	add: #String -> #compressed:;
	add: 'ByteArray class' -> #fromCompressedData:;
	add: 'String class' -> #fromCompressedData:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ZLib Streams';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

compressed
	"Answer a ByteArray that contains the reciever compressed at the default
	compression level."

#CUadded.
	^ DeflaterWriteStream compressBinary: self.!

compressed: anInteger
	"Answer a ByteArray that contains the reciever compressed at the given
	compression level/"

#CUadded.
	^ DeflaterWriteStream compressBinary: self compressionLevel: anInteger.!

uncompressedBinary
	"Answer a new ByteArray created by decompressing the data in the receiver."

#CUadded.
	^ ByteArray fromCompressedData: self.!

uncompressedText
	"Answer a new String created by decompressing the data in the receiver."

#CUadded.
	^ String fromCompressedData: self.! !
!ByteArray categoriesFor: #compressed!compressing!converting!public! !
!ByteArray categoriesFor: #compressed:!compressing!converting!public! !
!ByteArray categoriesFor: #uncompressedBinary!compressing!converting!public! !
!ByteArray categoriesFor: #uncompressedText!compressing!converting!public! !

!ByteArray class methodsFor!

fromCompressedData: aByteArray
	"Answer a new instance that contains the result of uncompressing the data in aByteArray."

#CUadded.
	^ (InflaterWriteStream forBinaryOn: self writeStream)
		nextPutAll: aByteArray;
		contents.! !
!ByteArray class categoriesFor: #fromCompressedData:!decompressing!public! !

!String methodsFor!

compressed
	"answer a ByteArray that contains the reciever compressed at the default
	compression level"

#CUadded.
	^ DeflaterWriteStream compressText: self.!

compressed: anInteger
	"answer a ByteArray that contains the reciever compressed at the given
	compression level"

#CUadded.
	^ DeflaterWriteStream compressText: self compressionLevel: anInteger.! !
!String categoriesFor: #compressed!compressing!converting!public! !
!String categoriesFor: #compressed:!compressing!converting!public! !

!String class methodsFor!

fromCompressedData: aByteArray
	"Answer a new instance that contains the result of uncompressing the data in aByteArray."

#CUadded.
	^ (InflaterWriteStream forTextOn: self writeStream)
		nextPutAll: aByteArray;
		contents.! !
!String class categoriesFor: #fromCompressedData:!decompressing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

"Resources"!

