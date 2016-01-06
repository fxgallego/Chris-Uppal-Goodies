| package |
package := Package name: 'CU ZLib Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

A "raw" wrapper for the zlib library, in the form of the ZLIB1.DLL distrubuted from http://www.gzip.org/zlib/

This is a very low-level wrapper, see the packages ''CU ZLib Streams'' and ''CU ZipFiles'' for something that''s actually intended to be usefull.

This wrapper only covers the zlib (de)compression stuff.  The library itself has about 20 functions for handling gzipped files as if they were ''C'' <stdio> streams -- I have ignored all of that.

ZLib itself is covered by its own copyright notice, the following only applies to my stuff.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package classNames
	add: #ZLib1Library;
	add: #ZLibDictionaryNeededException;
	add: #ZLibError;
	add: #ZLIBStream;
	yourself.

package globalNames
	add: #ZLib1Constants;
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

Error subclass: #ZLibDictionaryNeededException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #ZLibError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #ZLib1Library
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ZLIBStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #ZLib1Constants put: (PoolConstantsDictionary named: #ZLib1Constants)!
ZLib1Constants at: 'Z_ASCII' put: 16r1!
ZLib1Constants at: 'Z_BEST_COMPRESSION' put: 16r9!
ZLib1Constants at: 'Z_BEST_SPEED' put: 16r1!
ZLib1Constants at: 'Z_BINARY' put: 16r0!
ZLib1Constants at: 'Z_BLOCK' put: 16r5!
ZLib1Constants at: 'Z_BUF_ERROR' put: -16r5!
ZLib1Constants at: 'Z_DATA_ERROR' put: -16r3!
ZLib1Constants at: 'Z_DEFAULT_COMPRESSION' put: -16r1!
ZLib1Constants at: 'Z_DEFAULT_STRATEGY' put: 16r0!
ZLib1Constants at: 'Z_DEFLATED' put: 16r8!
ZLib1Constants at: 'Z_ERRNO' put: -16r1!
ZLib1Constants at: 'Z_FILTERED' put: 16r1!
ZLib1Constants at: 'Z_FINISH' put: 16r4!
ZLib1Constants at: 'Z_FULL_FLUSH' put: 16r3!
ZLib1Constants at: 'Z_HUFFMAN_ONLY' put: 16r2!
ZLib1Constants at: 'Z_MEM_ERROR' put: -16r4!
ZLib1Constants at: 'Z_NEED_DICT' put: 16r2!
ZLib1Constants at: 'Z_NO_COMPRESSION' put: 16r0!
ZLib1Constants at: 'Z_NO_FLUSH' put: 16r0!
ZLib1Constants at: 'Z_NULL' put: 16r0!
ZLib1Constants at: 'Z_OK' put: 16r0!
ZLib1Constants at: 'Z_PARTIAL_FLUSH' put: 16r1!
ZLib1Constants at: 'Z_RLE' put: 16r3!
ZLib1Constants at: 'Z_STREAM_END' put: 16r1!
ZLib1Constants at: 'Z_STREAM_ERROR' put: -16r2!
ZLib1Constants at: 'Z_SYNC_FLUSH' put: 16r2!
ZLib1Constants at: 'Z_UNKNOWN' put: 16r2!
ZLib1Constants at: 'Z_VERSION_ERROR' put: -16r6!
ZLib1Constants at: 'ZLIB_VERNUM' put: 16r1210!
ZLib1Constants at: 'ZLIB_VERSION' put: '1.2.1'!
ZLib1Constants shrink!

"Classes"!

ZLibDictionaryNeededException guid: (GUID fromString: '{F84BC768-D0DC-4755-9EF1-5D845612C70C}')!
ZLibDictionaryNeededException comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A *resumable* error stating that zlib cannor carry on without being given a decompression dictionay.

The word ''dictionary'' here is a bit misleading, all the library really wants is some example text"

The tag of the exception is the adler checksum of the dictionary that zlib is expecting to be given (so that you can use more than one dictionary if you wish -- prociding they have different checksums).

A decompressor wth throw this exception when it discovers that it needs the dictionary, if you want to provide one then you should #resume: with the same String or ByteArray that was used to #setCompressionDIctionary: of the compressor.

NB: this feature is very rarly used, but it can be usefull for improving the compression of short texts (especially) when you know what the typicall text to compress looks like in advance.'!
!ZLibDictionaryNeededException categoriesForClass!Unclassified! !
!ZLibDictionaryNeededException methodsFor!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^ 'ZLib: Dictionary need for decompression, Adler checksum: %2!!%Xd!!'.!

errorCode
	"answer the error code that caused this exception"

	^ self tag.!

isResumable
	"answer true because we are resumable"

	^ true.! !
!ZLibDictionaryNeededException categoriesFor: #_descriptionFormat!displaying!public! !
!ZLibDictionaryNeededException categoriesFor: #errorCode!accessing!public! !
!ZLibDictionaryNeededException categoriesFor: #isResumable!public!testing! !

ZLibError guid: (GUID fromString: '{65B4A9DD-E927-4ED4-924B-405837A73760}')!
ZLibError comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!ZLibError categoriesForClass!Unclassified! !
!ZLibError methodsFor!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^ 'ZLib Error: %1 (%2)'.!

errorCode
	"answer the error code that caused this exception"

	^ self tag.! !
!ZLibError categoriesFor: #_descriptionFormat!displaying!public! !
!ZLibError categoriesFor: #errorCode!accessing!public! !

ZLib1Library guid: (GUID fromString: '{16854272-96D3-4369-A9EE-48E8A0D31067}')!
ZLib1Library comment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

This is a wrapper for the *official* zlib library -- zlib1.dll -- distributed at http://www.gzip.org/zlib/

ZLib itself is:
Copyright © 1995-2003 Jean-loup Gailly and Mark Adler

Note that there are many zlib.dll files floating around the Net -- I have at least 5 on this machine, all different -- and there is more than one Dolphin wrapper for these too.

This is the *only* wrapper written by *me* for the *official* zlib library.  That makes it kinda special ;-)

Note that the methods in the zlib-* categories are links to the corresponding zlib1.dll exports, and reproduce the comments on those functions from zlib.h.  BTW, I would like to say that I am extremely gratefull to the zlib people for distributing it with a licence that makes it possible to reproduce elements of the documentation in this way.  (Over and above being gratefull for zlib itself, of course!!)

One last pont -- at the time of writing, if a method in the ''zlib-advanced functions'' category is not called from somewhere in one of my ''zlib'' packages, then it''s a fairly safe bet that I''ve never called it at all, let alone *tested* it...

	-- chris


The following is reproduced (with some C-specific stuff elided) from zlib,.h:

==============================================================

  zlib.h -- interface of the ''zlib'' general purpose compression library
  version 1.2.1, November 17th, 2003

  Copyright (C) 1995-2003 Jean-loup Gailly and Mark Adler

  This software is provided ''as-is'', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files http://www.ietf.org/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).

     The ''zlib'' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

     Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap''ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The compressed data format used by the in-memory functions is the zlib
  format, which is a zlib wrapper documented in RFC 1950, wrapped around a
  deflate stream, which is itself documented in RFC 1951.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio using the functions that start
  with "gz".  The gzip format is different from the zlib format.  gzip is a
  gzip wrapper, documented in RFC 1952, wrapped around a deflate stream.

     The zlib format was designed to be compact and fast for use in memory
  and on communications channels.  The gzip format was designed for single-
  file compression on file systems, has a larger header than zlib to maintain
  directory information, and uses a different, slower check method than zlib.

     This library does not provide any functions to write gzip files in memory.
  However such functions could be easily written using zlib''s deflate function,
  the documentation in the gzip RFC, and the examples in gzio.c.

     The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input.
'!
!ZLib1Library categoriesForClass!Unclassified! !
!ZLib1Library methodsFor!

adler32: aByteArray
	"invoke the adler2() method from ZLIB1.DLL to compute the 32bit Adler crc of the given byte array"

	^ self
		adler32: (self initialAdler32)
		buffer: aByteArray yourAddress
		size: aByteArray size.!

adler32: anInteger buffer: anExternalAddress size: anotherInteger
	"invoke the adler32() method from ZLIB1.DLL

	From zlib.h:
		Update a running Adler-32 checksum with the bytes buf[0..len-1] and
		return the updated checksum. If buf is NULL, this function returns
		the required initial value for the checksum.
		An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
		much faster.

		[from the example it is clear that you should 'seed' the computation with:
			ZLib1Library default adler32: 0 buffer: 0 size: 0.
		which is 1]


		uLong __cdecl adler32(uLong adler, const Bytef *buf, uInt len);
	"

	<cdecl: dword adler32 dword void* dword>
	^ self invalidCall.!

assertVersionCompatibility
	"checks the version compatibility and throws an assertion error if they don't martch"

	self assert: [self checkVersionCompatibility].!

checkVersionCompatibility
	"answer true iff the version of the library is consistant with the version of zlib.h that was used
	to create this package.

		self default checkVersionCompatibility.
	"

	^ self zlibVersion first = self expectedVersion first.!

compileTimeSettings
	"answer a Dictionary of the compile-time settings of the wrapped DLL.

		self default compileTimeSettings.
	"

	| flags settings sizes |

	flags := self zlibCompileFlags.
	settings := IdentityDictionary new.

	"size of types, -1 means 'other' "
	sizes := #(16 32 64 -1).
	settings at: #UintSize put: (sizes at: (flags & 16r3) + 1).
	settings at: #UlongSize put: (sizes at: (flags >> 2 & 16r3) + 1).
	settings at: #PointerSize put: (sizes at: (flags >> 4 & 16r3) + 1).
	settings at: #OffsetSize put: (sizes at: (flags >> 6 & 16r3) + 1).

	"compiler, assembler, and debug options"
	settings at: #DebugBuild put: (flags isBitSet: 9).
	settings at: #UsesASMCode put: (flags isBitSet: 10).
	settings at: #UsesWINAPICallingConvention put: (flags isBitSet: 11).

	"bit 12 (in 'C' terms 11) is reserved"

	"one-time table building (makes for smaler code but is not threadsafe)"
	settings at: #BuildBlockDecodingTablesOnDemand put: (flags isBitSet: 13).
	settings at: #BuildCRCTablesOnDemand put: (flags isBitSet: 14).

	"bits 15 and 16 (aka 14 and 15) are reserved"

	"lbrary content (indicates missing functionality)"
	settings at: #NoGZCompress put: (flags isBitSet: 17).
	settings at: #NoGZip put: (flags isBitSet: 18).

	"bits 19 and 20 (aka 18 and 19) are reserved"

	"operation variations (changes in library functionality)"
	settings at: #PKZipBugWorkaround put: (flags isBitSet: 21).
	settings at: #FastestOnly put: (flags isBitSet: 22).

	"bits 23 and 24 (aka 22 and 23) are reserved"

	"sprintf() variant used by gzprintf -- not that it makes any difference..."
	settings at: #SprintfIsLimitedTo20Arguments put: (flags isBitSet: 25).
	settings at: #SprintfIsUnsafe put: (flags isBitSet: 26).
	settings at: #SprintfReturnIsGuesswork put: (flags isBitSet: 27).

	^ settings.!

compressBound: anInteger
	"invoke the compressBound() method from ZLIB1.DLL

	From zlib.h:
		compressBound() returns an upper bound on the compressed size after
		compress() or compress2() on sourceLen bytes.  It would be used before
		a compress() or compress2() call to allocate the destination buffer.


		uLong __cdecl compressBound(uLong sourceLen);
	"

	<cdecl: dword compressBound dword>
	^ self invalidCall.!

compressDestination: anExternalAddress1 destinationSizePointer: anExternalAddress2 souce: anExternalAddress3 sourceSize: anInteger
	"invoke the compress() method from ZLIB1.DLL

	From zlib.h:
		Compresses the source buffer into the destination buffer.  sourceLen is
		the byte length of the source buffer. Upon entry, destLen is the total
		size of the destination buffer, which must be at least the value returned
		by compressBound(sourceLen). Upon exit, destLen is the actual size of the
		compressed buffer.
		This function can be used to compress a whole file at once if the
		input file is mmap'ed.
		compress returns Z_OK if success, Z_MEM_ERROR if there was not
		enough memory, Z_BUF_ERROR if there was not enough room in the output
		buffer.


		int __cdecl compress(char *dest,  uLong *destLen, char *source, uLong sourceLen);
	"

	<cdecl: sdword compress void* dword* void* dword>
	^ self invalidCall.!

compressDestination: anExternalAddress1 destinationSizePointer: anExternalAddress2 souce: anExternalAddress3 sourceSize: anInteger level: anotherInteger
	"invoke the compress() method from ZLIB1.DLL

	From zlib.h:
		Compresses the source buffer into the destination buffer. The level
		parameter has the same meaning as in deflateInit.  sourceLen is the byte
		length of the source buffer. Upon entry, destLen is the total size of the
		destination buffer, which must be at least the value returned by
		compressBound(sourceLen). Upon exit, destLen is the actual size of the
		compressed buffer.

		compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
		memory, Z_BUF_ERROR if there was not enough room in the output buffer,
		Z_STREAM_ERROR if the level parameter is invalid.


		int __cdecl compress2(char *dest,  uLong *destLen, char *source, uLong sourceLen, int level);
	"

	<cdecl: sdword compress2 void* dword* void* dword dword>
	^ self invalidCall.!

crc32: aByteArray
	"invoke the crc32() method from ZLIB1.DLL to compute the 32bit crc of the given byte array"

	^ self
		crc32: (self initialCrc32)
		buffer: aByteArray yourAddress
		size: aByteArray size.!

crc32: anInteger buffer: anExternalAddress size: anotherInteger
	"invoke the crc32() method from ZLIB1.DLL

	From zlib.h:
		Update a running crc with the bytes buf[0..len-1] and return the updated
		crc. If buf is NULL, this function returns the required initial value
		for the crc. Pre- and post-conditioning (one's complement) is performed
		within this function so it shouldn't be done by the application.


		uLong __cdecl crc32(uLong adler, const Bytef *buf, uInt len);
	"

	<cdecl: dword crc32 dword void* dword>
	^ self invalidCall.!

deflateBoundStream: aZLIBStream sourceLen: anInteger
	"invoke the deflateBound() method from ZLIB1.DLL

	From zlib.h:
		deflateBound() returns an upper bound on the compressed size after
		deflation of sourceLen bytes.  It must be called after deflateInit()
		or deflateInit2().  This would be used to allocate an output buffer
		for deflation in a single pass, and so would be called before deflate().


		int __cdecl deflateBound(z_stream *strm, uLong sourceLen);
	"

	<cdecl: sdword deflateBound ZLIBStream* dword>
	^ self invalidCall.!

deflateCopyDestination: aZLIBStream1 source: aZLIBStream2
	"invoke the deflateCopy() method from ZLIB1.DLL

	From zlib.h:
		Sets the destination stream as a complete copy of the source stream.

		This function can be useful when several compression strategies will be
		tried, for example when there are several ways of pre-processing the input
		data with a filter. The streams that will be discarded should then be freed
		by calling deflateEnd.  Note that deflateCopy duplicates the internal
		compression state which can be quite large, so this strategy is slow and
		can consume lots of memory.

		deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
		enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
		(such as zalloc being NULL). msg is left unchanged in both source and
		destination.


		int __cdecl deflateCopy(z_stream *dest, z_stream *source);

	"

	<cdecl: sdword deflateCopy ZLIBStream* ZLIBStream*>
	^ self invalidCall.!

deflateEndStream: aZLIBStream
	"invoke the deflateEnd() method from ZLIB1.DLL

	From zlib.h:
		All dynamically allocated data structures for this stream are freed.
		This function discards any unprocessed input and does not flush any
		pending output.

		deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
		stream state was inconsistent, Z_DATA_ERROR if the stream was freed
		prematurely (some input or output was discarded). In the error case,
		msg may be set but then points to a static string (which must not be
		deallocated).


		int __cdecl deflateEnd(z_stream *strm);
	"

	<cdecl: sdword deflateEnd ZLIBStream*>
	^ self invalidCall.!

deflateInit2Stream: aZLIBStream level: anInteger1 method: anInteger2 windowBits: anInteger3 memLevel: anInteger4 strategy: anInteger5
	"invoke the deflateInit2() method from ZLIB1.DLL

	From zlib.h:
		This is another version of deflateInit with more compression options. The
		fields next_in, zalloc, zfree and opaque must be initialized before by
		the caller.

		The method parameter is the compression method. It must be Z_DEFLATED in
		this version of the library.

		The windowBits parameter is the base two logarithm of the window size
		(the size of the history buffer). It should be in the range 8..15 for this
		version of the library. Larger values of this parameter result in better
		compression at the expense of memory usage. The default value is 15 if
		deflateInit is used instead.

		windowBits can also be -8..-15 for raw deflate. In this case, -windowBits
		determines the window size. deflate() will then generate raw deflate data
		with no zlib header or trailer, and will not compute an adler32 check value.

		windowBits can also be greater than 15 for optional gzip encoding. Add
		16 to windowBits to write a simple gzip header and trailer around the
		compressed data instead of a zlib wrapper. The gzip header will have no
		file name, no extra data, no comment, no modification time (set to zero),
		no header crc, and the operating system will be set to 255 (unknown).

		The memLevel parameter specifies how much memory should be allocated
		for the internal compression state. memLevel=1 uses minimum memory but
		is slow and reduces compression ratio; memLevel=9 uses maximum memory
		for optimal speed. The default value is 8. See zconf.h for total memory
		usage as a function of windowBits and memLevel.

		The strategy parameter is used to tune the compression algorithm. Use the
		value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
		filter (or predictor), Z_HUFFMAN_ONLY to force Huffman encoding only (no
		string match), or Z_RLE to limit match distances to one (run-length
		encoding). Filtered data consists mostly of small values with a somewhat
		random distribution. In this case, the compression algorithm is tuned to
		compress them better. The effect of Z_FILTERED is to force more Huffman
		coding and less string matching; it is somewhat intermediate between
		Z_DEFAULT and Z_HUFFMAN_ONLY. Z_RLE is designed to be almost as fast as
		Z_HUFFMAN_ONLY, but give better compression for PNG image data. The strategy
		parameter only affects the compression ratio but not the correctness of the
		compressed output even if it is not set appropriately.

		deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
		memory, Z_STREAM_ERROR if a parameter is invalid (such as an invalid
		method). msg is set to null if there is no error message.  deflateInit2 does
		not perform any compression: this will be done by deflate().


		int __cdecl deflateInit2(
					z_stream *trm,
					int  level,
					int  method,
					int  windowBits,
					int  memLevel,
					int  strategy);
	"

	"deflateInit2() is actually implemented as a macro that expands to something like"
	^ self
		deflateInit2Stream: aZLIBStream
		level: anInteger1
		method: anInteger2
		windowBits: anInteger3
		memLevel: anInteger4
		strategy: anInteger5
		version: self expectedVersion
		size: aZLIBStream byteSize.
!

deflateInit2Stream: aZLIBStream level: anInteger1 method: anInteger2 windowBits: anInteger3 memLevel: anInteger4 strategy: anInteger5 version: aString size: aInteger6
	"private -- invoke the deflateInit2() method from ZLIB1.DLL

	This method is 'private' in zlib -- the published function, deflateInit2(), is actually a macro
	that forwards to this method, passing the compile-time version and size parameters
	so that the run-time can do a bit of sanity checking.


		int __cdecl deflateInit2(
					z_stream *strm,
					int  level,
					int  method,
					int  windowBits,
					int  memLevel,
					int  strategy,
					char *version,
					int size);
	"

	<cdecl: sdword deflateInit2_ ZLIBStream* sdword sdword sdword sdword sdword char* sdword>
	^ self invalidCall.!

deflateInitStream: aZLIBStream level: anInteger
	"invoke the deflateInit() method from ZLIB1.DLL

	From zlib.h:
		Initializes the internal stream state for compression. The fields
		zalloc, zfree and opaque must be initialized before by the caller.
		If zalloc and zfree are set to Z_NULL, deflateInit updates them to
		use default allocation functions.

		The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
		1 gives best speed, 9 gives best compression, 0 gives no compression at
		all (the input data is simply copied a block at a time).
		Z_DEFAULT_COMPRESSION requests a default compromise between speed and
		compression (currently equivalent to level 6).

		deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
		enough memory, Z_STREAM_ERROR if level is not a valid compression level,
		Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
		with the version assumed by the caller (ZLIB_VERSION).
		msg is set to null if there is no error message.  deflateInit does not
		perform any compression: this will be done by deflate().


		int __cdecl deflateInit(z_stream *strm, int level);
	"

	"deflateInit() is actually implemented as a macro that expands to something like"
	^ self
		deflateInitStream: aZLIBStream
		level: anInteger
		version: self expectedVersion
		size: aZLIBStream byteSize.!

deflateInitStream: aZLIBStream level: anInteger version: aString size: anotherInteger
	"private -- invoke the deflateInit_() method from ZLIB1.DLL

	This method is 'private' in zlib -- the published function, deflateInit(), is actually a macro
	that forwards to this method, passing the compile-time version and size parameters
	so that the run-time can do a bit of sanity checking.


		int __cdecl deflateInit_(z_stream *strm, int level, char* version, int size);
	"

	<cdecl: sdword deflateInit_ ZLIBStream* sdword char* sdword>
	^ self invalidCall.!

deflateParamsStream: aZLIBStream level: anInteger1 strategy: anInteger5
	"invoke the deflateParams() method from ZLIB1.DLL

	From zlib.h:
		Dynamically update the compression level and compression strategy.  The
		interpretation of level and strategy is as in deflateInit2.  This can be
		used to switch between compression and straight copy of the input data, or
		to switch to a different kind of input data requiring a different
		strategy. If the compression level is changed, the input available so far
		is compressed with the old level (and may be flushed); the new level will
		take effect only at the next call of deflate().

		Before the call of deflateParams, the stream state must be set as for
		a call of deflate(), since the currently available input may have to
		be compressed and flushed. In particular, strm->avail_out must be non-zero.

		deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
		stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
		if strm->avail_out was zero.


		int __cdecl deflateParams(z_stream *strm, int level, int strategy);
	"

	<cdecl: sdword deflateParams ZLIBStream* sdword sdword>
	^ self invalidCall.!

deflatePrimeStream: aZLIBStream bits: anInteger1 value: anInteger2
	"invoke the deflatePrime() method from ZLIB1.DLL

	From zlib.h:
		deflatePrime() inserts bits in the deflate output stream.  The intent
		is that this function is used to start off the deflate output with the
		bits leftover from a previous deflate stream when appending to it.  As such,
		this function can only be used for raw deflate, and must be used before the
		first deflate() call after a deflateInit2() or deflateReset().  bits must be
		less than or equal to 16, and that many of the least significant bits of
		value will be inserted in the output.

		deflatePrime returns Z_OK if success, or Z_STREAM_ERROR if the source
		stream state was inconsistent.


		int __cdecl deflatePrime(z_stream *strm, int bits, int value);
	"

	<cdecl: sdword deflatePrime ZLIBStream* sdword sdword>
	^ self invalidCall.!

deflateResetStream: aZLIBStream
	"invoke the deflateReset() method from ZLIB1.DLL

	From zlib.h:
		This function is equivalent to deflateEnd followed by deflateInit,
		but does not free and reallocate all the internal compression state.
		The stream will keep the same compression level and any other attributes
		that may have been set by deflateInit2.

		deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
		stream state was inconsistent (such as zalloc or state being NULL).


		int __cdecl deflateReset(z_stream *strm);
	"

	<cdecl: sdword deflateReset ZLIBStream*>
	^ self invalidCall.!

deflateSetDictionary: aZLIBStream dictionary: anExternalAddress dictLength: anInteger
	"invoke the deflateSetDictionary() method from ZLIB1.DLL

	From zlib.h:
		Initializes the compression dictionary from the given byte sequence
		without producing any compressed output. This function must be called
		immediately after deflateInit, deflateInit2 or deflateReset, before any
		call of deflate. The compressor and decompressor must use exactly the same
		dictionary (see inflateSetDictionary).

		The dictionary should consist of strings (byte sequences) that are likely
		to be encountered later in the data to be compressed, with the most commonly
		used strings preferably put towards the end of the dictionary. Using a
		dictionary is most useful when the data to be compressed is short and can be
		predicted with good accuracy; the data can then be compressed better than
		with the default empty dictionary.

		Depending on the size of the compression data structures selected by
		deflateInit or deflateInit2, a part of the dictionary may in effect be
		discarded, for example if the dictionary is larger than the window size in
		deflate or deflate2. Thus the strings most likely to be useful should be
		put at the end of the dictionary, not at the front.

		Upon return of this function, strm->adler is set to the adler32 value
		of the dictionary; the decompressor may later use this value to determine
		which dictionary has been used by the compressor. (The adler32 value
		applies to the whole dictionary even if only a subset of the dictionary is
		actually used by the compressor.) If a raw deflate was requested, then the
		adler32 value is not computed and strm->adler is not set.

		deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
		parameter is invalid (such as NULL dictionary) or the stream state is
		inconsistent (for example if deflate has already been called for this stream
		or if the compression method is bsort). deflateSetDictionary does not
		perform any compression: this will be done by deflate().


		int __cdecl deflateSetDictionary OF(
					z_stream *strm,
					const Bytef *dictionary,
					uInt  dictLength);
	"

	<cdecl: sdword deflateSetDictionary ZLIBStream* void* dword>
	^ self invalidCall.!

deflateStream: aZLIBStream flush: anInteger
	"invoke the deflate() method from ZLIB1.DLL

	From zlib.h:
		deflate compresses as much data as possible, and stops when the input
		buffer becomes empty or the output buffer becomes full. It may introduce some
		output latency (reading input without producing any output) except when
		forced to flush.

		The detailed semantics are as follows. deflate performs one or both of the
		following actions:

		- Compress more input starting at next_in and update next_in and avail_in
		  accordingly. If not all input can be processed (because there is not
		  enough room in the output buffer), next_in and avail_in are updated and
		  processing will resume at this point for the next call of deflate().

		- Provide more output starting at next_out and update next_out and avail_out
		  accordingly. This action is forced if the parameter flush is non zero.
		  Forcing flush frequently degrades the compression ratio, so this parameter
		  should be set only when necessary (in interactive applications).
		  Some output may be provided even if flush is not set.

		Before the call of deflate(), the application should ensure that at least
		one of the actions is possible, by providing more input and/or consuming
		more output, and updating avail_in or avail_out accordingly; avail_out
		should never be zero before the call. The application can consume the
		compressed output when it wants, for example when the output buffer is full
		(avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
		and with zero avail_out, it must be called again after making room in the
		output buffer because there might be more output pending.

		If the parameter flush is set to Z_SYNC_FLUSH, all pending output is
		flushed to the output buffer and the output is aligned on a byte boundary, so
		that the decompressor can get all input data available so far. (In particular
		avail_in is zero after the call if enough output space has been provided
		before the call.)  Flushing may degrade compression for some compression
		algorithms and so it should be used only when necessary.

		If flush is set to Z_FULL_FLUSH, all output is flushed as with
		Z_SYNC_FLUSH, and the compression state is reset so that decompression can
		restart from this point if previous compressed data has been damaged or if
		random access is desired. Using Z_FULL_FLUSH too often can seriously degrade
		the compression.

		If deflate returns with avail_out == 0, this function must be called again
		with the same value of the flush parameter and more output space (updated
		avail_out), until the flush is complete (deflate returns with non-zero
		avail_out). In the case of a Z_FULL_FLUSH or Z_SYNC_FLUSH, make sure that
		avail_out is greater than six to avoid repeated flush markers due to
		avail_out == 0 on return.

		If the parameter flush is set to Z_FINISH, pending input is processed,
		pending output is flushed and deflate returns with Z_STREAM_END if there
		was enough output space; if deflate returns with Z_OK, this function must be
		called again with Z_FINISH and more output space (updated avail_out) but no
		more input data, until it returns with Z_STREAM_END or an error. After
		deflate has returned Z_STREAM_END, the only possible operations on the
		stream are deflateReset or deflateEnd.

		 Z_FINISH can be used immediately after deflateInit if all the compression
		is to be done in a single step. In this case, avail_out must be at least
		the value returned by deflateBound (see below). If deflate does not return
		Z_STREAM_END, then it must be called again as described above.

		deflate() sets strm->adler to the adler32 checksum of all input read
		so far (that is, total_in bytes).

		deflate() may update data_type if it can make a good guess about
		the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
		binary. This field is only for information purposes and does not affect
		the compression algorithm in any manner.

		deflate() returns Z_OK if some progress has been made (more input
		processed or more output produced), Z_STREAM_END if all input has been
		consumed and all output has been produced (only when flush is set to
		Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
		if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible
		(for example avail_in or avail_out was zero). Note that Z_BUF_ERROR is not
		fatal, and deflate() can be called again with more input and more output
		space to continue compressing.


		int __cdecl deflate(z_stream *strm, int flush);
	"

	<cdecl: sdword deflate ZLIBStream* sdword>
	^ self invalidCall.!

expectedVersion
	"answer the String version of the zlib.h that this package expects to find"

	^ ZLIB_VERSION.!

inflateBackEndStream: aZLIBStream
	"invoke the inflateBackEnd() method from ZLIB1.DLL

	From zlib.h:
		All memory allocated by inflateBackInit() is freed.

		inflateBackEnd() returns Z_OK on success, or Z_STREAM_ERROR if the stream
		state was inconsistent.


		int __cdecl inflateBackEnd(z_stream *strm);
	"

	<cdecl: sdword inflateBackEnd  ZLIBStream*>
	^ self invalidCall.!

inflateBackInitStream: aZLIBStream windowBits: anInteger window: anExternalAddress
	"invoke the inflateBackInit() method from ZLIB1.DLL

	From zlib.h:
		Initialize the internal stream state for decompression using inflateBack()
		calls.  The fields zalloc, zfree and opaque in strm must be initialized
		before the call.  If zalloc and zfree are Z_NULL, then the default library-
		derived memory allocation routines are used.  windowBits is the base two
		logarithm of the window size, in the range 8..15.  window is a caller
		supplied buffer of that size.  Except for special applications where it is
		assured that deflate was used with small window sizes, windowBits must be 15
		and a 32K byte window must be supplied to be able to decompress general
		deflate streams.

		See inflateBack() for the usage of these routines.

		inflateBackInit will return Z_OK on success, Z_STREAM_ERROR if any of
		the paramaters are invalid, Z_MEM_ERROR if the internal state could not
		be allocated, or Z_VERSION_ERROR if the version of the library does not
		match the version of the header file.


		int __cdecl inflateBackInit(z_stream *strm, int windowBits, unsigned char *window);
	"

	"inflateBackInit() is actually implemented as a macro that expands to something like"
	^ self
		inflateBackInitStream: aZLIBStream
		windowBits: anInteger
		window: anExternalAddress
		version: self expectedVersion
		size: aZLIBStream byteSize.!

inflateBackInitStream: aZLIBStream windowBits: anInteger1 window: anExternalAddress version: aString size: aInteger2
	"private -- invoke the inflateBackInit_() method from ZLIB1.DLL

	This method is 'private' in zlib -- the published function, inflateBackInit(), is actually a macro
	that forwards to this method, passing the compile-time version and size parameters
	so that the run-time can do a bit of sanity checking.


		int __cdecl inflateBackInit_(
				z_stream *strm,
				int windowBits,
				unsigned char *window,
				char *version,
				int size);
	"
	<cdecl: sdword inflateBackInit_ ZLIBStream* sdword void* char* sdword>
	^ self invalidCall.
!

inflateBackStream: aZLIBStream in: anExternalCallback1 inDesc: anExternalAddress1 out: anExternalCallback2 outDesc: anExternalAddress2
	"invoke the inflateBack() method from ZLIB1.DLL

	From zlib.h:
		inflateBack() does a raw inflate with a single call using a call-back
		interface for input and output.  This is more efficient than inflate() for
		file i/o applications in that it avoids copying between the output and the
		sliding window by simply making the window itself the output buffer.  This
		function trusts the application to not change the output buffer passed by
		the output function, at least until inflateBack() returns.

		inflateBackInit() must be called first to allocate the internal state
		and to initialize the state with the user-provided window buffer.
		inflateBack() may then be used multiple times to inflate a complete, raw
		deflate stream with each call.  inflateBackEnd() is then called to free
		the allocated state.

		A raw deflate stream is one with no zlib or gzip header or trailer.
		This routine would normally be used in a utility that reads zip or gzip
		files and writes out uncompressed files.  The utility would decode the
		header and process the trailer on its own, hence this routine expects
		only the raw deflate stream to decompress.  This is different from the
		normal behavior of inflate(), which expects either a zlib or gzip header and
		trailer around the deflate stream.

		inflateBack() uses two subroutines supplied by the caller that are then
		called by inflateBack() for input and output.  inflateBack() calls those
		routines until it reads a complete deflate stream and writes out all of the
		uncompressed data, or until it encounters an error.  The function's
		parameters and return types are defined above in the in_func and out_func
		typedefs.  inflateBack() will call in(in_desc, &buf) which should return the
		number of bytes of provided input, and a pointer to that input in buf.  If
		there is no input available, in() must return zero--buf is ignored in that
		case--and inflateBack() will return a buffer error.  inflateBack() will call
		out(out_desc, buf, len) to write the uncompressed data buf[0..len-1].  out()
		should return zero on success, or non-zero on failure.  If out() returns
		non-zero, inflateBack() will return with an error.  Neither in() nor out()
		are permitted to change the contents of the window provided to
		inflateBackInit(), which is also the buffer that out() uses to write from.
		The length written by out() will be at most the window size.  Any non-zero
		amount of input may be provided by in().

		For convenience, inflateBack() can be provided input on the first call by
		setting strm->next_in and strm->avail_in.  If that input is exhausted, then
		in() will be called.  Therefore strm->next_in must be initialized before
		calling inflateBack().  If strm->next_in is Z_NULL, then in() will be called
		immediately for input.  If strm->next_in is not Z_NULL, then strm->avail_in
		must also be initialized, and then if strm->avail_in is not zero, input will
		initially be taken from strm->next_in[0 .. strm->avail_in - 1].

		The in_desc and out_desc parameters of inflateBack() is passed as the
		first parameter of in() and out() respectively when they are called.  These
		descriptors can be optionally used to pass any information that the caller-
		supplied in() and out() functions need to do their job.

		On return, inflateBack() will set strm->next_in and strm->avail_in to
		pass back any unused input that was provided by the last in() call.  The
		return values of inflateBack() can be Z_STREAM_END on success, Z_BUF_ERROR
		if in() or out() returned an error, Z_DATA_ERROR if there was a format
		error in the deflate stream (in which case strm->msg is set to indicate the
		nature of the error), or Z_STREAM_ERROR if the stream was not properly
		initialized.  In the case of Z_BUF_ERROR, an input or output error can be
		distinguished using strm->next_in which will be Z_NULL only if in() returned
		an error.  If strm->next is not Z_NULL, then the Z_BUF_ERROR was due to
		out() returning non-zero.  (in() will always be called before out(), so
		strm->next_in is assured to be defined if out() returns non-zero.)  Note
		that inflateBack() cannot return Z_OK.


		int __cdecl inflateBack(
				z_stream *strm,
				in_func in,
				void *in_desc,
				out_func out,
				void *out_desc);
		where:
			typedef unsigned (*in_func)(void*, unsigned char**);
			typedef int (*out_func)(void*, unsigned char*, unsigned);
	"

	<cdecl: sdword inflateBack ZLIBStream* ExternalCallback void* ExternalCallback void*>
	^ self invalidCall.!

inflateCopyDestination: aZLIBStream1 source: aZLIBStream2
	"invoke the inflateCopy() method from ZLIB1.DLL

	From zlib.h:
		Sets the destination stream as a complete copy of the source stream.

		This function can be useful when randomly accessing a large stream.  The
		first pass through the stream can periodically record the inflate state,
		allowing restarting inflate at those points when randomly accessing the
		stream.

		inflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
		enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
		(such as zalloc being NULL). msg is left unchanged in both source and
		destination.


		int __cdecl inflateCopy(z_stream *dest, z_stream *source);

	"

	<cdecl: sdword inflateCopy ZLIBStream* ZLIBStream*>
	^ self invalidCall.!

inflateEndStream: aZLIBStream
	"invoke the inflateEnd() method from ZLIB1.DLL

	From zlib.h:
		All dynamically allocated data structures for this stream are freed.
		This function discards any unprocessed input and does not flush any
		pending output.

		inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state
		was inconsistent. In the error case, msg may be set but then points to a
		static string (which must not be deallocated).


		int __cdecl inflateEnd(z_stream *strm);
	"

	<cdecl: sdword inflateEnd ZLIBStream*>
	^ self invalidCall.!

inflateInit2Stream: aZLIBStream windowBits: anInteger
	"invoke the inflateInit2() method from ZLIB1.DLL

	From zlib.h:
		This is another version of inflateInit with an extra parameter. The
		fields next_in, avail_in, zalloc, zfree and opaque must be initialized
		before by the caller.

		The windowBits parameter is the base two logarithm of the maximum window
		size (the size of the history buffer).  It should be in the range 8..15 for
		this version of the library. The default value is 15 if inflateInit is used
		instead. windowBits must be greater than or equal to the windowBits value
		provided to deflateInit2() while compressing, or it must be equal to 15 if
		deflateInit2() was not used. If a compressed stream with a larger window
		size is given as input, inflate() will return with the error code
		Z_DATA_ERROR instead of trying to allocate a larger window.

		windowBits can also be -8..-15 for raw inflate. In this case, -windowBits
		determines the window size. inflate() will then process raw deflate data,
		not looking for a zlib or gzip header, not generating a check value, and not
		looking for any check values for comparison at the end of the stream. This
		is for use with other formats that use the deflate compressed data format
		such as zip.  Those formats provide their own check values. If a custom
		format is developed using the raw deflate format for compressed data, it is
		recommended that a check value such as an adler32 or a crc32 be applied to
		the uncompressed data as is done in the zlib, gzip, and zip formats.  For
		most applications, the zlib format should be used as is. Note that comments
		above on the use in deflateInit2() applies to the magnitude of windowBits.

		windowBits can also be greater than 15 for optional gzip decoding. Add
		32 to windowBits to enable zlib and gzip decoding with automatic header
		detection, or add 16 to decode only the gzip format (the zlib format will
		return a Z_DATA_ERROR).

		inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
		memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
		memLevel). msg is set to null if there is no error message.  inflateInit2
		does not perform any decompression apart from reading the zlib header if
		present: this will be done by inflate(). (So next_in and avail_in may be
		modified, but next_out and avail_out are unchanged.)


		int __cdecl inflateInit2(z_stream *strm, int windowBits);
	"

	"inflateInit2() is actually implemented as a macro that expands to something like"
	^ self
		inflateInit2Stream: aZLIBStream
		windowBits: anInteger
		version: self expectedVersion
		size: aZLIBStream byteSize.!

inflateInit2Stream: aZLIBStream windowBits: anInteger version: aString size: aInteger2
	"private -- invoke the inflateInit2_() method from ZLIB1.DLL

	This method is 'private' in zlib -- the published function, inflateInit2(), is actually a macro
	that forwards to this method, passing the compile-time version and size parameters
	so that the run-time can do a bit of sanity checking.


		int __cdecl inflateInit2_(z_stream *strm, int  windowBits, char *version, int size);
	"

	<cdecl: sdword inflateInit2_ ZLIBStream* sdword char* sdword>
	^ self invalidCall.
!

inflateInitStream: aZLIBStream
	"invoke the inflateInit() method from ZLIB1.DLL

	From zlib.h:
		Initializes the internal stream state for decompression. The fields
		next_in, avail_in, zalloc, zfree and opaque must be initialized before by
		the caller. If next_in is not Z_NULL and avail_in is large enough (the exact
		value depends on the compression method), inflateInit determines the
		compression method from the zlib header and allocates all data structures
		accordingly; otherwise the allocation will be deferred to the first call of
		inflate.  If zalloc and zfree are set to Z_NULL, inflateInit updates them to
		use default allocation functions.

		inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough
		memory, Z_VERSION_ERROR if the zlib library version is incompatible with the
		version assumed by the caller.  msg is set to null if there is no error
		message. inflateInit does not perform any decompression apart from reading
		the zlib header if present: this will be done by inflate().  (So next_in and
		avail_in may be modified, but next_out and avail_out are unchanged.)


		int __cdecl inflateInit(z_stream *strm);
	"

	"deflateInit() is actually implemented as a macro that expands to something like"
	^ self
		inflateInitStream: aZLIBStream
		version: self expectedVersion
		size: aZLIBStream byteSize.!

inflateInitStream: aZLIBStream version: aString size: aInteger
	"private -- invoke the inflateInit_() method from ZLIB1.DLL

	This method is 'private' in zlib -- the published function, inflateInit(), is actually a macro
	that forwards to this method, passing the compile-time version and size parameters
	so that the run-time can do a bit of sanity checking.


		int __cdecl inflateInit_(z_stream *trm, char *version, int size);
	"

	<cdecl: sdword inflateInit_ ZLIBStream* char* sdword>
	^ self invalidCall.!

inflateResetStream: aZLIBStream
	"invoke the inflateReset() method from ZLIB1.DLL

	From zlib.h:
		This function is equivalent to inflateEnd followed by inflateInit,
		but does not free and reallocate all the internal decompression state.
		The stream will keep attributes that may have been set by inflateInit2.

		inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
		stream state was inconsistent (such as zalloc or state being NULL).


		int __cdecl inflateReset(z_stream *strm);
	"

	<cdecl: sdword inflateReset ZLIBStream*>
	^ self invalidCall.!

inflateSetDictionary: aZLIBStream dictionary: anExternalAddress dictLength: anInteger
	"invoke the inflateSetDictionary() method from ZLIB1.DLL

	From zlib.h:
		Initializes the decompression dictionary from the given uncompressed byte
		sequence. This function must be called immediately after a call of inflate
		if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
		can be determined from the adler32 value returned by this call of
		inflate. The compressor and decompressor must use exactly the same
		dictionary (see deflateSetDictionary).

		inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
		parameter is invalid (such as NULL dictionary) or the stream state is
		inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
		expected one (incorrect adler32 value). inflateSetDictionary does not
		perform any decompression: this will be done by subsequent calls of
		inflate().


		int __cdecl inflateSetDictionary OF(
					z_stream *strm,
					const Bytef *dictionary,
					uInt  dictLength);
	"

	<cdecl: sdword inflateSetDictionary ZLIBStream* void* dword>
	^ self invalidCall.!

inflateStream: aZLIBStream flush: anInteger
	"invoke the inflate() method from ZLIB1.DLL

	From zlib.h:
		inflate decompresses as much data as possible, and stops when the input
		buffer becomes empty or the output buffer becomes full. It may introduce
		some output latency (reading input without producing any output) except when
		forced to flush.

		The detailed semantics are as follows. inflate performs one or both of the
		following actions:

		- Decompress more input starting at next_in and update next_in and avail_in
		  accordingly. If not all input can be processed (because there is not
		  enough room in the output buffer), next_in is updated and processing
		  will resume at this point for the next call of inflate().

		- Provide more output starting at next_out and update next_out and avail_out
		  accordingly.  inflate() provides as much output as possible, until there
		  is no more input data or no more space in the output buffer (see below
		  about the flush parameter).

		Before the call of inflate(), the application should ensure that at least
		one of the actions is possible, by providing more input and/or consuming
		more output, and updating the next_* and avail_* values accordingly.
		The application can consume the uncompressed output when it wants, for
		example when the output buffer is full (avail_out == 0), or after each
		call of inflate(). If inflate returns Z_OK and with zero avail_out, it
		must be called again after making room in the output buffer because there
		might be more output pending.

		The flush parameter of inflate() can be Z_NO_FLUSH, Z_SYNC_FLUSH,
		Z_FINISH, or Z_BLOCK. Z_SYNC_FLUSH requests that inflate() flush as much
		output as possible to the output buffer. Z_BLOCK requests that inflate() stop
		if and when it get to the next deflate block boundary. When decoding the zlib
		or gzip format, this will cause inflate() to return immediately after the
		header and before the first block. When doing a raw inflate, inflate() will
		go ahead and process the first block, and will return when it gets to the end
		of that block, or when it runs out of data.

		The Z_BLOCK option assists in appending to or combining deflate streams.
		Also to assist in this, on return inflate() will set strm->data_type to the
		number of unused bits in the last byte taken from strm->next_in, plus 64
		if inflate() is currently decoding the last block in the deflate stream,
		plus 128 if inflate() returned immediately after decoding an end-of-block
		code or decoding the complete header up to just before the first byte of the
		deflate stream. The end-of-block will not be indicated until all of the
		uncompressed data from that block has been written to strm->next_out.  The
		number of unused bits may in general be greater than seven, except when
		bit 7 of data_type is set, in which case the number of unused bits will be
		less than eight.

		inflate() should normally be called until it returns Z_STREAM_END or an
		error. However if all decompression is to be performed in a single step
		(a single call of inflate), the parameter flush should be set to
		Z_FINISH. In this case all pending input is processed and all pending
		output is flushed; avail_out must be large enough to hold all the
		uncompressed data. (The size of the uncompressed data may have been saved
		by the compressor for this purpose.) The next operation on this stream must
		be inflateEnd to deallocate the decompression state. The use of Z_FINISH
		is never required, but can be used to inform inflate that a faster approach
		may be used for the single inflate() call.

		In this implementation, inflate() always flushes as much output as
		possible to the output buffer, and always uses the faster approach on the
		first call. So the only effect of the flush parameter in this implementation
		is on the return value of inflate(), as noted below, or when it returns early
		because Z_BLOCK is used.

		If a preset dictionary is needed after this call (see inflateSetDictionary
		below), inflate sets strm-adler to the adler32 checksum of the dictionary
		chosen by the compressor and returns Z_NEED_DICT; otherwise it sets
		strm->adler to the adler32 checksum of all output produced so far (that is,
		total_out bytes) and returns Z_OK, Z_STREAM_END or an error code as described
		below. At the end of the stream, inflate() checks that its computed adler32
		checksum is equal to that saved by the compressor and returns Z_STREAM_END
		only if the checksum is correct.

		inflate() will decompress and check either zlib-wrapped or gzip-wrapped
		deflate data.  The header type is detected automatically.  Any information
		contained in the gzip header is not retained, so applications that need that
		information should instead use raw inflate, see inflateInit2() below, or
		inflateBack() and perform their own processing of the gzip header and
		trailer.

		inflate() returns Z_OK if some progress has been made (more input processed
		or more output produced), Z_STREAM_END if the end of the compressed data has
		been reached and all uncompressed output has been produced, Z_NEED_DICT if a
		preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
		corrupted (input stream not conforming to the zlib format or incorrect check
		value), Z_STREAM_ERROR if the stream structure was inconsistent (for example
		if next_in or next_out was NULL), Z_MEM_ERROR if there was not enough memory,
		Z_BUF_ERROR if no progress is possible or if there was not enough room in the
		output buffer when Z_FINISH is used. Note that Z_BUF_ERROR is not fatal, and
		inflate() can be called again with more input and more output space to
		continue decompressing. If Z_DATA_ERROR is returned, the application may then
		call inflateSync() to look for a good compression block if a partial recovery
		of the data is desired.


		int __cdecl inflate(z_stream *strm, int flush);
	"

	<cdecl: sdword inflate ZLIBStream* sdword>
	^ self invalidCall.!

inflateSyncStream: aZLIBStream
	"invoke the inflateSync() method from ZLIB1.DLL

	From zlib.h:
		Skips invalid compressed data until a full flush point (see above the
		description of deflate with Z_FULL_FLUSH) can be found, or until all
		available input is skipped. No output is provided.

		inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
		if no more input was provided, Z_DATA_ERROR if no flush point has been found,
		or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
		case, the application may save the current current value of total_in which
		indicates where valid compressed data was found. In the error case, the
		application may repeatedly call inflateSync, providing more input each time,
		until success or end of the input data.


		int __cdecl inflateSync(z_stream *strm);
	"

	<cdecl: sdword inflateSync ZLIBStream*>
	^ self invalidCall.!

initialAdler32
	"answer the seed value to use for accumulating adler checksums"

	^ self adler32: 0 buffer: nil size: 0.!

initialCrc32
	"answer the seed value to use for accumulating crc checksums"

	^ self crc32: 0 buffer: nil size: 0.!

uncompressDestination: anExternalAddress1 destinationSizePointer: anExternalAddress2 souce: anExternalAddress3 sourceSize: anInteger
	"invoke the uncompress() method from ZLIB1.DLL

	From zlib.h:
		Decompresses the source buffer into the destination buffer.  sourceLen is
		the byte length of the source buffer. Upon entry, destLen is the total
		size of the destination buffer, which must be large enough to hold the
		entire uncompressed data. (The size of the uncompressed data must have
		been saved previously by the compressor and transmitted to the decompressor
		by some mechanism outside the scope of this compression library.)
		Upon exit, destLen is the actual size of the compressed buffer.
		This function can be used to decompress a whole file at once if the
		input file is mmap'ed.

		uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
		enough memory, Z_BUF_ERROR if there was not enough room in the output
		buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete.


		int __cdecl uncompress(char *dest,  uLong *destLen, char *source, uLong sourceLen);
	"

	<cdecl: sdword uncompress void* dword* void* dword>
	^ self invalidCall.
!

zlibCompileFlags
	"invoke the zlibCompileFlags() method from ZLIB1.DLL

	zlib.h:
		Return flags indicating compile-time options.

		Type sizes, two bits each, 00 = 16 bits, 01 = 32, 10 = 64, 11 = other:
		 1.0: size of uInt
		 3.2: size of uLong
		 5.4: size of voidpf (pointer)
		 7.6: size of z_off_t

		Compiler, assembler, and debug options:
		 8: DEBUG
		 9: ASMV or ASMINF -- use ASM code
		 10: ZLIB_WINAPI -- exported functions use the WINAPI calling convention
		 11: 0 (reserved)

		One-time table building (smaller code, but not thread-safe if true):
		 12: BUILDFIXED -- build static block decoding tables when needed
		 13: DYNAMIC_CRC_TABLE -- build CRC calculation tables when needed
		 14,15: 0 (reserved)

		Library content (indicates missing functionality):
		 16: NO_GZCOMPRESS -- gz* functions cannot compress (to avoid linking
						deflate code when not needed)
		 17: NO_GZIP -- deflate can't write gzip streams, and inflate can't detect
						and decode gzip streams (to avoid linking crc code)
		 18-19: 0 (reserved)

		Operation variations (changes in library functionality):
		 20: PKZIP_BUG_WORKAROUND -- slightly more permissive inflate
		 21: FASTEST -- deflate algorithm with only one, lowest compression level
		 22,23: 0 (reserved)

		The sprintf variant used by gzprintf (zero is best):
		 24: 0 = vs*, 1 = s* -- 1 means limited to 20 arguments after the format
		 25: 0 = *nprintf, 1 = *printf -- 1 means gzprintf() not secure!!
		 26: 0 = returns value, 1 = void -- 1 means inferred string length returned

		Remainder:
		27-31: 0 (reserved)


		uLong __cdecl zlibCompileFlags();
	"

	<cdecl: dword zlibCompileFlags>
	^ self invalidCall.!

zlibVersion
	"answer the 'x.x.x' version string from ZLIB1.DLL

		self default zlibVersion.

	From zlib.h:
		The application can compare zlibVersion and ZLIB_VERSION for consistency.
		If the first character differs, the library code actually used is
		not compatible with the zlib.h header file used by the application.
		This check is automatically made by deflateInit and inflateInit.

		const char * __cdecl zlibVersion();
	"

	<cdecl: char* zlibVersion>
	^ self invalidCall.! !
!ZLib1Library categoriesFor: #adler32:!checksum computation!public! !
!ZLib1Library categoriesFor: #adler32:buffer:size:!checksum computation!public!zlib-checksum functions! !
!ZLib1Library categoriesFor: #assertVersionCompatibility!checking!public!versions! !
!ZLib1Library categoriesFor: #checkVersionCompatibility!checking!public!versions! !
!ZLib1Library categoriesFor: #compileTimeSettings!public!versions! !
!ZLib1Library categoriesFor: #compressBound:!compressing!public!zlib-utility functions! !
!ZLib1Library categoriesFor: #compressDestination:destinationSizePointer:souce:sourceSize:!compressing!public!zlib-utility functions! !
!ZLib1Library categoriesFor: #compressDestination:destinationSizePointer:souce:sourceSize:level:!compressing!public!zlib-utility functions! !
!ZLib1Library categoriesFor: #crc32:!checksum computation!public! !
!ZLib1Library categoriesFor: #crc32:buffer:size:!checksum computation!public!zlib-checksum functions! !
!ZLib1Library categoriesFor: #deflateBoundStream:sourceLen:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateCopyDestination:source:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateEndStream:!compressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #deflateInit2Stream:level:method:windowBits:memLevel:strategy:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateInit2Stream:level:method:windowBits:memLevel:strategy:version:size:!compressing!private!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateInitStream:level:!compressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #deflateInitStream:level:version:size:!compressing!private!zlib-basic functions! !
!ZLib1Library categoriesFor: #deflateParamsStream:level:strategy:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflatePrimeStream:bits:value:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateResetStream:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateSetDictionary:dictionary:dictLength:!compressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #deflateStream:flush:!compressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #expectedVersion!constants!public!versions! !
!ZLib1Library categoriesFor: #inflateBackEndStream:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateBackInitStream:windowBits:window:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateBackInitStream:windowBits:window:version:size:!decompressing!private!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateBackStream:in:inDesc:out:outDesc:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateCopyDestination:source:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateEndStream:!decompressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #inflateInit2Stream:windowBits:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateInit2Stream:windowBits:version:size:!decompressing!private!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateInitStream:!decompressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #inflateInitStream:version:size:!decompressing!private!zlib-basic functions! !
!ZLib1Library categoriesFor: #inflateResetStream:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateSetDictionary:dictionary:dictLength:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #inflateStream:flush:!decompressing!public!zlib-basic functions! !
!ZLib1Library categoriesFor: #inflateSyncStream:!decompressing!public!zlib-advanced functions! !
!ZLib1Library categoriesFor: #initialAdler32!checksum computation!public! !
!ZLib1Library categoriesFor: #initialCrc32!checksum computation!public! !
!ZLib1Library categoriesFor: #uncompressDestination:destinationSizePointer:souce:sourceSize:!decompressing!public!zlib-utility functions! !
!ZLib1Library categoriesFor: #zlibCompileFlags!public!versions!zlib-advanced functions! !
!ZLib1Library categoriesFor: #zlibVersion!public!versions!zlib-basic functions! !

!ZLib1Library class methodsFor!

fileName
	"answer the host system file name for the library"

	"note that the *official* zlib library is known as zlib1"
	^ 'ZLib1'.!

rebuildPoolConstants
	"private -- rebuild the ZLib pool constants dictionary.

		self rebuildPoolConstants.
	"

	"these constants are copied directly (including most of the comments) from zlib.h"

	(Smalltalk at: #ZLib1Constants ifAbsentPut: [PoolConstantsDictionary new])

		"this is the version I used to build this package -- v1.2.1"
		at: 'ZLIB_VERSION'			put: '1.2.1';
		at: 'ZLIB_VERNUM'			put: 16r1210;

		"allowed flush values; see deflate() and inflate()"
		at: 'Z_NO_FLUSH' 			put: 0;
		at: 'Z_PARTIAL_FLUSH' 		put: 1;		"will be removed, use Z_SYNC_FLUSH instead"
		at: 'Z_SYNC_FLUSH' 		put: 2;
		at: 'Z_FULL_FLUSH' 		put: 3;
		at: 'Z_FINISH' 			put: 4;
		at: 'Z_BLOCK' 			put: 5;

		"return codes for the compression/decompression functions. Negative
		values are errors, positive values are used for special but normal events"
		at: 'Z_OK' 				put: 0;
		at: 'Z_STREAM_END' 		put: 1;
		at: 'Z_NEED_DICT' 			put: 2;
		at: 'Z_ERRNO' 			put: -1;
		at: 'Z_STREAM_ERROR' 		put: -2;
		at: 'Z_DATA_ERROR' 		put: -3;
		at: 'Z_MEM_ERROR' 		put: -4;
		at: 'Z_BUF_ERROR' 		put: -5;
		at: 'Z_VERSION_ERROR' 		put: -6;

		"compression levels"
		at: 'Z_NO_COMPRESSION' 	put: 0;
		at: 'Z_BEST_SPEED' 		put: 1;
		at: 'Z_BEST_COMPRESSION' 	put: 9;
		at: 'Z_DEFAULT_COMPRESSION' put: -1;

		"compression strategy; see deflateInit2() for details"
		at: 'Z_FILTERED'			put: 1;
		at: 'Z_HUFFMAN_ONLY'		put: 2;
		at: 'Z_RLE' 				put: 3;
		at: 'Z_DEFAULT_STRATEGY'	put: 0;

		"possible values of the data_type field (though see inflate())"
		at: 'Z_BINARY'			put: 0;
		at: 'Z_ASCII'				put: 1;
		at: 'Z_UNKNOWN'			put: 2;

		"the deflate compression method (the only one supported in this version)"
		at: 'Z_DEFLATED'			put: 8;

		"for initializing zalloc, zfree, opaque"
		at: 'Z_NULL'				put: 0;

		shrink.
! !
!ZLib1Library class categoriesFor: #fileName!constants!public! !
!ZLib1Library class categoriesFor: #rebuildPoolConstants!development!private! !

ZLIBStream guid: (GUID fromString: '{C99A42AD-A2E4-455C-8908-33F90D98515D}')!
ZLIBStream comment: 'Copyright © Chris Uppal, 2003, 2004.
chris.uppal@metagnostic.org

This is a wrapper for the "z_stream_s" structure from the *official* zlib library -- zlib1.dll -- distributed at http://www.zlib.org/

ZLib itself is:
Copyright © 1995-2003 Jean-loup Gailly and Mark Adler

The following is reproduced (with some C-specific stuff elided) from zlib,.h:

==============================================================

   The application must update next_in and avail_in when avail_in has
   dropped to zero. It must update next_out and avail_out when avail_out
   has dropped to zero. The application must initialize zalloc, zfree and
   opaque before calling the init function. All other fields are set by the
   compression library and must not be updated by the application.

   The opaque value provided by the application will be passed as the first
   parameter for calls of zalloc and zfree. This can be useful for custom
   memory management. The compression library attaches no meaning to the
   opaque value.

   zalloc must return Z_NULL if there is not enough memory for the object.
   If zlib is used in a multi-threaded application, zalloc and zfree must be
   thread safe.

   On 16-bit systems, the functions zalloc and zfree must be able to allocate
   exactly 65536 bytes, but will not be required to allocate more than this
   if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
   pointers returned by zalloc for objects of exactly 65536 bytes *must*
   have their offset normalized to zero. The default allocation function
   provided by this library ensures this (see zutil.c). To reduce memory
   requirements and avoid any allocation of 64K objects, at the expense of
   compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

   The fields total_in and total_out can be used for statistics or
   progress reports. After compression, total_in holds the total size of
   the uncompressed data and may be saved for use in the decompressor
   (particularly if the decompressor wants to decompress everything in
   a single step).
'!
!ZLIBStream categoriesForClass!Unclassified! !
!ZLIBStream methodsFor!

adler
	"Answer the receiver's adler field as a Smalltalk object."

	^(bytes dwordAtOffset: 48)!

avail_in
	"Answer the receiver's avail_in field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

avail_in: anObject
	"Set the receiver's avail_in field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

avail_out
	"Answer the receiver's avail_out field as a Smalltalk object."

	^(bytes sdwordAtOffset: 16)!

avail_out: anObject
	"Set the receiver's avail_out field to the value of anObject."

	bytes sdwordAtOffset: 16 put: anObject!

data_type
	"Answer the receiver's data_type field as a Smalltalk object."

	^(bytes sdwordAtOffset: 44)!

data_type: anObject
	"Set the receiver's data_type field to the value of anObject."

	bytes sdwordAtOffset: 44 put: anObject!

msg
	"Answer the receiver's msg field as a String"

	^ String fromAddress: (bytes dwordAtOffset: 24) asExternalAddress.!

msg: anObject
	"Set the receiver's msg field to the value of anObject."

	bytes dwordAtOffset: 24 put: anObject yourAddress.!

next_in
	"Answer the receiver's next_in field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

next_in: anObject
	"Set the receiver's next_in field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

next_out
	"Answer the receiver's next_out field as a Smalltalk object."

	^(bytes dwordAtOffset: 12) asExternalAddress!

next_out: anObject
	"Set the receiver's next_out field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

opaque
	"Answer the receiver's opaque field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

opaque: anObject
	"Set the receiver's opaque field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject!

total_in
	"Answer the receiver's total_in field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

total_out
	"Answer the receiver's total_out field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

zalloc
	"Answer the receiver's zalloc field as a Smalltalk object."

	^(bytes dwordAtOffset: 32) asExternalAddress!

zalloc: anObject
	"Set the receiver's zalloc field to the value of anObject."

	bytes dwordAtOffset: 32 put: anObject!

zfree
	"Answer the receiver's zfree field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

zfree: anObject
	"Set the receiver's zfree field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject! !
!ZLIBStream categoriesFor: #adler!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #avail_in!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #avail_in:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #avail_out!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #avail_out:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #data_type!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #data_type:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #msg!accessing!public! !
!ZLIBStream categoriesFor: #msg:!accessing!public! !
!ZLIBStream categoriesFor: #next_in!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #next_in:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #next_out!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #next_out:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #opaque!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #opaque:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #total_in!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #total_out!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #zalloc!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #zalloc:!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #zfree!**compiled accessors**!public! !
!ZLIBStream categoriesFor: #zfree:!**compiled accessors**!public! !

!ZLIBStream class methodsFor!

defineFields
	"
	self compileDefinition.

		typedef struct z_stream_s {
			Bytef		*next_in;		/* next input byte */
			uInt		avail_in;		/* number of bytes available at next_in */
			uLong		total_in;		/* total nb of input bytes read so far */

			Bytef		*next_out;		/* next output byte should be put there */
			uInt		avail_out;		/* remaining free space at next_out */
			uLong		total_out;		/* total nb of bytes output so far */

			char		*msg; 			/* last error message, NULL if no error */
			struct internal_state FAR
					*state;			/* not visible by applications */
			
			alloc_func	zalloc;			/* used to allocate the internal state */
			free_func	zfree;			/* used to free the internal state */
			voidpf		opaque;		/* private data object passed to zalloc and zfree */
			
			int		data_type;		/* best guess about the data type: ascii or binary */
			uLong		adler;			/* adler32 value of the uncompressed data */
			uLong		reserved;		/* reserved for future use */
		} z_stream;
	"

	self

		defineField: #next_in type: LPVOIDField new;
		defineField: #avail_in type: SDWORDField new;
		defineField: #total_in type: DWORDField readOnly;

		defineField: #next_out type: LPVOIDField new;
		defineField: #avail_out type: SDWORDField new;
		defineField: #total_out type: DWORDField readOnly;

		defineField: #msg type: LPVOIDField new;
		defineField: #state type: LPVOIDField filler;

		defineField: #zalloc type: LPVOIDField new;
		defineField: #zfree type: LPVOIDField new;
		defineField: #opaque type: LPVOIDField new;

		defineField: #data_type type: SDWORDField new;
		defineField: #adler type: DWORDField readOnly;
		defineField: #reserved type: DWORDField filler;

		yourself.
! !
!ZLIBStream class categoriesFor: #defineFields!constants!public! !

"Binary Globals"!

"Resources"!

