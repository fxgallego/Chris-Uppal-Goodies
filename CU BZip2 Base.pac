| package |
package := Package name: 'CU BZip2 Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A "raw" wrapper for the bzip2 library by Julian Seward, described at, and distrubuted from, http://sources.redhat.com/bzip2/.

BZip2 is a an implementation of a "block sorting" compression scheme.  Here is Julian''s description of it:

	bzip2 is a freely available, patent free (see below), high-quality data compressor.
	It typically compresses files to within 10% to 15% of the best available techniques
	(the PPM family of statistical compressors), whilst being around twice as fast at
	compression and six times faster at decompression.

Compared to the compression in ZLib (for which I make wrappers available too) it is often more effective, especialy on long texts, but somewhat slower and more memory-intensive.

The library is distributed in the form of the LIBBZ2x.x.x.DLL Note that the DLL is distributed with a full version as part of its name.  This package expects you to rename it to
	''libbz2-1.dll''

The version that I have used to build this is actually 1.0.0, but the current version is 1.0.2.  There are apparently no changes to the libary (the changes are in the bzip2 [de]compression utilities that Julian also distributes.

This is a very low-level wrapper, see the package ''CU BZip2 Streams'' for something that''s actually intended to be usefull.

This wrapper only covers the bzip (de)compression stuff.  The library itself has 17 functions for handling bzip2 files directly -- I have ignored all of that.

The BZip2 library itself is covered by its own copyright notice, the following only applies to my stuff.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.01'.


package classNames
	add: #BZip2Error;
	add: #BZip2Library;
	add: #BZIP2Stream;
	yourself.

package globalNames
	add: #BZip2Constants;
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

Error subclass: #BZip2Error
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #BZip2Library
	instanceVariableNames: 'verbosity'
	classVariableNames: ''
	poolDictionaries: 'BZip2Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #BZIP2Stream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #BZip2Constants put: (PoolConstantsDictionary named: #BZip2Constants)!
BZip2Constants at: 'BZ_CONFIG_ERROR' put: -16r9!
BZip2Constants at: 'BZ_DATA_ERROR' put: -16r4!
BZip2Constants at: 'BZ_DATA_ERROR_MAGIC' put: -16r5!
BZip2Constants at: 'BZ_DEFAULT_WORK_FACTOR' put: 16r0!
BZip2Constants at: 'BZ_FINISH' put: 16r2!
BZip2Constants at: 'BZ_FINISH_OK' put: 16r3!
BZip2Constants at: 'BZ_FLUSH' put: 16r1!
BZip2Constants at: 'BZ_FLUSH_OK' put: 16r2!
BZip2Constants at: 'BZ_IO_ERROR' put: -16r6!
BZip2Constants at: 'BZ_MAX_BLOCK_SIZE' put: 16r9!
BZip2Constants at: 'BZ_MAX_VERBOSITY' put: 16r4!
BZip2Constants at: 'BZ_MAX_WORK_FACTOR' put: 16rFA!
BZip2Constants at: 'BZ_MEM_ERROR' put: -16r3!
BZip2Constants at: 'BZ_MIN_BLOCK_SIZE' put: 16r1!
BZip2Constants at: 'BZ_MIN_VERBOSITY' put: 16r0!
BZip2Constants at: 'BZ_MIN_WORK_FACTOR' put: 16r1!
BZip2Constants at: 'BZ_OK' put: 16r0!
BZip2Constants at: 'BZ_OUTBUFF_FULL' put: -16r8!
BZip2Constants at: 'BZ_PARAM_ERROR' put: -16r2!
BZip2Constants at: 'BZ_RUN' put: 16r0!
BZip2Constants at: 'BZ_RUN_OK' put: 16r1!
BZip2Constants at: 'BZ_SEQUENCE_ERROR' put: -16r1!
BZip2Constants at: 'BZ_STREAM_END' put: 16r4!
BZip2Constants at: 'BZ_UNEXPECTED_EOF' put: -16r7!
BZip2Constants at: 'BZIP2_VERSION' put: '1.0.2'!
BZip2Constants shrink!

"Classes"!

BZip2Error guid: (GUID fromString: '{E370E5EB-4E71-4612-94AB-5E847CD866A9}')!
BZip2Error comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org'!
!BZip2Error categoriesForClass!Unclassified! !
!BZip2Error methodsFor!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^ 'BZip2 Error: %1 (%2)'.!

errorCode
	"answer the error code that caused this exception"

	^ self tag.! !
!BZip2Error categoriesFor: #_descriptionFormat!displaying!public! !
!BZip2Error categoriesFor: #errorCode!accessing!public! !

BZip2Library guid: (GUID fromString: '{ACA77D06-B832-4D8D-9DC0-F88EEC246E22}')!
BZip2Library comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

This is a wrapper for the bzip2 library by Julian Seward, described at, and distrubuted from, http://sources.redhat.com/bzip2/.

That library is distributed in the form of the LIBBZ2x.x.x.DLL (with a full version as part of its name), this class expects you to rename it to ''libbz2-1.dll''

The version that I have used to build this is actually 1.0.0, thoughthe current version of the code is 1.0.2.  There are apparently no changes to the libary between those two versions (the changes are in the bzip2 [de]compression utilities that Julian also distributes.

This wrapper only covers the bzip (de)compression stuff.  The library itself has 17 functions for handling bzip2 files directly -- I have ignored all of that.
'!
!BZip2Library categoriesForClass!Unclassified! !
!BZip2Library methodsFor!

BZ2_bzCompress: aZLIBStream action: anInteger
	"private -- invoke the BZ2_bzCompress() method from the bzip2 dll.

		int __stdcall BZ2_bzCompress(bz_stream *strm, int action);
	"

	<stdcall: sdword BZ2_bzCompress BZIP2Stream* sdword>
	^ self invalidCall.!

BZ2_bzCompressEnd: aZLIBStream
	"private -- invoke the BZ2_bzCompressEnd() method from the bzip2 dll.

		int __stdcall BZ2_bzCompressEnd(bz_stream *strm);
	"

	<stdcall: sdword BZ2_bzCompressEnd BZIP2Stream*>
	^ self invalidCall.!

BZ2_bzCompressInit: aZLIBStream blockSize: anInteger1 verbosity: anInteger2 workFactor: anInteger3
	"private -- invoke the BZ2_bzCompressInit() method from the bzip2 dll.

		int __stdcall BZ2_bzCompressInit(
					bz_stream *strm,
					int blockSize100k,
					int verbosity,
					int workFactor);
	"

	<stdcall: sdword BZ2_bzCompressInit BZIP2Stream* sdword sdword sdword>
	^ self invalidCall.!

BZ2_bzDecompress: aZLIBStream
	"private -- invoke the BZ2_bzDecompress() method from the bzip2 dll.

		int __stdcall BZ2_bzDecompress(bz_stream *strm);
	"

	<stdcall: sdword BZ2_bzDecompress BZIP2Stream*>
	^ self invalidCall.!

BZ2_bzDecompressEnd: aZLIBStream
	"private -- invoke the BZ2_bzDecompressEnd() method from the bzip2 dll.

		int __stdcall BZ2_bzDecompressEnd(bz_stream *strm);
	"

	<stdcall: sdword BZ2_bzDecompressEnd BZIP2Stream*>
	^ self invalidCall.!

BZ2_bzDecompressInit: aZLIBStream verbosity: anInteger small: aBoolean
	"private -- invoke the BZ2_bzDecompressInit() method from the bzip2 dll.

		int __stdcall BZ2_bzDecompressInit(
					bz_stream *strm,
					int verbosity,
					int small);		/* treated as a bool */
	"

	<stdcall: sdword BZ2_bzDecompressInit BZIP2Stream* sdword bool>
	^ self invalidCall.!

BZ2_bzlibVersion
	"private -- answer the 'x.x.x, <date>' version string from our DLL
	Note, this function is not yet a part of the 'offical' bzip2 library, and the format
	it returns does not match its documentation.

		self default BZ2_bzlibVersion.

		const char * __stdcall BZ2_bzlibVersion();
	"

	<stdcall: char* BZ2_bzlibVersion>
	^ self invalidCall.!

check: anInteger
	"private -- check the given return code and throw an error if it is
	less than zero, or answer it if not"

	anInteger < 0 ifTrue: [self signalBZip2Error: anInteger].

	^ anInteger.!

compressEnd: aZLIBStream
	"release all resources that were assigned to the given 'stream' for compression.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self check: (self BZ2_bzCompressEnd: aZLIBStream).
!

compressFinish: aZLIBStream
	"attempt to flush the remaining data and terminate the ongaing compression.
	Answers BZ_FINISH_OK (3), BZ_STREAM_END (4), or thows a BZip2Error"

	^ self check: (self BZ2_bzCompress: aZLIBStream action: BZ_FINISH).
!

compressFlush: aZLIBStream
	"attempt to flush the remaining data and finish the current compression bllock
	(see the real documentation for an explanation that makes better sense ;-).
	Answers BZ_FLUSH_OK (2), BZ_RUN_OK (1), or thows a BZip2Error"

	^ self check: (self BZ2_bzCompress: aZLIBStream action: BZ_FLUSH).
!

compressInit: aZLIBStream blockSize: anInteger
	"prepare the given 'stream' for compression.
	The block size determines how much memory (in 100K) is used and should be in the range
	BZ_MIN_BLOCK_SIZE (1) to BZ_MAX_BLOCK_SIZE (9) inclusive.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self compressInit: aZLIBStream blockSize: anInteger workFactor: BZ_DEFAULT_WORK_FACTOR.
!

compressInit: aZLIBStream blockSize: anInteger1 verbosity: anInteger2 workFactor: anInteger3
	"prepare the given 'stream' for compression.
	The block size determines how much memory (in 100K) is used and should be in the range
	BZ_MIN_BLOCK_SIZE (1) to BZ_MAX_BLOCK_SIZE (9) inclusive.
	The verbosity is a measure of how much chatter to write to the stdout stream, and should be in the
	range BZ_MIN_VERBOSITY (=0, silence) to BZ_MAX_VERBOSITY (=4) inclusive.
	The work factor is a measure of how readily the library will fall back from a sort routine that
	is quick, but has a poor worst case, to one that is slower (~3x) but has no pathological behaviour.
	This should be in the range BZ_MIN_WORK_FACTOR (=1) to BZ_MAX_WORK_FACTOR (=230), inclusive or
	be BZ_DEFAULT_WORK_FACTOR (=0, and equvalent to 30).  Higher values mean that it is willing to do more
	work before deciding that the supposedly 'quick' algorithm isn't appropriate.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self check: (self BZ2_bzCompressInit: aZLIBStream blockSize: anInteger1 verbosity: anInteger2 workFactor: anInteger3).
!

compressInit: aZLIBStream blockSize: anInteger1 workFactor: anInteger2
	"prepare the given 'stream' for compression.
	The block size determines how much memory (in 100K) is used and should be in the range
	BZ_MIN_BLOCK_SIZE (1) to BZ_MAX_BLOCK_SIZE (9) inclusive.
	The work factor is a measure of how readily the library will fall back from a sort routine that
	is quick, but has a poor worst case, to one that is slower (~3x) but has no pathological behaviour.
	This should be in the range BZ_MIN_WORK_FACTOR (=1) to BZ_MAX_WORK_FACTOR (=230), inclusive or
	be BZ_DEFAULT_WORK_FACTOR (=0, and equvalent to 30).  Higher values mean that it is willing to do more
	work before deciding that the supposedly 'quick' algorithm isn't appropriate.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self compressInit: aZLIBStream blockSize: anInteger1 verbosity: verbosity workFactor: anInteger2.
!

compressRun: aZLIBStream
	"one block's worth of ongoing compression.
	Answers BZ_RUN_OK (1)  or thows a BZip2Error"

	^ self check: (self BZ2_bzCompress: aZLIBStream action: BZ_RUN).
!

decompress: aZLIBStream
	"one block's worth of ongoing decompression.
	Answers BZ_OK (1), BZ_STREAM_END (4), or thows a BZip2Error"

	^ self check: (self BZ2_bzDecompress: aZLIBStream).
!

decompressEnd: aZLIBStream
	"release all resources that were assigned to the given 'stream' for decompression.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self check: (self BZ2_bzDecompressEnd: aZLIBStream).
!

decompressInit: aZLIBStream small: aBoolean
	"prepare the given 'stream' for decompression.
	If 'small' is true, then the library will use a slower, but less memory-hungy implementation.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self decompressInit: aZLIBStream verbosity: verbosity small: aBoolean.!

decompressInit: aZLIBStream verbosity: anInteger small: aBoolean
	"prepare the given 'stream' for decompression.
	The verbosity is a measure of how much chatter to write to the stdout stream, and should be in the
	range BZ_MIN_VERBOSITY (=0, silence) to BZ_MAX_VERBOSITY (=4) inclusive.
	If 'small' is true, then the library will use a slower, but less memory-hungy implementation.
	Answers BZ_OK (0)  or thows a BZip2Error"

	^ self check: (self BZ2_bzDecompressInit: aZLIBStream verbosity: anInteger small: aBoolean).
!

expectedVersion
	"answer the String version of the bzip2 library that this package expects to find"

	^ BZIP2_VERSION.!

initialise
	"private -- setup our initial state"

	verbosity := BZ_MIN_VERBOSITY.!

signalBZip2Error: anInteger
	"private -- raise an exception corresponding to the given
	error code"

	| messages message |

	messages :=
		#(
			'API sequence error'							"-1"			"i.e. your code is buggy"
			'API parameter error' 							"-2"			"still  buggy...."
			'Not enough memory is available' 					"-3"			"and you need  a bigger machine"
			'Compressed data is corrupted' 					"-4"			"oops!!"
			'Compressed data has wrong magic number'	"-5"			"probably a user error"
			'IO error' 								"-6"			"shouldn't happen with this wrapper since we do no I/O"
			'Uunexpected EOF' 							"-7"			"run out of file... (as above, shouldn't happen)"
			'Output buffer full' 							"-8"			"only used for 1-step [de]compression (which we don't wrap)"
			'Config error' 								"-9"			"something is *seriously* wrong with the DLL"
		).

	message := messages
			at: (0- anInteger)
			ifAbsent: ['Unknown error'].

	BZip2Error signal: message with: anInteger.

!

verbosity
	"answer the default 'verbosity' that is passed to the bzip2 library
	if not otherwise specified"

	^ verbosity.!

verbosity: anInteger
	"set the default 'verbosity' that will be passed to the bzip2 library
	if not otherwise specified.

	The verbose output is written to the standard output stream.  To see it
	in a developement image, execute:
		SessionManager current openConsole.
	to open the console window, and:
		SessionManager current closeConsole.
	to close it again.
	"

	self assert: [anInteger between: BZ_MIN_VERBOSITY "=0" and: BZ_MAX_VERBOSITY "=4"].

	verbosity := anInteger.!

version
	"answer the dotted String form of the library version.

		self default version
	"

	| version index |

	version := self BZ2_bzlibVersion.

	index := version indexOf: Character space.
	index = 0 ifTrue: [^ version].

	[index > 1 and: [' ,' includes: (version at: index)]] whileTrue: [index := index - 1].

	^ version first: index.! !
!BZip2Library categoriesFor: #BZ2_bzCompress:action:!compressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzCompressEnd:!compressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzCompressInit:blockSize:verbosity:workFactor:!compressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzDecompress:!decompressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzDecompressEnd:!decompressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzDecompressInit:verbosity:small:!decompressing!libbz2-basic functions!private! !
!BZip2Library categoriesFor: #BZ2_bzlibVersion!libbz2-utility functions!private!versions! !
!BZip2Library categoriesFor: #check:!helpers!private! !
!BZip2Library categoriesFor: #compressEnd:!compressing!public! !
!BZip2Library categoriesFor: #compressFinish:!compressing!public! !
!BZip2Library categoriesFor: #compressFlush:!compressing!public! !
!BZip2Library categoriesFor: #compressInit:blockSize:!compressing!public! !
!BZip2Library categoriesFor: #compressInit:blockSize:verbosity:workFactor:!compressing!public! !
!BZip2Library categoriesFor: #compressInit:blockSize:workFactor:!compressing!public! !
!BZip2Library categoriesFor: #compressRun:!compressing!public! !
!BZip2Library categoriesFor: #decompress:!decompressing!public! !
!BZip2Library categoriesFor: #decompressEnd:!decompressing!public! !
!BZip2Library categoriesFor: #decompressInit:small:!decompressing!public! !
!BZip2Library categoriesFor: #decompressInit:verbosity:small:!decompressing!public! !
!BZip2Library categoriesFor: #expectedVersion!constants!public!versions! !
!BZip2Library categoriesFor: #initialise!initializing!private! !
!BZip2Library categoriesFor: #signalBZip2Error:!errors!private! !
!BZip2Library categoriesFor: #verbosity!accessing!public! !
!BZip2Library categoriesFor: #verbosity:!accessing!public! !
!BZip2Library categoriesFor: #version!public!versions! !

!BZip2Library class methodsFor!

fileName
	"answer the host system file name for the library"

	^ 'libbz2-1'.!

rebuildPoolConstants
	"private -- rebuild the BZip2 pool constants dictionary.

		self rebuildPoolConstants.
	"

	(Smalltalk at: #BZip2Constants ifAbsentPut: [PoolConstantsDictionary new])

		"this is the version I used to build this package -- v1.0.0"
		at: 'BZIP2_VERSION'		put: '1.0.0';

		at: 'BZ_RUN' 			put: 0;
		at: 'BZ_FLUSH' 			put: 1;
		at: 'BZ_FINISH' 			put: 2;

		at: 'BZ_OK' 				put: 0;
		at: 'BZ_RUN_OK' 			put: 1;
		at: 'BZ_FLUSH_OK' 			put: 2;
		at: 'BZ_FINISH_OK' 			put: 3;
		at: 'BZ_STREAM_END' 		put: 4;
		at: 'BZ_SEQUENCE_ERROR' 	put: -1;
		at: 'BZ_PARAM_ERROR' 		put: -2;
		at: 'BZ_MEM_ERROR' 		put: -3;
		at: 'BZ_DATA_ERROR' 		put: -4;
		at: 'BZ_DATA_ERROR_MAGIC' 	put: -5;
		at: 'BZ_IO_ERROR' 			put: -6;
		at: 'BZ_UNEXPECTED_EOF' 	put: -7;
		at: 'BZ_OUTBUFF_FULL' 		put: -8;
		at: 'BZ_CONFIG_ERROR' 		put: -9;

		at: 'BZ_MIN_BLOCK_SIZE' 		put: 1;
		at: 'BZ_MAX_BLOCK_SIZE' 		put: 9;
		at: 'BZ_MIN_VERBOSITY'	 		put: 0;		"ie no chatter at all"
		at: 'BZ_MAX_VERBOSITY'	 		put: 4;
		at: 'BZ_DEFAULT_WORK_FACTOR' 	put: 0;		"equivalent to 30"
		at: 'BZ_MIN_WORK_FACTOR'	 	put: 1;
		at: 'BZ_MAX_WORK_FACTOR' 		put: 250;

		shrink.
! !
!BZip2Library class categoriesFor: #fileName!constants!public! !
!BZip2Library class categoriesFor: #rebuildPoolConstants!development!private! !

BZIP2Stream guid: (GUID fromString: '{8EFD963B-1BC1-4421-A079-943122EFC97C}')!
BZIP2Stream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org'!
!BZIP2Stream categoriesForClass!Unclassified! !
!BZIP2Stream methodsFor!

avail_in
	"Answer the receiver's avail_in field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

avail_in: anObject
	"Set the receiver's avail_in field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

avail_out
	"Answer the receiver's avail_out field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

avail_out: anObject
	"Set the receiver's avail_out field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

next_in
	"Answer the receiver's next_in field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

next_in: anObject
	"Set the receiver's next_in field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

next_out
	"Answer the receiver's next_out field as a Smalltalk object."

	^(bytes dwordAtOffset: 16) asExternalAddress!

next_out: anObject
	"Set the receiver's next_out field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

opaque
	"Answer the receiver's opaque field as a Smalltalk object."

	^(bytes dwordAtOffset: 44) asExternalAddress!

opaque: anObject
	"Set the receiver's opaque field to the value of anObject."

	bytes dwordAtOffset: 44 put: anObject!

total_in
	"Answer the receiver's total_in field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

total_out
	"Answer the receiver's total_out field as a Smalltalk object."

	^(bytes qwordAtOffset: 24)!

zalloc
	"Answer the receiver's zalloc field as a Smalltalk object."

	^(bytes dwordAtOffset: 36) asExternalAddress!

zalloc: anObject
	"Set the receiver's zalloc field to the value of anObject."

	bytes dwordAtOffset: 36 put: anObject!

zfree
	"Answer the receiver's zfree field as a Smalltalk object."

	^(bytes dwordAtOffset: 40) asExternalAddress!

zfree: anObject
	"Set the receiver's zfree field to the value of anObject."

	bytes dwordAtOffset: 40 put: anObject! !
!BZIP2Stream categoriesFor: #avail_in!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #avail_in:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #avail_out!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #avail_out:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #next_in!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #next_in:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #next_out!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #next_out:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #opaque!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #opaque:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #total_in!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #total_out!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #zalloc!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #zalloc:!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #zfree!**compiled accessors**!public! !
!BZIP2Stream categoriesFor: #zfree:!**compiled accessors**!public! !

!BZIP2Stream class methodsFor!

defineFields
	"
	self compileDefinition.

		typedef struct z_stream_s {
			char		*next_in;
			unsigned int	avail_in;
			unsigned int	total_in_lo32;
			unsigned int	total_in_hi32;

			char		*next_out;
			unsigned int	avail_out;
			unsigned int	total_out_lo32;
			unsigned int	total_out_hi32;

			void		*state;	
			
			void		*(*bzalloc)(void *,int,int);
			void		(*bzfree)(void *,void *);
			voidpf		opaque;
		} z_stream;
	"

	self

		defineField: #next_in type: LPVOIDField new;
		defineField: #avail_in type: DWORDField new;
"		defineField: #total_in_lo32 type: DWORDField readOnly;
		defineField: #total_in_hi32 type: DWORDField readOnly;
"		defineField: #total_in type: QWORDField readOnly;			"a bit cheeky, but it works..."

		defineField: #next_out type: LPVOIDField new;
		defineField: #avail_out type: DWORDField new;
"		defineField: #total_out_lo32 type: DWORDField readOnly;
		defineField: #total_out_hi32 type: DWORDField readOnly;
"		defineField: #total_out type: QWORDField readOnly;

		defineField: #state type: LPVOIDField filler;

		defineField: #zalloc type: LPVOIDField new;
		defineField: #zfree type: LPVOIDField new;
		defineField: #opaque type: LPVOIDField new;

		yourself.
! !
!BZIP2Stream class categoriesFor: #defineFields!constants!public! !

"Binary Globals"!

"Resources"!

