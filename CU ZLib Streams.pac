| package |
package := Package name: 'CU ZLib Streams'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Three Stream classes built on top of zlib compression.

Two are write streams. One, DeflaterWriteStream, that compresses any data that is written to it.  The other, InflaterWriteStream, decompresses anything that is written to it.  In both cases the processed data is then written to an underlying WriteStream.

Also includes a decompressing read stream, InflaterReadStream.  That wraps an existing ReadStream onto a source of compressed data, and decompresses it as you read.  Also includes a limited abillity to "push back" data, so as to make (for instance) splitting the data into lines easier.

There is no corresponding DeflaterReadStream because I can''t think of a use for it.

See the class comments for more information.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
History:

2.00
-	Added Inflater/DeflaterReadStream class>>file:text:.
-	Minor change to InflaterReadStream>>skip:

1.00
-	First release.
'.

package basicPackageVersion: '2.00'.


package classNames
	add: #DeflaterWriteStream;
	add: #InflaterReadStream;
	add: #InflaterWriteStream;
	add: #ZLibWriteStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	add: 'CU ZLib Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

AbstractReadStream subclass: #InflaterReadStream
	instanceVariableNames: 'controlBlock inBuffer firstIn lastIn outBuffer nextOut lastOut inStream crc isOpen isClosed zlibWindowBits atEnd skipBackLimit'
	classVariableNames: ''
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #ZLibWriteStream
	instanceVariableNames: 'controlBlock inBuffer firstIn lastIn outBuffer lastOut outStream crc isOpen isClosed zlibWindowBits'
	classVariableNames: ''
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!
ZLibWriteStream subclass: #DeflaterWriteStream
	instanceVariableNames: 'compressionLevel zlibMemLevel zlibStrategy dictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZLibWriteStream subclass: #InflaterWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

InflaterReadStream guid: (GUID fromString: '{3D9B1B42-DBA1-4B49-8ED7-9A919DE3745B}')!
InflaterReadStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these wraps a (binary) ReadStream; it reads data from the input stream and uses the zlib library to decompress it, then makes the decompressed data available via the usual read stream protocol.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

The stream maintains a limited buffer to allow it to be positioned backwards.  By default it can skip back over the previously read object -- i.e. a #skipBackLimit of 1.  The limit: can be changed *before any data is read* to a higher value or to zero.  The bigger the skip back buffer, the less efficient reading will be.

Note that *arbitrary* positioning is not supported -- that would require buffering the entire output, and if you are going to do that then you may as well just read the whole thing into a String/ByteArray and use that directly.  That means that although methods that use limited lookahead (e.g: #nextLine) will work OK, methods like #contents which jump about arbitrarily will not work.  Methods that don''t use positioning, such as #upToEnd, are fine, of course, as are methods that only skip forwards (like #setToEnd).

Since we don''t know the size of the uncompressed data until we have decompressed it, #size isn''t supported either.

Since the decompressor is reading from the underlying stream into its own (and zlib''s) buffers, there''s a danger that it can overrun the limits of the compressed part of a stream.  If that matters to you, then you can either turn off input buffering, at some cost in execution time, by sending #unBufferInput before you start reading; or (if the underlying stream is positionable) use #pushBackUnconsumedInput: after you''ve finished reading.

The decompressor can be set to read any of the three modes supported by zlib -- zlib (=''deflate''), gzip, and raw.  These differ only in the kinds of header/trailer attatched to the compressed data.  See the comment on ZLibWriteStream>>zlibFormat: for more information.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to acquire and release resource in the ZLIB library.  (They do close themselves by finalisation, if necessary).

Also see the class-side methods in the ''examples'' category.
'!
!InflaterReadStream categoriesForClass!Unclassified! !
!InflaterReadStream methodsFor!

adlerChecksum
	"answer the Integer 'Adler' checksum of all the uncompressed data we have seen.
	Note that this is an error until after we have been #close-ed.
	Note also that the adler checksum is not maintained in #Raw mode"

	isClosed ifFalse: [self errorNotYetClosed].

	^ controlBlock adler.!

allocateBuffers
	"private -- allocate the working buffers and initialise the start/end pointers into them.
	Note that this is initially set up for binary uncompressed data"

	inBuffer := self makeBuffer: (self defaultInBufferSize) binary: true.
	self resetInBuffer.

	outBuffer := self makeBuffer: (self defaultOutBufferSize + skipBackLimit) binary: true.
	self resetOutBuffer.
!

atEnd
	"one of the root methods for positionable streams"

	"ensure we are open, since we don't know until then"
	isOpen ifFalse:
		[isClosed ifTrue: [^ true].
		self open].

	^ self basicAtEnd.!

basicAtEnd
	"private -- on the assumption that we have been opened and not yet closed,
	answer whether we have reached the end fo the compressed input"

	^ atEnd and: [nextOut > lastOut].!

beBinary
	"set whether we produce text or binary output"

	self beBinary: true.!

beBinary: aBool
	"set whether we produce text or binary output"

	(self isBinary = aBool)
		ifFalse: [self makeOutBufferBinary: aBool].!

beClosed
	"private -- record the fact that we are no longer open"

	"we don't need to be finalisable any more"
	self beUnfinalizable.

	isClosed := true.
	isOpen := false.!

beOpen
	"private -- record the fact that we are now open"

	"we now have to ensure that we'll clean up if necessary"
	self beFinalizable.

	isOpen := true.!

beRaw
	"set us to use zlib in a mode where no header or crc will be added to/expected in
	the input stream.
	Note that it is an error to call this after we have been #open-ed"

	self zlibFormat: #Raw.!

beText
	"set whether we produce text or binary output"

	self beBinary: false.!

beText: aBool
	"set whether we produce text or binary output"

	self beBinary: aBool not.!

checkStatus: anInteger
	"private -- if anInteger is less than Z_OK (which means an error) then trigger a zlib error.
	This is called immediately after most zlib operations"

	anInteger < Z_OK ifTrue: [self zlibError: anInteger].

	^ anInteger.!

close
	"complete or abandon the processes of decompressing our input data, and
	release all allocated resources.
	Until this has been called the computed #adler and #crc checksums are not valid.
	NB: this neither #close-s nor #flush-es the output stream, you have to
	do that yourself if you are, say, writing to a file"

	isClosed ifTrue: [^ self].

	"complete the zlib stuff.
	if nothing has been written then that is an error (since 0 bytes is not valid
	compressed data), but there seems little point in worrying about it"
	isOpen ifTrue: [self reallyClose].

	self beClosed.
!

closeAbruptly
	"private -- ensure that we have no further resources managed by ZLib.
	This is different from #discardZlibConnection in that it assumes that
	zlib is still valid and so has to be cleaned up, and from #close in that it
	doesn't attempt to flush any outstanding data or check for errors"

	isOpen ifTrue: [self reallyCloseAbruptly; beClosed].!

closeZLibStream
	"private -- close the zlib stream or throw an error"

	^ self checkStatus: (self closeZLibStreamAbruptly).
!

closeZLibStreamAbruptly
	"private -- close the zlib stream without any error checking"

	^ self library inflateEndStream: controlBlock.
!

compressedSize
	"answer the compressed size of our data.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ self compressedSizeSoFar.!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_in.
!

compressionRatio
	"answer the compression ratio we managed.
	Note that this is an error until after we have been #close-ed"

	^ self compressedSize / self uncompressedSize.!

compressionRatioSoFar
	"answer the compression ratio we are seeing to date.
	unlike #compressionRatio, this is can be called before we have been
	closed, but it is an approximation until that time since it doesn't take
	any account of data held in buffers"

	#CUtodo.  "we should probably at least #flushPartial here"

	^ self compressedSizeSoFar / self uncompressedSizeSoFar.
!

contentsSpecies
	"private -- overriden to answer the correct type of data for, e.g. #next: or #upToEnd"

	^ outBuffer class.!

controlBlock
	"private -- answer the ZLIBStream that holds the state of our processing"

	^ controlBlock.!

crcChecksum
	"answer the Integer CRC checksum of all the uncompressed data we have seen.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ crc.!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ self class defaultInBufferSize.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ self class defaultOutBufferSize.!

defaultSkipBackLimit
	"answer how many extra objects we keep in our buffer in order
	to support negative positioning (e.g. in #peek) by default"

	^ self class defaultSkipBackLimit.!

discardZlibConnection
	"private -- ensure that we will make no further use of resources managed by ZLib.
	This is different from #closeAbruptly in that it assumes that zlib is already
	dead"

	self isOpen ifTrue: [self beClosed].!

errorAlreadyClosed
	"private -- trigger a ZLibError to say that the stream has already been closed"

	^ ZLibError
		signal: 'already closed'
		with: Z_STREAM_ERROR.!

errorAlreadyOpen
	"private -- trigger a ZLibError to say that the stream has already been opened"

	^ ZLibError
		signal: 'already open'
		with: Z_STREAM_ERROR.!

errorNotPositionable: anInteger
	"private -- trigger an error because we cannot change position to anInteger"

	"this may be marginally more helpful than throwing a custom Error, and it's much easier ;-)"
	^ self errorSubscriptBounds: anInteger.
!

errorNotYetClosed
	"private -- trigger a ZLibError to say that the stream has not yet been closed"

	^ ZLibError
		signal: 'not yet closed'
		with: Z_STREAM_ERROR.!

fillInBuffer
	"private -- try to ensure that we are holding some data in our inBuffer"

	"if the buffer's empty then start again at the begining"
	firstIn > lastIn ifTrue: [self resetInBuffer].

	"we have to use an explicit loop or we would fall foul of undefined
	behaviour when attempting to read past the end of the stream"
	#CUtodo.  "can we use #nextAvailable: safely?"
	[inStream atEnd or: [lastIn >= inBuffer size]] whileFalse:
		[lastIn := lastIn + 1.
		inBuffer at: lastIn put: inStream next].
!

fillOutBuffer
	"private -- ensure that we are holding some decompressed data in our outBuffer"

	| status |

	"ensure the out buffer's 'empty'"
	self resetOutBufferToSkipback.

	"loop until zlib has managed to put some data into the out buffer, or it
	signals eond-of-stream.  We need the loop since the in buffer may not
	hold enough data for zlib to produce *anything* on one itteration"
	[self fillOutBufferStep] whileFalse.

	"add all the newly decompressed data to the crc"
	crc := self library
			crc32: crc
			buffer: outBuffer yourAddress + nextOut - 1
			size: lastOut - nextOut + 1.!

fillOutBufferStep
	"private -- ask zlib to generate some data in our out buffer (which is assumed to be empty).
	Answers whether it has managed to do so or has found end-of-stream, in either case we
	can stop bothering zlib for now.
	(Incidentally, the only real reason for this method to exist was to factor out the body of
	an awkward loop in #fillOutBuffer -- one of the cases where Smalltalk's flow-of-control
	syle forces a clumsy factoring)"

	| status |

	"already finished ?"
	atEnd ifTrue: [^ true].

	"try to fill the in buffer (null-op if the input's at EOF or the buffer's full)"
	self fillInBuffer.

	"ask zlib to fill the out buffer"
	status := self flushZLibStream: Z_NO_FLUSH.

	"if zlib needs a decompression dictionary then trigger a resumable error to say so"
	status = Z_NEED_DICT ifTrue: [self requestDecompressionDictionary].

	"has zlib found end-of-stream in its data ?  If so, then ideally we'd put the leftover data back
	on the input stream, but that assumes that the input stream is positionable, which it may not be.
	So we leave the stream where it is, but we provide an accessor to get any leftover data in case
	it's needed -- #unconsumedInput, and also (which may not work, but it's handy to have),
	#pushBackUnconsumedInput.  Alternatively you can avoid the problem, at the expense of
	efficiency, running with 'no' input buffer"
	status = Z_STREAM_END ifTrue: [atEnd := true].

	^ atEnd or: [lastOut >= nextOut].!

finalize
	"we should ensure that we have released any resouces used
	internally by zlib"

	self closeAbruptly.!

flushZLibStream: anInteger
	"private -- flush the compression stream to the given 'level' or throw an error.
	Answers the status (which may take several OK values)"

	| status |

	self updateControlBlock.
	status := self checkStatus: (self library inflateStream: controlBlock flush: anInteger).
	self updateFromControlBlock.

	^ status.!

inBufferSize
	"answer the size of the buffer used to hold data before
	decompression"

	^ inBuffer size.!

inBufferSize: anInteger
	"set the size of the buffer used for holding data before processsing.
	This influences the number of external calls to the zlib library, so
	a large buffer has less effect on efficiency than, for instance, a
	large buffer used for a FileStream.  However a moderately sized
	buffer does help.  The smallest legal value is 1 which causes us
	to consume no more of our input stream than we actually decompress.
	Note that it is an error to call this after we have been #open-ed"

	self assert: [anInteger >= 1].

	isOpen ifTrue: [^ self errorAlreadyOpen].

	inBuffer size = anInteger ifFalse:
		[inBuffer := inBuffer class newFixed: anInteger].

!

initialize
	"private -- establish a coherent initial state.
	Note that we initially assume we are going to have binary uncompressed data"

	controlBlock := ZLIBStream new.
	isOpen := isClosed := atEnd := false.
	skipBackLimit := self defaultSkipBackLimit.

	"see ZLib1Library>>inflateInitStream2:windowBits: and #deflateInit2Stream:level:method:windowBits:memLevel:strategy:"
	zlibWindowBits := 15.

	self allocateBuffers.!

initializeChecksums
	"private -- initialize the running checksums that we maintain"

	"the zlib library maintains the adler checksum for us, 
	but we have to do the crc32 ourselves"
	crc := self library initialCrc32.
!

inStream
	"answer the stream from which we read compressed data"

	^ inStream.!

inStream: aBinaryReadStream
	"private -- set the stream from which we will read compressed data"

	inStream := aBinaryReadStream.!

isBinary
	"answer whether we producet binary output"

	^ outBuffer isKindOf: ByteArray.!

isClosed
	"answer whether we have finished all the decompression we are going to do.
	NB: untill this returns true, the contents of the output stream are incomplete,
	anf the values of #crc and #adler are undefined"

	^ isClosed.!

isOpen
	"answer whether we have started decompression yet.
	NB: this is largely meaningless since we normally
	'open' lazilly when we decide to flush our inBuffer the first time"

	^ isOpen.!

isText
	"answer whether we producet text output"

	^ self isBinary not.
!

library
	"private -- answer the external library that we use"

	^ ZLib1Library default.!

makeBuffer: anInteger binary: aBoolean
	"private -- allocate a buffer to use for holding char or byte data while the zlib library works on it"

	^ (aBoolean ifTrue: [ByteArray] ifFalse: [String])
		newFixed: anInteger.!

makeInBufferBinary: aBool
	"private -- convert the existing inBuffer to text/binary according as aBool"

	| new |

	inBuffer isNil ifFalse:
		[new := self makeBuffer: inBuffer size binary: aBool.
		inBuffer replaceBytesOf: new from: firstIn to: lastIn startingAt: firstIn.
		inBuffer := new].
!

makeOutBufferBinary: aBool
	"private -- convert the existing outBuffer to text/binary according as aBool"

	| new |

	outBuffer isNil ifFalse:
		[new := self makeBuffer: outBuffer size binary: aBool.
		outBuffer replaceBytesOf: new from: 1 to: lastOut startingAt: 1.
		outBuffer := new].
!

next
	"one of the root methods of <ReadStream>"

	| answer |

	self atEnd ifTrue: [^ self errorEndOfStream].
	answer := outBuffer at: nextOut.
	nextOut := nextOut + 1.

	"ensure we don't leave the buffers empty"
	nextOut > lastOut ifTrue: [self fillOutBuffer].

	^ answer.!

next: anInteger into: destination startingAt: anIndex
	"overriden for improved performance (about 6x on a big collection)"

	| index todo |

	"ensure we are open"
	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	"nasty loop: fill the buffer, dump it all out to the destination, refill the buffer..."
	todo := anInteger.
	index := anIndex.
	[todo > 0] whileTrue:
		[| chunk |
		self basicAtEnd ifTrue: [^ self errorEndOfStream].
		chunk := (lastOut - nextOut + 1) min: todo.
		destination
			replaceFrom: index
			to: index + chunk - 1
			with: outBuffer
			startingAt: nextOut.
		index := index + chunk.
		todo := todo - chunk.
		nextOut := nextOut + chunk.
		nextOut > lastOut ifTrue: [self fillOutBuffer]].

	^ destination.!

onStartup
	"private -- this is called at image startup, ensure that we do not attempt
	to use resources that were allocated by zlib in the previous session"

	self discardZlibConnection.!

open
	"initialize the underlying zlib library's data structures for processing
	our data.
	It is not normally necessary to call this explicitly since we #open
	lazily when we first are asked for any output"

	isOpen ifFalse: [self reallyOpen; beOpen].!

openZLibStream
	"private -- open the zlib stream or throw an error"

	^ self checkStatus: (self library inflateInit2Stream: controlBlock windowBits: zlibWindowBits).!

outBufferSize
	"answer the size of the buffer used to hold data after
	decompression.  This value excludes the size of the
	skipBackBuffer (if any)"

	^ outBuffer size - skipBackLimit.!

outBufferSize: anInteger
	"set the size of the buffer used for holding data after processsing.
	This influences the number of external calls to the zlib library, and
	also how efficiently we can 'drive' our outStream.  A large buffer
	has less effect on efficiency than, for instance, a large buffer used
	for a FileStream.  However a moderately sized buffer does help.
	This value excludes the size of the skipBackBuffer (if any)
	The smallest legal value is 1.
	Note that it is an error to call this after we have been #open-ed"

	| newSize |

	self assert: [anInteger >= 1].

	isOpen ifTrue: [^ self errorAlreadyOpen].

	newSize := anInteger + skipBackLimit.
	outBuffer size = newSize ifFalse:
		[outBuffer := outBuffer class newFixed: newSize].!

position
	"answer how many objects have been read from this stream"

	^ controlBlock total_out - lastOut + nextOut - 1.!

position: anInteger
	"if we can change our position to that given then do so, otherwise
	trigger an error"

	self skip: (anInteger - self position).!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: (self isText ifTrue: [' forTextOn: '] ifFalse: [' forBinaryOn: ']);
		display: inStream.!

pushBackUnconsumedInput
	"return any data that we have read from our input stream but which we have not
	uncompressed.  This exists because we may be connected to an input stream from
	which we have just decompressed data up to the point where the zlib library states
	that the compressed stream is finished.  In such cases we may have read more bytes
	into our own buffers than we needed to.  This assumes that the input stream is positionable"

	firstIn <= lastIn ifTrue:
		[inStream skip: firstIn - lastIn - 1.	"negative"
		self resetInBuffer].


	!

reallyClose
	"private -- complete or abandon the process of decompression"

	self closeZLibStream.
!

reallyCloseAbruptly
	"private -- ensure that we have no further resources managed by ZLib"

	self closeZLibStreamAbruptly.!

reallyOpen
	"private -- initialize the underlying zlib library's data structures for decompressing
	our data"

	self
		initializeChecksums;
		openZLibStream;
		fillOutBuffer.!

requestDecompressionDictionary
	"private -- trigger a *resumable* error stating that zlib cannor carry on without
	being given a decompression dictionay.  The tag of the exception is the adler
	checksum that zlib uses to 'identify' which dictionary it needs.
	If you want to handle this, then you should resume the exeption with the correct
	dictionary.
	The word 'dictionary' here is a bit misleading, all the library really wants is
	some example text"

	| dictionary |

	dictionary := ZLibDictionaryNeededException signalWith: (controlBlock adler).

	self checkStatus: (self library
				inflateSetDictionary: controlBlock
				dictionary: dictionary yourAddress
				dictLength: dictionary size).
!

resetInBuffer
	"private -- start again at the begining of the in buffer"

	firstIn := 1.
	lastIn := 0.!

resetOutBuffer
	"private -- start again at the begining of the out buffer,
	discarding any input that might have been used to
	implement skip back.
	c.f. #resetOutBufferToSkipback"

	nextOut := 1.
	lastOut := 0.!

resetOutBufferToSkipback
	"private -- start again at the begining of the out buffer
	whilst attempting to preserve enough data to implement our
	skip back limit.
	c.f. #resetOutBuffer"

	skipBackLimit > 0
		ifTrue:
			[| save |
			save := skipBackLimit min: lastOut.
			1 to: save do: [:i | outBuffer at: i put: (outBuffer at: lastOut - save + i)].
			nextOut := 1 + save.
			lastOut := save]
		ifFalse:
			[nextOut := 1.
			lastOut := 0].

!

setToEnd
	"skip over any remaining input"

	"ensure we are open"
	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	[self basicAtEnd] whileFalse:
		[nextOut := lastOut + 1.
		self fillOutBuffer].
!

size
	"one of the root methods for positionable Streams"

	"we can't implement this"
	#CUtodo.  "or can we ?"
	self shouldNotImplement.!

skip: anInteger
	"if we can change our position by the given distance then do so, otherwise
	trigger an error"

	| newNextOut todo |

	"ensure we are open"
	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	newNextOut := nextOut + anInteger.

	"exceeding the configured skipback limit ?"
	anInteger < (0 - skipBackLimit) ifTrue: [^ self errorNotPositionable: anInteger].

	"exceeding the available skipback data ?"
	newNextOut < 1 ifTrue: [^ self errorNotPositionable: anInteger].

	"fill and discard the buffer until we reach the right place"
	todo := anInteger.
	[newNextOut > lastOut] whileTrue:
		[| discard |
		self basicAtEnd ifTrue: [^ self errorEndOfStream].
		discard := lastOut - nextOut + 1.
		todo := todo - discard.
		nextOut := lastOut + 1.
		self fillOutBuffer.
		newNextOut := nextOut + todo].

	"and now we can finish off by updating the pointer"
	nextOut := newNextOut.
!

skipBackLimit
	"answer how many objects we are configured to save so
	that we can 'unread' them with #pop, or #position.  The
	default is 1 which is sufficient for may applications"

	^ skipBackLimit.!

skipBackLimit: anInteger
	"set how many objects we are configured to save so
	that we can 'unread' them with #pop, or #position.  The
	default is 1 which is sufficient for may applications.  Note
	that any figure > 0 implies extra copying of data.  The
	minimum legal value is 0.
	It is an error to use this after we have been #open-ed"

	| newSize |

	self assert: [anInteger >= 0].

	isOpen ifTrue: [^ self errorAlreadyOpen].

	newSize := outBuffer size - skipBackLimit + anInteger.
	outBuffer size = newSize ifFalse:
		[outBuffer := outBuffer class newFixed: newSize.
		skipBackLimit := anInteger].!

unBufferInput
	"arrange that we will not consume any more of our input stream
	than is actually needed to decompress the data from it (hence
	we won't read beyond the end of the compressed data).
	Note that it is an error to call this after we have been #open-ed"

	self inBufferSize: 1.!

uncompressedSize
	"answer the uncompressed size of our data.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ self uncompressedSizeSoFar.!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_out.
!

unconsumedInput
	"answer any data that we have read from our input stream but which we have not
	uncompressed.  This exists because we may be connected to an input stream from
	which we have just decompressed data up to the point where the zlib library states
	that the compressed stream is finished.  In such cases we may have read more bytes
	into our own buffers than we needed to.  Since we can't assume that we can push
	the data back into the input stream (which may not be positionable, e.g. reading from
	a socket) this accessor will answer whatever data is leftover.
	It makes very little sense to call this until after we have reached the end of the compressed
	input, but it won't break anything so that rule is not enforced"

	^ inBuffer copyFrom: firstIn to: lastIn.!

updateControlBlock
	"private -- update our control block with our current buffers"

	controlBlock
		next_in: (inBuffer yourAddress + firstIn - 1);			"first byte that contains valid data"
		avail_in: (lastIn - firstIn + 1);					"number of valid bytes"
		next_out: (outBuffer yourAddress + lastOut);	"first byte that has not yet been written to"
		avail_out: (outBuffer size - lastOut);				"number of bytes before the end of the buffer"
		msg: nil.							"may as well reset this too"!

updateFromControlBlock
	"private -- update our current buffers with data from the control block"

	firstIn := controlBlock next_in asInteger - inBuffer yourAddress asInteger + 1.			"first byte that contains valid data"
	lastIn := firstIn + controlBlock avail_in - 1.													"last byte that contains valid data"
	lastOut := controlBlock next_out asInteger - outBuffer yourAddress asInteger.			"last byte that contains valid data"
!

upToEnd
	"overriden for improved performance (about 6X on a long stream)"

	| stream |

	"ensure we are open"
	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	stream := self contentsSpecies writeStream: 128.
	[self basicAtEnd] whileFalse:
		[stream
			next: (lastOut - nextOut + 1)
			putAll: outBuffer
			startingAt: nextOut.
		nextOut := lastOut + 1.
		self fillOutBuffer].

	^ stream contents.!

windowBits
	"answer the log2 of the size of the decompression window we will use"

	^ zlibWindowBits < 0
		ifTrue: [0 - zlibWindowBits]
		ifFalse: [zlibWindowBits > 15
			ifTrue: [zlibWindowBits - 16]
			ifFalse: [zlibWindowBits]].!

windowBits: anInteger
	"set the log2 of the size of the window we will use for (de)compression.
	The value must lie in the range 8..15.
	Note that it is an error to call this after we have been #open-ed"

	self
		windowBits: anInteger
		zlibFormat: self zlibFormat.!

windowBits: anInteger zlibFormat: aSymbol
	"set the log2 of the size of the window we will use for (de)compression and the format
	according to which zlib format we expect to read/write.
	The integer must lie in the range 8..15.
	The symbol should be one of: #(#ZLib #GZip #Raw). see #zlibFormat for the meanings of these values.
	Note that it is an error to call this after we have been #open-ed"

	| newWindowBits |

	self assert: [(8 to: 15) includes: anInteger].
	self assert: [#(#ZLib #GZip #Raw) includes: aSymbol].

	isOpen ifTrue: [^ self errorAlreadyOpen].	#CUtodo.  "remove this restriction"

	newWindowBits := anInteger.
	aSymbol = #Raw ifTrue: [newWindowBits := 0 - newWindowBits].
	aSymbol = #GZip ifTrue: [newWindowBits := 16 + newWindowBits].

	zlibWindowBits := newWindowBits.!

windowSize
	"answer the size of the decompression window we will use"

	^ 1 bitShift: self windowBits.!

windowSize: anInteger
	"set the size of the window we will use for (de)compression.
	The value must lie in the range 256 ..32768 and will be rounded down to
	a power of 2.
	Note that it is an error to call this after we have been #open-ed"

	self windowBits: anInteger highBit - 1.!

zlibError: anInteger
	"private -- trigger a ZLibError with anInteger as its error tag, and the error message in our control block as the message"

	^ ZLibError
		signal: (controlBlock msg)
		with: anInteger.!

zlibFormat
	"answer one of:
		#(#ZLib #GZip #Raw)
	according to which zlib format we expect to read/write.

	#Raw is the underlying compressed format with no identifying header or checksum.
	It is used (for instance) in Zip files, and in other applications that define their own
	wrappers and sanity check externally to the zlib library.  This format is described in
	RFC 1951.

	#ZLib is the #Raw format with a 2-byte identifying header (#[120 156] I believe)
	and the Adler checksum added to the end (32bits in network byte order).  This format
	is described in RFC 1950.  It is also the format that we use by default.

	#GZlip is a simple implementation of the gzip format that is implemented directly by
	zlib, it includes/parses minimal header and crc data to interwork with programs using
	that format.  (The zlib library also includes more elaborate support for gzip format, but that
	only manipulates files, and is not exposed via these classes.)  The gzip format is described
	in RFC 1952.

	Note that, according to the zlib FAQ, in HTTP transfers,
			Content-Encoding: gzip
	means the #GZip format, and
			Content-Encoding: deflate
	means what we call #ZLib, however the FAQ also notes that several implementations
	(including Microsoft) wrongly use it to mean what we call #Raw.

	Also note that the zlib documentation implies that a #GZip compressed stream should be recognised
	and decoded automatically by a default inflater -- this appears not to be true, you have to tell the
	inflater to expect #GZip explicitly"

	"in point of fact the format is encoded in the window bits in a particularly hacky way..."
	^ zlibWindowBits < 0
		ifTrue: [#Raw]
		ifFalse: [zlibWindowBits > 15
			ifTrue: [#GZip]
			ifFalse: [#ZLib]].!

zlibFormat: aSymbol
	"set the format according to which zlib format we expect to read/write.  The symbol
	should be one of:
		#(#ZLib #GZip #Raw)
	See #zlibFormat for the meanings of these values.
	Note that it is an error to call this after we have been #open-ed"

	self
		windowBits: self windowBits
		zlibFormat: aSymbol.! !
!InflaterReadStream categoriesFor: #adlerChecksum!accessing!public! !
!InflaterReadStream categoriesFor: #allocateBuffers!helpers!private! !
!InflaterReadStream categoriesFor: #atEnd!public!streaming!testing! !
!InflaterReadStream categoriesFor: #basicAtEnd!private!streaming!testing! !
!InflaterReadStream categoriesFor: #beBinary!modes!public! !
!InflaterReadStream categoriesFor: #beBinary:!modes!public! !
!InflaterReadStream categoriesFor: #beClosed!modes!operations!private! !
!InflaterReadStream categoriesFor: #beOpen!modes!operations!private! !
!InflaterReadStream categoriesFor: #beRaw!initializing!modes!public! !
!InflaterReadStream categoriesFor: #beText!initializing!modes!public! !
!InflaterReadStream categoriesFor: #beText:!modes!public! !
!InflaterReadStream categoriesFor: #checkStatus:!helpers!private! !
!InflaterReadStream categoriesFor: #close!operations!public! !
!InflaterReadStream categoriesFor: #closeAbruptly!operations!private! !
!InflaterReadStream categoriesFor: #closeZLibStream!operations!private! !
!InflaterReadStream categoriesFor: #closeZLibStreamAbruptly!operations!private! !
!InflaterReadStream categoriesFor: #compressedSize!accessing!public! !
!InflaterReadStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!InflaterReadStream categoriesFor: #compressionRatio!accessing!public! !
!InflaterReadStream categoriesFor: #compressionRatioSoFar!accessing!public! !
!InflaterReadStream categoriesFor: #contentsSpecies!helpers!private! !
!InflaterReadStream categoriesFor: #controlBlock!accessing!private! !
!InflaterReadStream categoriesFor: #crcChecksum!accessing!public! !
!InflaterReadStream categoriesFor: #defaultInBufferSize!constants!public! !
!InflaterReadStream categoriesFor: #defaultOutBufferSize!constants!public! !
!InflaterReadStream categoriesFor: #defaultSkipBackLimit!constants!public! !
!InflaterReadStream categoriesFor: #discardZlibConnection!operations!private! !
!InflaterReadStream categoriesFor: #errorAlreadyClosed!exceptions!private! !
!InflaterReadStream categoriesFor: #errorAlreadyOpen!exceptions!private! !
!InflaterReadStream categoriesFor: #errorNotPositionable:!exceptions!positioning!private! !
!InflaterReadStream categoriesFor: #errorNotYetClosed!exceptions!private! !
!InflaterReadStream categoriesFor: #fillInBuffer!operations!private! !
!InflaterReadStream categoriesFor: #fillOutBuffer!operations!private! !
!InflaterReadStream categoriesFor: #fillOutBufferStep!operations!private! !
!InflaterReadStream categoriesFor: #finalize!finalizing!public! !
!InflaterReadStream categoriesFor: #flushZLibStream:!operations!private! !
!InflaterReadStream categoriesFor: #inBufferSize!accessing!public! !
!InflaterReadStream categoriesFor: #inBufferSize:!accessing!initializing!public! !
!InflaterReadStream categoriesFor: #initialize!initializing!private! !
!InflaterReadStream categoriesFor: #initializeChecksums!operations!private! !
!InflaterReadStream categoriesFor: #inStream!accessing!public! !
!InflaterReadStream categoriesFor: #inStream:!initializing!private! !
!InflaterReadStream categoriesFor: #isBinary!modes!public!testing! !
!InflaterReadStream categoriesFor: #isClosed!modes!public!testing! !
!InflaterReadStream categoriesFor: #isOpen!modes!public!testing! !
!InflaterReadStream categoriesFor: #isText!modes!public!testing! !
!InflaterReadStream categoriesFor: #library!helpers!private! !
!InflaterReadStream categoriesFor: #makeBuffer:binary:!helpers!private! !
!InflaterReadStream categoriesFor: #makeInBufferBinary:!helpers!private! !
!InflaterReadStream categoriesFor: #makeOutBufferBinary:!helpers!private! !
!InflaterReadStream categoriesFor: #next!public!reading!streaming! !
!InflaterReadStream categoriesFor: #next:into:startingAt:!public!reading! !
!InflaterReadStream categoriesFor: #onStartup!event handling!private! !
!InflaterReadStream categoriesFor: #open!operations!public! !
!InflaterReadStream categoriesFor: #openZLibStream!operations!private! !
!InflaterReadStream categoriesFor: #outBufferSize!accessing!public! !
!InflaterReadStream categoriesFor: #outBufferSize:!accessing!initializing!public! !
!InflaterReadStream categoriesFor: #position!accessing!positioning!public! !
!InflaterReadStream categoriesFor: #position:!positioning!public! !
!InflaterReadStream categoriesFor: #printOn:!printing!public! !
!InflaterReadStream categoriesFor: #pushBackUnconsumedInput!operations!positioning!public! !
!InflaterReadStream categoriesFor: #reallyClose!operations!private! !
!InflaterReadStream categoriesFor: #reallyCloseAbruptly!operations!private! !
!InflaterReadStream categoriesFor: #reallyOpen!operations!private! !
!InflaterReadStream categoriesFor: #requestDecompressionDictionary!exceptions!private! !
!InflaterReadStream categoriesFor: #resetInBuffer!helpers!private! !
!InflaterReadStream categoriesFor: #resetOutBuffer!helpers!private! !
!InflaterReadStream categoriesFor: #resetOutBufferToSkipback!helpers!operations!private! !
!InflaterReadStream categoriesFor: #setToEnd!positioning!public! !
!InflaterReadStream categoriesFor: #size!accessing!positioning!public! !
!InflaterReadStream categoriesFor: #skip:!positioning!public! !
!InflaterReadStream categoriesFor: #skipBackLimit!accessing!public! !
!InflaterReadStream categoriesFor: #skipBackLimit:!accessing!initializing!public! !
!InflaterReadStream categoriesFor: #unBufferInput!initializing!modes!public! !
!InflaterReadStream categoriesFor: #uncompressedSize!accessing!public! !
!InflaterReadStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!InflaterReadStream categoriesFor: #unconsumedInput!accessing!public! !
!InflaterReadStream categoriesFor: #updateControlBlock!helpers!private! !
!InflaterReadStream categoriesFor: #updateFromControlBlock!helpers!private! !
!InflaterReadStream categoriesFor: #upToEnd!public!reading! !
!InflaterReadStream categoriesFor: #windowBits!accessing!public! !
!InflaterReadStream categoriesFor: #windowBits:!initializing!modes!public! !
!InflaterReadStream categoriesFor: #windowBits:zlibFormat:!initializing!modes!public! !
!InflaterReadStream categoriesFor: #windowSize!accessing!public! !
!InflaterReadStream categoriesFor: #windowSize:!initializing!modes!public! !
!InflaterReadStream categoriesFor: #zlibError:!exceptions!private! !
!InflaterReadStream categoriesFor: #zlibFormat!accessing!modes!public! !
!InflaterReadStream categoriesFor: #zlibFormat:!initializing!modes!public! !

!InflaterReadStream class methodsFor!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ 1024.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	"since we are expecting more output than input"
	^ 2048.!

defaultSkipBackLimit
	"answer how many extra objects we keep in our buffer in order
	to support negative positioning (e.g. in #peek) by default"

	"just enough to make #peekFor:, and hence #nextLine and #nextWord, work"
	^ 1.!

exampleDecompressingAByteArray
	"decompressing a ByteArray and writing it to the Transcript

		self exampleDecompressingAByteArray.
	"

	| compressedData inflater |

	compressedData :=  DeflaterWriteStream exampleCompressingAByteArray.

	inflater := InflaterReadStream forBinaryOn: compressedData readStream.

	Transcript nextPutAll: 'Decompressed: #('.
	inflater do: [:each | Transcript display:each; space].
	Transcript nextPutAll: ')'; cr.

	inflater close.!

exampleDecompressingAShortStringUsingADictionary
	"decompressing text that has been compressed more effectively by pre-conditioning the compressor
	with a fixed, known, 'dictionary' of sample text.

		self exampleDecompressingAShortStringUsingADictionary.

	"

	| dictionary compressedData inflater |

	"we have to be able to provide the inflater with the same example text as was
	used to compress it.  To keep it easy, we just hardwire that text here"
	dictionary := 'The cat sat on the mat sat on the rat sat on the cat sat on the mat'.

	"this is data compressed using the same dictionary"
	compressedData :=  DeflaterWriteStream exampleCompressingAShortStringUsingADictionary.

	inflater := InflaterReadStream forTextOn: compressedData readStream.
	Transcript nextPutAll: 'Decompressed: '''.

	"try to decompress the data, at some point (determined by zlib) it will throw a
	resumable ZLibDictionaryNeededException, which we -- if we wish to continue -- must
	resume with the 'disctionary' text"
	[inflater do: [:each | Transcript nextPut: each]]
		on: ZLibDictionaryNeededException 
		do: [:ex | ex resume: dictionary].

	Transcript nextPutAll: ''''; cr.
	inflater close.
!

exampleDecompressingAString1
	"decompressing a String from a ByteArray and writing it to the Transcript.

		self exampleDecompressingAString1.
	"

	| compressedData inflater |

	compressedData :=  DeflaterWriteStream exampleCompressingAString.

	inflater := InflaterReadStream forTextOn: compressedData readStream.

	Transcript nextPutAll: 'Decompressed: '''.
	inflater do: [:each | Transcript nextPut:each].
	Transcript nextPutAll: ''''; cr.

	inflater close.
!

exampleDecompressingAString2
	"decompressing a String from a ByteArray and writing it to the Transcript.
	Similar to exampleDecompressingAString1 except that we use more of
	the <ReadStream> protocol.

		self exampleDecompressingAString2.
	"

	| compressedData inflater |

	compressedData :=  DeflaterWriteStream exampleCompressingAString.

	inflater := InflaterReadStream forTextOn: compressedData readStream.

	[inflater atEnd] whileFalse:
		[| word |
		word := inflater nextWord.
		Transcript display:word; cr].

	inflater close.
!

exampleDecompressingFromAStream1
	"decompression several lines of text which have been compressed
	'in-line' in an otherwise normal stream of bytes.

		self exampleDecompressingFromAStream1.
	"

	| inStream header inflater trailer |

	"this data should consist of an 9-byte 'header', followed by an unknown amount
	of compressed data, followed by an 9-byte trailer"
	inStream :=  DeflaterWriteStream exampleCompressingToAStream readStream.

	"first get the header"
	header := inStream next: 9.
	Transcript display: 'Header: '; display: header; cr.

	"open an InflaterReadStream on the input"
	inflater := InflaterReadStream forTextOn: inStream.

	"read lines until we read EOF"
	[inflater atEnd] whileFalse:
		[| line |
		line := inflater nextLine.
		Transcript nextPutAll: line; cr].

	"now the inflater will almost certainly have read beyond the compressed part of the
	input, so we have to adjust"
	inflater
		pushBackUnconsumedInput;
		close.

	"now we can read the original trailer from the input"
	trailer := inStream upToEnd.
	Transcript display: 'Trailer: '; display: trailer; cr.!

exampleDecompressingFromAStream2
	"decompression several lines of text which have been compressed
	'in-line' in an otherwise normal stream of bytes.
	This is the same as #exampleDecompressingFromAStream1 except that
	we use an unbuffered stream to do the decompression, rather than assuming
	that the underlying stream is positionable and fixing up the overun after
	decompression

		self exampleDecompressingFromAStream2.
	"

	| inStream header inflater trailer |

	"this data should consist of an 9-byte 'header', followed by an unknown amount
	of compressed data, followed by an 9-byte trailer"
	inStream :=  DeflaterWriteStream exampleCompressingToAStream readStream.

	"first get the header"
	header := inStream next: 9.
	Transcript display: 'Header: '; display: header; cr.

	"open an InflaterReadStream on the input.  In this example we ensure that
	it will not buffer more input that it will use, hence it cannot read over the end
	of the compressed data (but that will make it run more slowly"
	inflater := InflaterReadStream forTextOn: inStream.
	inflater unBufferInput.

	"read lines until we read EOF"
	[inflater atEnd] whileFalse:
		[| line |
		line := inflater nextLine.
		Transcript nextPutAll: line; cr].

	inflater close.

	"now we can read the original trailer from the input, since the inflater wasn't buffered it should not
	have consumed any of the data from the trailer"
	trailer := inStream upToEnd.
	Transcript display: 'Trailer: '; display: trailer; cr.!

file: aFilename text: aBool
	"answer a new instance that reads its compressed data from the given
	file, which is assumed to be in GZip format. This is only a convenience
	operation, so we don't bother with similar methods for the other formats
	which are rarely used to compress whole files (in any case you can change
	the format before reading if you wish)"

	^ (self new)
		inStream: (FileStream read: aFilename text: false);
		zlibFormat: #GZip;
		beText: aBool;
		yourself.
!

forBinaryOn: aBinaryReadStream
	"answer a new instance that will read data from aBinaryReadStream
	and decompress it.
	The answered instance will answer binary data (bytes) to #next and its family"

	^ (self new)
		beBinary;
		inStream: aBinaryReadStream;
		yourself.
!

forTextOn: aBinaryReadStream
	"answer a new instance that will read data from aBinaryReadStream
	and decompress it.
	The answered instance will answer text data (chars) to #next and its family"

	^ (self new)
		beText;
		inStream: aBinaryReadStream;
		yourself.
!

initialize
	"private -- class initialisation.

		self initialize.
	"

	SessionManager current when: #sessionStarted send: #onStartup to: self.
!

new
	"private -- use #on: or #for{Text/Binary}On: instead"

	^ (self basicNew)
		initialize;
		yourself.
		!

on: aBinaryReadStream
	"answer a new instance that will read data from aBinaryReadStream
	and decompress it.
	The answered instance will answer text data (chars) to #next and its family"

	^ self forTextOn: aBinaryReadStream.!

onStartup
	"private -- called as an image starts; ensure any instances can't be used
	in ways that would result in memory corruption"

	self allSubinstances do: [:each | each onStartup].

!

uninitialize
	"private -- class initialisation.

		self uninitialize.
	"

	SessionManager current removeEventsTriggeredFor: self.! !
!InflaterReadStream class categoriesFor: #defaultInBufferSize!constants!public! !
!InflaterReadStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!InflaterReadStream class categoriesFor: #defaultSkipBackLimit!constants!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingAByteArray!examples!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingAShortStringUsingADictionary!examples!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingAString1!examples!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingAString2!examples!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingFromAStream1!examples!public! !
!InflaterReadStream class categoriesFor: #exampleDecompressingFromAStream2!examples!public! !
!InflaterReadStream class categoriesFor: #file:text:!instance creation!public! !
!InflaterReadStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!InflaterReadStream class categoriesFor: #forTextOn:!instance creation!public! !
!InflaterReadStream class categoriesFor: #initialize!initializing!private! !
!InflaterReadStream class categoriesFor: #new!instance creation!private! !
!InflaterReadStream class categoriesFor: #on:!instance creation!public! !
!InflaterReadStream class categoriesFor: #onStartup!event handling!private! !
!InflaterReadStream class categoriesFor: #uninitialize!initializing!private! !

ZLibWriteStream guid: (GUID fromString: '{82256C13-7177-4430-B056-02141A3B10B3}')!
ZLibWriteStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Abstract superclass that collects commonality between {De/In}flaterWriteStream -- it is purely an implementation detail.

See the subclass comments, and class-side methods in the ''examples'' category.'!
!ZLibWriteStream categoriesForClass!Unclassified! !
!ZLibWriteStream methodsFor!

adlerChecksum
	"answer the Integer 'Adler' checksum of all the uncompressed data we have seen.
	Note that this is an error until after we have been #close-ed.
	Note also that the adler checksum is not maintained in #Raw mode"

	isClosed ifFalse: [self errorNotYetClosed].

	^ controlBlock adler.!

allocateBuffers
	"private -- allocate the working buffers and initialise the start/end pointers into them.
	Note that this is initially set up for binary uncompressed data"

	inBuffer := self makeBuffer: (self defaultInBufferSize) binary: true.
	self resetInBuffer.

	outBuffer := self makeBuffer: (self defaultOutBufferSize) binary: true.
	self resetOutBuffer.
!

basicBeBinary: aBool
	"private -- set whether we produce text or binary output / expect text or binary input"

	self subclassResponsibility.!

beBinary
	"set whether we produce text or binary output / expect text or binary input"

	self beBinary: true.!

beBinary: aBool
	"set whether we produce text or binary output / expect text or binary input
	This can be changed after the stream has been opened but in that case
	the advsiory 'text-or-binary' flag in the stream (if we are deflating) will be
	misleading"

	(self isBinary = aBool)
		ifFalse: [self basicBeBinary: aBool].!

beClosed
	"private -- record the fact that we are no longer open"

	"and we don't need to be finalisable either"
	self beUnfinalizable.

	isClosed := true.
	isOpen := false.!

beOpen
	"private -- record the fact that we are now open"

	"we now have to ensure that we'll clean up if necessary"
	self beFinalizable.

	isOpen := true.!

beRaw
	"set us to use zlib in a mode where no header or crc will be added to/expected in
	the input stream.
	Note that it is an error to call this after we have been #open-ed"

	self zlibFormat: #Raw.!

beText
	"set whether we produce text or binary output / expect text or binary input"

	self beBinary: false.!

beText: aBool
	"set whether we produce text or binary output / expect text or binary input"

	self beBinary: aBool not.!

checkStatus: anInteger
	"private -- if anInteger is less than Z_OK (which means an error) then trigger a zlib error.
	This is called immediately after most zlib operations"

	anInteger < Z_OK ifTrue: [self zlibError: anInteger].

	^ anInteger.!

close
	"complete the processes of (de)compressing our input data to the output
	stream, and release all allocated resources.
	Until this has been called, the output data is not valid, nor are the
	computed #adler and #crc checksums.
	NB: this neither #close-es nor #flush-es the output stream, you have to
	do that yourself if you are, say, writing to a file"

	isClosed ifTrue: [^ self].

	"if nothing has been written then we neep to open, compressing 0 bytes is *not* a nullop"
	isOpen ifFalse: [self open].

	"complete the zlib stuff"
	isOpen ifTrue: [self reallyClose].

	self beClosed.
!

closeAbruptly
	"private -- ensure that we have no further resources managed by ZLib.
	This is different from #discardZlibConnection in that it assumes that
	zlib is still valid and so has to be cleaned up, and from #close in that it
	doesn't attempt to flush any outstanding data or check for errors"

	isOpen ifTrue: [self reallyCloseAbruptly; beClosed].!

closeZLibStream
	"private -- close the zlib stream or throw an error"

	^ self checkStatus: (self closeZLibStreamAbruptly).
!

closeZLibStreamAbruptly
	"private -- close the zlib stream without any error checking"

	self subclassResponsibility.!

compressedSize
	"answer the compressed size of our data.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ self compressedSizeSoFar.!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ self subclassResponsibility.!

compressionRatio
	"answer the compression ratio we saw.
	Note that this is an error until after we have been #close-ed"

	^ self compressedSize / self uncompressedSize.!

compressionRatioSoFar
	"answer the compression ratio we are seeing to date.
	unlike #compressionRatio, this is can be called before we have been
	closed, but it is an approximation until that time since it doesn't take
	any account of data held in buffers"

	#CUtodo.  "we should probably at least #flushPartial here"

	^ self compressedSizeSoFar / self uncompressedSizeSoFar.
!

contents
	"answer the #contents of the stream that we've been writing to.
	NB1: this will #close the reciever if it is not closed already.
	NB2: this will break if our outstream doesn't understand #contents"

	^ self close; outStreamContents.!

controlBlock
	"private -- answer the ZLIBStream that holds the state of our processing"

	^ controlBlock.!

crcChecksum
	"answer the Integer CRC checksum of all the uncompressed data we have seen.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ crc.!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ self class defaultInBufferSize.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ self class defaultOutBufferSize.!

discardZlibConnection
	"private -- ensure that we will make no further use of resources managed by ZLib.
	This is different from #closeAbruptly in that it assumes that zlib is already
	dead"

	self isOpen ifTrue: [self beClosed].!

errorAlreadyClosed
	"private -- trigger a ZLibError to say that the stream has already been closed"

	^ ZLibError
		signal: 'already closed'
		with: Z_STREAM_ERROR.!

errorAlreadyOpen
	"private -- trigger a ZLibError to say that the stream has already been opened"

	^ ZLibError
		signal: 'already open'
		with: Z_STREAM_ERROR.!

errorNotPositionable: anInteger
	"private -- trigger an error because we cannot change position to anInteger"

	"this may be marginally more helpful than throwing a custom Error, and it's much easier ;-)"
	^ self errorSubscriptBounds: anInteger.
!

errorNotYetClosed
	"private -- trigger a ZLibError to say that the stream has not yet been closed"

	^ ZLibError
		signal: 'not yet closed'
		with: Z_STREAM_ERROR.!

finalize
	"we should ensure that we have released any resouces used
	internally by zlib"

	self closeAbruptly.!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	self subclassResponsibility.!

flushZLibStream: anInteger
	"private -- flush the compression stream to the given 'level' or throw an error.
	Answers the status (which may take several OK values)"

	self subclassResponsibility.
!

inBufferSize
	"answer the size of the buffer used to hold data before
	processing"

	^ inBuffer size.!

inBufferSize: anInteger
	"set the size of the buffer used for holding data before processsing.
	This influences the number of external calls to the zlib library, so
	a large buffer has less effect on efficiency than, for instance, a
	large buffer used for a FileStream.  However a moderately sized
	buffer does help.  The smallest legal value is 1.
	Note that it is an error to call this after we have been #open-ed"

	self assert: [anInteger >= 1].

	isOpen ifTrue: [^ self errorAlreadyOpen].

	inBuffer size = anInteger ifFalse:
		[inBuffer := inBuffer class newFixed: anInteger].

!

initialize
	"private -- establish a coherent initial state.
	Note that we initially assume we are going to have binary uncompressed data"

	controlBlock := ZLIBStream new.
	isOpen := isClosed := false.

	"see ZLib1Library>>inflateInitStream2:windowBits: and #deflateInit2Stream:level:method:windowBits:memLevel:strategy:"
	zlibWindowBits := 15.

	self allocateBuffers.!

initializeChecksums
	"private -- initialize the running checksums that we maintain"

	"the zlib library maintains the adler checksum for us, 
	but we have to do the crc32 ourselves"
	crc := self library initialCrc32.
!

isBinary
	"answer whether we producet text output / expect text input"

	self subclassResponsibility.!

isClosed
	"answer whether we have finished all the (de)compression we are going to do.
	NB: untill this returns true, the contents of the output stream are incomplete,
	and the values of #crc and #adler are undefined"

	^ isClosed.!

isOpen
	"answer whether we have started (de)compression yet.
	NB: this is largely meaningless since we normally
	'open' lazilly when we decide to flush our inBuffer the first time"

	^ isOpen.!

isText
	"answer whether we producet binary output / expect binary input"

	^ self isBinary not.!

library
	"private -- answer the external library that we use"

	^ ZLib1Library default.!

makeBuffer: anInteger binary: aBoolean
	"private -- allocate a buffer to use for holding char or byte data while the zlib library works on it"

	^ (aBoolean ifTrue: [ByteArray] ifFalse: [String])
		newFixed: anInteger.!

makeInBufferBinary: aBool
	"private -- convert the existing inBuffer to text/binary according as aBool"

	| new |

	inBuffer isNil ifFalse:
		[new := self makeBuffer: inBuffer size binary: aBool.
		inBuffer replaceBytesOf: new from: firstIn to: lastIn startingAt: firstIn.
		inBuffer := new].
!

makeOutBufferBinary: aBool
	"private -- convert the existing outBuffer to text/binary according as aBool"

	| new |

	outBuffer isNil ifFalse:
		[new := self makeBuffer: outBuffer size binary: aBool.
		outBuffer replaceBytesOf: new from: 1 to: lastOut startingAt: 1.
		outBuffer := new].
!

next: anInteger putAll: data startingAt: anIndex
	"overriden for improved performance (about 8x on a big collection).
	Also has the desirable side-effect of ensuring that we are #open-ed
	if someone writes an empty collection to this stream"

	"this implementation is such that data can be any byte object, which would be great except
	that #nextPut: will only accept Characters or Integers according as we are in text or binary
	mode"

	| from todo |

	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	from := anIndex.
	todo := anInteger.
	[todo > 0] whileTrue:
		[| chunk |
		chunk := todo min: (inBuffer size - lastIn).
		inBuffer replaceFrom: lastIn+1 to: lastIn+chunk with: data startingAt: from.
		lastIn := lastIn + chunk.
		from := from + chunk.
		todo := todo - chunk.
		lastIn >= inBuffer size ifTrue: [self flushInBuffer]].

	^ data.!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	isOpen ifFalse:
		[isClosed ifTrue: [^ self errorAlreadyClosed].
		self open].

	lastIn := lastIn + 1.
	inBuffer at: lastIn put: anObject.

	lastIn >= inBuffer size ifTrue: [self flushInBuffer].

	^ anObject.!

onStartup
	"private -- this is called at image startup, ensure that we do not attempt
	to use resources that were allocated by zlib in the previous session"

	self discardZlibConnection.!

open
	"initialize the underlying zlib library's data structures for processing
	our data.
	It is not normally necessary to call this explicitly since we #open
	lazily when we first need to flush our input data"

	isOpen ifFalse: [self reallyOpen; beOpen].!

openZLibStream
	"private -- open the zlib stream or throw an error"

	self subclassResponsibility.!

outBufferSize
	"answer the size of the buffer used to hold data after
	processing"

	^ outBuffer size.!

outBufferSize: anInteger
	"set the size of the buffer used for holding data after processsing.
	This influences the number of external calls to the zlib library, and
	also how efficiently we can 'drive' our outStream.  A large buffer
	has less effect on efficiency than, for instance, a large buffer used
	for a FileStream.  However a moderately sized buffer does help.
	The smallest legal value is 1.
	Note that it is an error to call this after we have been #open-ed"

	self assert: [anInteger >= 1].

	isOpen ifTrue: [^ self errorAlreadyOpen].

	outBuffer size = anInteger ifFalse:
		[outBuffer := outBuffer class newFixed: anInteger].

!

outStream
	"answer the stream to which we write processed data"

	^ outStream.!

outStream: aWriteStream
	"private -- set the stream to which we will write processed data"

	outStream := aWriteStream.!

outStreamContents
	"answer the #contents of the stream that we've been writing to.
	This is a convenience feature in that:
		a) it only works with Streams that *have* #contents!!
		b) it 'knows' that the contents are not valid until we have been closed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ self outStream contents.!

position
	"answer how many bytes have been written to this stream"

	^ controlBlock total_in + lastIn.!

position: anInteger
	"trigger an error because we are not positionable"

	^ self errorNotPositionable: anInteger.!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: (self isText ifTrue: [' forTextOn: '] ifFalse: [' forBinaryOn: ']);
		display: outStream.!

reallyClose
	"private -- complete the process of (de)compression"

	"get rid of any data that we are still waiting to process"
	self flushInBuffer.

	#CUtodo. "#flushInBuffer may close us as a side-effect!!"
	isClosed ifTrue: [^ self].

	"keep trying to 'FINISH' until zlib has flushed any data it holds internally"
	[| status |
	status := self flushZLibStream: Z_FINISH.
	self flushOutBuffer.
	status ~= Z_STREAM_END] whileTrue.

	"finally we can close the [de]compressor"
	self closeZLibStream.!

reallyCloseAbruptly
	"private -- ensure that we have no further resources managed by ZLib"

	self closeZLibStreamAbruptly.!

reallyOpen
	"private -- initialize the underlying zlib library's data structures for (de)compressing
	our data"

	self
		initializeChecksums;
		openZLibStream;
		resetInBuffer.
!

resetInBuffer
	"private -- start again at the begining of the in buffer"

	firstIn := 1.
	lastIn := 0.!

resetOutBuffer
	"private -- start again at the begining of the out buffer"

	lastOut := 0.!

uncompressedSize
	"answer the uncompressed size of our data.
	Note that this is an error until after we have been #close-ed"

	isClosed ifFalse: [self errorNotYetClosed].

	^ self uncompressedSizeSoFar.!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ self subclassResponsibility.!

updateControlBlock
	"private -- update our control block with our current buffers"

	controlBlock
		next_in: (inBuffer yourAddress + firstIn - 1);			"first byte that contains valid data"
		avail_in: (lastIn - firstIn + 1);					"number of valid bytes"
		next_out: (outBuffer yourAddress + lastOut);	"first byte that has not yet been written to"
		avail_out: (outBuffer size - lastOut);				"number of bytes before the end of the buffer"
		msg: nil.							"may as well reset this too"!

updateFromControlBlock
	"private -- update our current buffers with data from the control block"

	firstIn := controlBlock next_in asInteger - inBuffer yourAddress asInteger + 1.			"first byte that contains valid data"
	lastIn := firstIn + controlBlock avail_in - 1.													"last byte that contains valid data"
	lastOut := controlBlock next_out asInteger - outBuffer yourAddress asInteger.			"last byte that contains valid data"
!

windowBits
	"answer the log2 of the size of the (de)compression window we will use"

	^ zlibWindowBits < 0
		ifTrue: [0 - zlibWindowBits]
		ifFalse: [zlibWindowBits > 15
			ifTrue: [zlibWindowBits - 16]
			ifFalse: [zlibWindowBits]].!

windowBits: anInteger
	"set the log2 of the size of the window we will use for (de)compression.
	The value must lie in the range 8..15.
	Note that it is an error to call this after we have been #open-ed"

	self
		windowBits: anInteger
		zlibFormat: self zlibFormat.!

windowBits: anInteger zlibFormat: aSymbol
	"set the log2 of the size of the window we will use for (de)compression and the format
	according to which zlib format we expect to read/write.
	The integer must lie in the range 8..15.
	The symbol should be one of: #(#ZLib #GZip #Raw). see #zlibFormat for the meanings of these values.
	Note that it is an error to call this after we have been #open-ed"

	| newWindowBits |

	self assert: [(8 to: 15) includes: anInteger].
	self assert: [#(#ZLib #GZip #Raw) includes: aSymbol].

	isOpen ifTrue: [^ self errorAlreadyOpen].	#CUtodo.  "remove this restriction"

	newWindowBits := anInteger.
	aSymbol = #Raw ifTrue: [newWindowBits := 0 - newWindowBits].
	aSymbol = #GZip ifTrue: [newWindowBits := 16 + newWindowBits].

	zlibWindowBits := newWindowBits.!

windowSize
	"answer the size of the (de)compression window we will use"

	^ 1 bitShift: self windowBits.!

windowSize: anInteger
	"set the size of the window we will use for (de)compression.
	The value must lie in the range 256 ..32768 and will be rounded down to
	a power of 2.
	Note that it is an error to call this after we have been #open-ed"

	self windowBits: anInteger highBit - 1.!

zlibError: anInteger
	"private -- trigger a ZLibError with anInteger as its error tag, and the error message in our control block as the message"

	^ ZLibError
		signal: (controlBlock msg)
		with: anInteger.!

zlibFormat
	"answer one of:
		#(#ZLib #GZip #Raw)
	according to which zlib format we expect to read/write.

	#Raw is the underlying compressed format with no identifying header or checksum.
	It is used (for instance) in Zip files, and in other applications that define their own
	wrappers and sanity check externally to the zlib library.  This format is described in
	RFC 1951.

	#ZLib is the #Raw format with a 2-byte identifying header (#[120 156] I believe)
	and the Adler checksum added to the end (32bits in network byte order).  This format
	is described in RFC 1950.  It is also the format that we use by default.

	#GZlip is a simple implementation of the gzip format that is implemented directly by
	zlib, it includes/parses minimal header and crc data to interwork with programs using
	that format.  (The zlib library also includes more elaborate support for gzip format, but that
	only manipulates files, and is not exposed via these classes.)  The gzip format is described
	in RFC 1952.

	Note that, according to the zlib FAQ, in HTTP transfers,
			Content-Encoding: gzip
	means the #GZip format, and
			Content-Encoding: deflate
	means what we call #ZLib, however the FAQ also notes that several implementations
	(including Microsoft) wrongly use it to mean what we call #Raw.

	Also note that the zlib documentation implies that a #GZip compressed stream should be recognised
	and decoded automatically by a default inflater -- this appears not to be true, you have to tell the
	inflater to expect #GZip explicitly"

	"in point of fact the format is encoded in the window bits in a particularly hacky way..."
	^ zlibWindowBits < 0
		ifTrue: [#Raw]
		ifFalse: [zlibWindowBits > 15
			ifTrue: [#GZip]
			ifFalse: [#ZLib]].!

zlibFormat: aSymbol
	"set the format according to which zlib format we expect to read/write.  The symbol
	should be one of:
		#(#ZLib #GZip #Raw)
	See #zlibFormat for the meanings of these values.
	Note that it is an error to call this after we have been #open-ed"

	self
		windowBits: self windowBits
		zlibFormat: aSymbol.! !
!ZLibWriteStream categoriesFor: #adlerChecksum!accessing!public! !
!ZLibWriteStream categoriesFor: #allocateBuffers!helpers!private! !
!ZLibWriteStream categoriesFor: #basicBeBinary:!modes!private! !
!ZLibWriteStream categoriesFor: #beBinary!modes!public! !
!ZLibWriteStream categoriesFor: #beBinary:!modes!public! !
!ZLibWriteStream categoriesFor: #beClosed!modes!operations!private! !
!ZLibWriteStream categoriesFor: #beOpen!modes!operations!private! !
!ZLibWriteStream categoriesFor: #beRaw!initializing!modes!public! !
!ZLibWriteStream categoriesFor: #beText!modes!public! !
!ZLibWriteStream categoriesFor: #beText:!modes!public! !
!ZLibWriteStream categoriesFor: #checkStatus:!helpers!private! !
!ZLibWriteStream categoriesFor: #close!operations!public! !
!ZLibWriteStream categoriesFor: #closeAbruptly!operations!private! !
!ZLibWriteStream categoriesFor: #closeZLibStream!operations!private! !
!ZLibWriteStream categoriesFor: #closeZLibStreamAbruptly!operations!private! !
!ZLibWriteStream categoriesFor: #compressedSize!accessing!public! !
!ZLibWriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!ZLibWriteStream categoriesFor: #compressionRatio!accessing!public! !
!ZLibWriteStream categoriesFor: #compressionRatioSoFar!accessing!public! !
!ZLibWriteStream categoriesFor: #contents!accessing!public! !
!ZLibWriteStream categoriesFor: #controlBlock!accessing!private! !
!ZLibWriteStream categoriesFor: #crcChecksum!accessing!public! !
!ZLibWriteStream categoriesFor: #defaultInBufferSize!constants!public! !
!ZLibWriteStream categoriesFor: #defaultOutBufferSize!constants!public! !
!ZLibWriteStream categoriesFor: #discardZlibConnection!operations!private! !
!ZLibWriteStream categoriesFor: #errorAlreadyClosed!exceptions!modes!private! !
!ZLibWriteStream categoriesFor: #errorAlreadyOpen!exceptions!modes!private! !
!ZLibWriteStream categoriesFor: #errorNotPositionable:!exceptions!positioning!private! !
!ZLibWriteStream categoriesFor: #errorNotYetClosed!exceptions!private! !
!ZLibWriteStream categoriesFor: #finalize!finalizing!public! !
!ZLibWriteStream categoriesFor: #flushInBuffer!operations!private! !
!ZLibWriteStream categoriesFor: #flushZLibStream:!operations!private! !
!ZLibWriteStream categoriesFor: #inBufferSize!accessing!public! !
!ZLibWriteStream categoriesFor: #inBufferSize:!accessing!initializing!public! !
!ZLibWriteStream categoriesFor: #initialize!initializing!private! !
!ZLibWriteStream categoriesFor: #initializeChecksums!operations!private! !
!ZLibWriteStream categoriesFor: #isBinary!modes!public!testing! !
!ZLibWriteStream categoriesFor: #isClosed!modes!public!testing! !
!ZLibWriteStream categoriesFor: #isOpen!modes!public!testing! !
!ZLibWriteStream categoriesFor: #isText!modes!public!testing! !
!ZLibWriteStream categoriesFor: #library!helpers!private! !
!ZLibWriteStream categoriesFor: #makeBuffer:binary:!helpers!private! !
!ZLibWriteStream categoriesFor: #makeInBufferBinary:!helpers!private! !
!ZLibWriteStream categoriesFor: #makeOutBufferBinary:!helpers!private! !
!ZLibWriteStream categoriesFor: #next:putAll:startingAt:!public!writing! !
!ZLibWriteStream categoriesFor: #nextPut:!public!writing! !
!ZLibWriteStream categoriesFor: #onStartup!event handling!private! !
!ZLibWriteStream categoriesFor: #open!operations!public! !
!ZLibWriteStream categoriesFor: #openZLibStream!operations!private! !
!ZLibWriteStream categoriesFor: #outBufferSize!accessing!public! !
!ZLibWriteStream categoriesFor: #outBufferSize:!accessing!initializing!public! !
!ZLibWriteStream categoriesFor: #outStream!accessing!public! !
!ZLibWriteStream categoriesFor: #outStream:!initializing!private! !
!ZLibWriteStream categoriesFor: #outStreamContents!accessing!public! !
!ZLibWriteStream categoriesFor: #position!accessing!positioning!public! !
!ZLibWriteStream categoriesFor: #position:!positioning!public! !
!ZLibWriteStream categoriesFor: #printOn:!printing!public! !
!ZLibWriteStream categoriesFor: #reallyClose!operations!private! !
!ZLibWriteStream categoriesFor: #reallyCloseAbruptly!operations!private! !
!ZLibWriteStream categoriesFor: #reallyOpen!operations!private! !
!ZLibWriteStream categoriesFor: #resetInBuffer!helpers!private! !
!ZLibWriteStream categoriesFor: #resetOutBuffer!helpers!private! !
!ZLibWriteStream categoriesFor: #uncompressedSize!accessing!public! !
!ZLibWriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!ZLibWriteStream categoriesFor: #updateControlBlock!helpers!private! !
!ZLibWriteStream categoriesFor: #updateFromControlBlock!helpers!private! !
!ZLibWriteStream categoriesFor: #windowBits!accessing!public! !
!ZLibWriteStream categoriesFor: #windowBits:!initializing!modes!public! !
!ZLibWriteStream categoriesFor: #windowBits:zlibFormat:!initializing!modes!public! !
!ZLibWriteStream categoriesFor: #windowSize!accessing!public! !
!ZLibWriteStream categoriesFor: #windowSize:!initializing!modes!public! !
!ZLibWriteStream categoriesFor: #zlibError:!exceptions!private! !
!ZLibWriteStream categoriesFor: #zlibFormat!accessing!modes!public! !
!ZLibWriteStream categoriesFor: #zlibFormat:!initializing!modes!public! !

!ZLibWriteStream class methodsFor!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is processed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ 1024.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is processed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	^ 1024.!

initialize
	"private -- class initialisation.

		self initialize.
	"

	SessionManager current when: #sessionStarted send: #onStartup to: self.
!

new
	"private -- used by subclasses only"

	^ (self basicNew)
		initialize;
		yourself.
		!

onStartup
	"private -- called as an image starts; ensure any instances can't be used
	in ways that would result in memory corruption"

	self allSubinstances do: [:each | each onStartup].

!

uninitialize
	"private -- class initialisation.

		self uninitialize.
	"

	SessionManager current removeEventsTriggeredFor: self.! !
!ZLibWriteStream class categoriesFor: #defaultInBufferSize!constants!public! !
!ZLibWriteStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!ZLibWriteStream class categoriesFor: #initialize!initializing!private! !
!ZLibWriteStream class categoriesFor: #new!instance creation!private! !
!ZLibWriteStream class categoriesFor: #onStartup!event handling!private! !
!ZLibWriteStream class categoriesFor: #uninitialize!initializing!private! !

DeflaterWriteStream guid: (GUID fromString: '{C5B7234D-D941-44B9-ABBE-9BD428022F06}')!
DeflaterWriteStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these wraps a (binary) WriteStream and arranges that any data writen to itself will be compressed with the zlib library, and the compressed data sent onwards to the output stream.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

The compressor can be set to write in any of the three modes supported by zlib -- zlib (=''deflate''), gzip, and raw.  These differ only in the kinds of header/trailer attatched to the compressed data.  See the comment on ZLibWriteStream>>zlibFormat: for more information.

You can set options to control the compression, in general this version of the class only supports setting the options before the stream is opened (or first written-to), although a later version may remove some of the restrictions.  A later version may also support ''resetting'' the compressor state in mid-stream, and re-synchronising after data errors -- this version doesn''t.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to acquire and release resource in the ZLIB library.  (They do close themselves by finalisation, if necessary, but don''t rely on that producing correct results, since data is not necessarily flushed through to the underlying stream until the #close).

Note that these streams close themselves on image startup.

Also see the class-side methods in the ''examples'' category.
'!
!DeflaterWriteStream categoriesForClass!Unclassified! !
!DeflaterWriteStream methodsFor!

basicBeBinary: aBool
	"private -- set whether we expect text or binary input.
	This can be changed after the stream has been opened but in that case
	the advsiory 'text-or-binary' flag in the stream will be misleading"

	controlBlock data_type: (aBool  ifTrue: [Z_BINARY] ifFalse: [Z_ASCII]).

	self makeInBufferBinary: aBool.!

closeZLibStreamAbruptly
	"private -- close the zlib stream without any error checking"

	^ self library deflateEndStream: controlBlock.
!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_out.!

compressionLevel
	"answer which one of the Z_XXX_COMPRESSION compression levels we have been
	configured to use.  Values are:
		0 for no compression.
		-1 for default compression level.
		1 for lowest (but fastest) compression up to:
		...
		9  for highest (but slowest) compression"

	^ compressionLevel.!

compressionLevel: anInteger
	"set which one of the Z_XXX_COMPRESSION compression levels we have been
	configured to use.  Values are:
		0 for no compression.
		-1 for default compression level.
		1 for lowest (but fastest) compression up to:
		...
		9  for highest (but slowest) compression.
	Note that it is an error to call this after we have been #open-ed"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	compressionLevel := anInteger.!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	"add all the available data to the crc"
	crc := self library
			crc32: crc
			buffer: inBuffer yourAddress + firstIn - 1
			size: lastIn - firstIn + 1.

	"keep compressing stuff untill there's nothing left to do.
	Note that we flush the output buffer completely each time around
	which may be a little inefficient, but it does make the logic slightly
	easier since we never have to worry that the out buffer may be
	full before we do the flush"
	[lastIn >= firstIn] whileTrue:
				[self
					flushZLibStream: Z_NO_FLUSH;
					flushOutBuffer].

	"start again at the begining of the buffer"
	self resetInBuffer.
!

flushOutBuffer
	"private -- ensure that we are holding no data in our outBuffer"

	"the way that we do things means that the valid data in the outBuffer
	always starts at the begining of the buffer"
	outStream
		next: lastOut
		putAll: outBuffer
		startingAt: 1.

	"start again at the begining of the buffer"
	self resetOutBuffer.!

flushZLibStream: anInteger
	"private -- flush the compression stream to the given 'level' or throw an error.
	Answers the status (which may take several OK values)"

	| status |

	self updateControlBlock.
	status := self checkStatus: (self library deflateStream: controlBlock flush: anInteger).
	self updateFromControlBlock.

	^ status.!

initialize
	"private -- establish a coherent initial state"

	super initialize.

	"see ZLib1Library>>deflateInit2Stream:level:method:windowBits:memLevel:strategy:"
	compressionLevel := Z_DEFAULT_COMPRESSION.
	zlibMemLevel := 8.
	zlibStrategy := Z_DEFAULT_STRATEGY.

!

isBinary
	"answer whether we expect binary input"

	^ controlBlock data_type = Z_BINARY.!

openZLibStream
	"private -- open the zlib stream or throw an error"

	| status |

	status := self library
			deflateInit2Stream: controlBlock
			level: compressionLevel
			method: Z_DEFLATED	"no choice"
			windowBits: zlibWindowBits
			memLevel: zlibMemLevel
			strategy: zlibStrategy.
	self checkStatus: status.

	"have we been asked to use a compression dictionary ?"
	dictionary isNil ifFalse:
		[self checkStatus:
			(self library
				deflateSetDictionary: controlBlock
				dictionary: dictionary
				dictLength: dictionary size)].

	^ status.!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_in.!

useBestCompression
	"set ourselves to use the best (but slowest) compression.
	Note that this has no effect after we have been #opened"

	self compressionLevel: Z_BEST_COMPRESSION.!

useBestSpeed
	"set ourselves to use the fastest (but worst) compression.
	Note that this has no effect after we have been #opened"

	self compressionLevel: Z_BEST_SPEED.!

useCompressionDictionary: aStringOrByteArray
	"this tells zlib to pre-load its compressor by analysing the given text (the word 'dictionary' here
	is a bit misleading, but that's what zlib calls it).  If you do that, then any decmpressor will
	have to supply an identical dictionary as part of decompression (it actually throws a 
	ZLibDictionaryNeededException).
	You can use more than one dictionary in an application -- they are identified to the app
	by the adler checksum of the data, this method answers that cfhecksum.  Also the
	corresponding ZLibDictionaryNeededException will have the same checksum as its
	tag.
	This feature is very rarly used, but it can be usefull for improving the compression of
	short texts (especially) when you know what the typicall text to compress looks like in
	advance.  See ZLib1Library>>deflateSetDictionary:dictionary:dictLength: for
	more detailed discussion.

	Note that it is an error to call this after we have been #open-ed"

	#CUtodo.  "the zlib documentation hints that the adler may not be recorded for #Raw format"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	dictionary := aStringOrByteArray.

	^ self library adler32: dictionary.
!

useNoCompression
	"set ourselves to use no compression at all.
	Note that this has no effect after we have been #opened"

	self compressionLevel: Z_NO_COMPRESSION.! !
!DeflaterWriteStream categoriesFor: #basicBeBinary:!modes!private! !
!DeflaterWriteStream categoriesFor: #closeZLibStreamAbruptly!operations!private! !
!DeflaterWriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!DeflaterWriteStream categoriesFor: #compressionLevel!accessing!public! !
!DeflaterWriteStream categoriesFor: #compressionLevel:!initializing!modes!public! !
!DeflaterWriteStream categoriesFor: #flushInBuffer!operations!private! !
!DeflaterWriteStream categoriesFor: #flushOutBuffer!operations!private! !
!DeflaterWriteStream categoriesFor: #flushZLibStream:!operations!private! !
!DeflaterWriteStream categoriesFor: #initialize!initializing!private! !
!DeflaterWriteStream categoriesFor: #isBinary!modes!public!testing! !
!DeflaterWriteStream categoriesFor: #openZLibStream!operations!private! !
!DeflaterWriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!DeflaterWriteStream categoriesFor: #useBestCompression!initializing!modes!public! !
!DeflaterWriteStream categoriesFor: #useBestSpeed!initializing!modes!public! !
!DeflaterWriteStream categoriesFor: #useCompressionDictionary:!initializing!public! !
!DeflaterWriteStream categoriesFor: #useNoCompression!initializing!modes!public! !

!DeflaterWriteStream class methodsFor!

compressBinary: aByteArray
	"answer a ByteArray containing the result of compressing the given ByteArray at the
	default compression ratio"

	^ (self forBinary)
		nextPutAll: aByteArray;
		contents.!

compressBinary: aByteArray compressionLevel: anInteger
	"answer a ByteArray containing the result of compressing the given ByteArray.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ (self forBinaryCompressionLevel: anInteger)
		nextPutAll: aByteArray;
		contents.!

compressText: aString
	"answer a ByteArray containing the result of compressing the given string at the
	default compression ratio"

	^ (self forText)
		nextPutAll: aString;
		contents.!

compressText: aString compressionLevel: anInteger
	"answer a ByteArray containing the result of compressing the given String.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"


	^ (self forTextCompressionLevel: anInteger)
		nextPutAll: aString;
		contents.!

exampleCompressingAByteArray
	"compressing a ByteArray into another ByteArray using all the defaults.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAByteArray.
	"

	"you may find it instructive to trace through this since it uses more of the public API"
	^ DeflaterWriteStream compressBinary: #(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 0).!

exampleCompressingAShortStringUsingADictionary
	"compressing a short string more effectively by pre-conditioning the compressor
	with a fixed, known, 'dictionary' of sample text.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAShortStringUsingADictionary.

	Try comparing the efficiencies by 'doing a printIt on these expressions -- they all comrpess
	the same boring text:
		self exampleCompressingAString size.
		self exampleCompressingAStringMax size.
		self exampleCompressingAShortStringUsingADictionary size.
	"

	| dictionary deflater |

	"the 'dictionary' is just a sample text that is likely to by similar to the target text"
	dictionary := 'The cat sat on the mat sat on the rat sat on the cat sat on the mat'.

	"create a deflater write stream that will build a ByteArray using an internal WriteStream"
	deflater := DeflaterWriteStream forText.

	"tell it to precondition itself from the sample text.  We may as well tell it to use maximum
	coimpression too.  Note that this settup must all happen before the first write to the stream"
	deflater
		useCompressionDictionary: dictionary;
		compressionLevel:  Z_BEST_COMPRESSION.

	"add our short text (which is the same as in the #exampleCompressingAString* examples)"
	deflater nextPutAll: 'The cat sat on the mat'.

	"get the compressed text back.  Note that this implicitly closes the stream"
	^ deflater contents.
!

exampleCompressingAString
	"compressing a string into a ByteArray using all the defaults.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAString.
	"

	"you may find it instructive to trace through this since it uses more of
	the public API"
	^ DeflaterWriteStream compressText: 'The cat sat on the mat'.!

exampleCompressingAStringWithMaxCompression
	"compressing a string into a ByteArray using the maximum compression level.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAStringWithMaxCompression.
	"

	^ DeflaterWriteStream
		compressText: 'The cat sat on the mat'
		compressionLevel:  Z_BEST_COMPRESSION.		"i.e 9"!

exampleCompressingToAStream
	"compressing several lines of text using the deflater explicitly as a Stream
	which writes to an existing stream.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingToAStream.
	"

	| stream deflater |

	"set up the stream that we'll add to, put a 'header' on it"
	stream := ByteArray writeStream.
	stream nextPutAll: #[0 1 2 3 4 5 6 7 9].

	"create a deflater that will append to the existing stream"
	deflater := DeflaterWriteStream forTextOn: stream.

	"write some lines of text to it"
	deflater
		nextPutAll: 'The rat sat on the mat'; cr;
		nextPutAll: 'The cat sat on the rat'; cr;
		nextPutAll: 'Which was unlucky for the rat'; cr.

	"now we *must* close the deflater of the compressed data will not be flushed
	through the zlib library and into our orginal stream"
	deflater close.

	"put a trailer after the compressed data in our original stream"
	stream nextPutAll: #[8 7 6 5 4 3 2 1 0].

	"if you inspect the result you'll see the header and trailer bracketing the compressed data"
	^ stream contents.!

exampleCompressingWithAStream
	"compressing several lines of text using the deflater explicitly as a Stream.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingWithAStream.
	"

	| deflater |

	"create a deflater write stream that will build a ByteArray using an internal WriteStream"
	deflater := DeflaterWriteStream forText.

	"write some lines of text to it"
	deflater
		nextPutAll: 'The rat sat on the mat'; cr;
		nextPutAll: 'The cat sat on the rat'; cr;
		nextPutAll: 'Which was unlucky for the rat'; cr.

	"get the compressed text back.  Note that this implicitly closes the stream"
	^ deflater contents.!

file: aFilename text: aBool
	"answer a new instance that writes compressed data to the given
	file in GZip format (a .gz file extension is conventional).
	This is only a convenience operation, so we don't
	bother with similar methods for the other formats which are rarely
	used to compress whole files, in any case you can change the
	format before writing anything if you wish"

	^ (self new)
		outStream: (FileStream write: aFilename text: false);
		zlibFormat: #GZip;
		beText: aBool;
		yourself.
!

forBinary
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to its internal write stream.
	This will use the default compression level"

	^ self forBinaryOn: ByteArray writeStream.!

forBinaryCompressionLevel: anInteger
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to its internal write stream.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ self forBinaryOn: ByteArray writeStream compressionLevel: anInteger.
		!

forBinaryOn: aBinaryWriteStream
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default compression level"

	^ (self new)
		beBinary;
		outStream: aBinaryWriteStream;
		yourself.
!

forBinaryOn: aBinaryWriteStream compressionLevel: anInteger
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ (self new)
		beBinary;
		compressionLevel: anInteger;
		outStream: aBinaryWriteStream;
		yourself.
		!

forText
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to its internal write stream.
	This will use the default compression level"

	^ self forTextOn: ByteArray writeStream.!

forTextCompressionLevel: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to its internal write stream.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ self forTextOn: ByteArray writeStream compressionLevel: anInteger
!

forTextOn: aBinaryWriteStream
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default compression level"

	^ (self new)
		beText;
		outStream: aBinaryWriteStream;
		yourself.
		!

forTextOn: aBinaryWriteStream compressionLevel: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ (self new)
		beText;
		compressionLevel: anInteger;
		outStream: aBinaryWriteStream;
		yourself.
		!

on: aBinaryWriteStream
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default compression level"

	^ self forTextOn: aBinaryWriteStream.
		!

on: aBinaryWriteStream compressionLevel: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The compression level is set by anInteger and should be a value from
	0 to 9 inclusive or one of the values from ZLib1Constants"

	^ self forTextOn: aBinaryWriteStream compressionLevel: anInteger.
		! !
!DeflaterWriteStream class categoriesFor: #compressBinary:!compressing!public! !
!DeflaterWriteStream class categoriesFor: #compressBinary:compressionLevel:!compressing!public! !
!DeflaterWriteStream class categoriesFor: #compressText:!compressing!public! !
!DeflaterWriteStream class categoriesFor: #compressText:compressionLevel:!compressing!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingAByteArray!examples!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingAShortStringUsingADictionary!examples!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingAString!examples!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingAStringWithMaxCompression!examples!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingToAStream!examples!public! !
!DeflaterWriteStream class categoriesFor: #exampleCompressingWithAStream!examples!public! !
!DeflaterWriteStream class categoriesFor: #file:text:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forBinary!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forBinaryCompressionLevel:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forBinaryOn:compressionLevel:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forText!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forTextCompressionLevel:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forTextOn:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #forTextOn:compressionLevel:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #on:!instance creation!public! !
!DeflaterWriteStream class categoriesFor: #on:compressionLevel:!instance creation!public! !

InflaterWriteStream guid: (GUID fromString: '{75FB9665-0D50-4C26-860B-8EFF0A9D9B2F}')!
InflaterWriteStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these wraps a WriteStream and arranges that any data writen to itself will be decompressed with the zlib library, and the decompressed data sent onwards to the output stream.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

NB: For most purposes InflaterReadStream is a better bet for the task of decompressing data, however this version is occasionally usefull (and quite a bit faster), so I''ve left it in.

The decompressor can be set to read any of the three modes supported by zlib -- zlib (=''deflate''), gzip, and raw.  These differ only in the kinds of header/trailer attatched to the compressed data.  See the comment on ZLibWriteStream>>zlibFormat: for more information.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to acquire and release resource in the ZLIB library.  (They do close themselves by finalisation, if necessary, but don''t rely on that producing correct results, since data is not necessarily flushed through to the underlying stream until the #close).

Note that these streams close themselves on image startup.

Also see the class-side methods in the ''examples'' category.
'!
!InflaterWriteStream categoriesForClass!Unclassified! !
!InflaterWriteStream methodsFor!

basicBeBinary: aBool
	"private -- set whether we produce text or binary output"

	self makeOutBufferBinary: aBool.
!

closeZLibStreamAbruptly
	"private -- close the zlib stream without any error checking"

	^ self library inflateEndStream: controlBlock.
!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_in.
!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	| status |

	"keep decompressing stuff until there's nothing left in the buffer or until
	zlib has discovered end-of-stream on the input.
	We will keep pushing data into zlib until EOS at which point we close
	the stream and return.
	Note that we flush the output buffer completely each time around
	which may be a little inefficient, but it does make the logic slightly
	easier since we never have to worry that the out buffer may be
	full before calling zlib"
	[firstIn <= lastIn] whileTrue:
		[status := self flushZLibStream: Z_NO_FLUSH.
		self flushOutBuffer.
		status = Z_NEED_DICT ifTrue: [self requestDecompressionDictionary].
		status = Z_STREAM_END ifTrue: [self closeZLibStream; beClosed. ^ self]].

	"start again at the begining of the in buffer"
	self resetInBuffer.

!

flushOutBuffer
	"private -- ensure that we are holding no data in our outBuffer"

	"add all the available data to the crc"
	crc := self library
			crc32: crc
			buffer: outBuffer yourAddress
			size: lastOut.

	"the way that we do things means that the valid data in the outBuffer
	always starts at the begining of the buffer"
	outStream
		next: lastOut
		putAll: outBuffer
		startingAt: 1.

	"start again at the begining of the buffer"
	self resetOutBuffer.!

flushZLibStream: anInteger
	"private -- flush the compression stream to the given 'level' or throw an error.
	Answers the status (which may take several OK values)"

	| status |

	self updateControlBlock.
	status := self checkStatus: (self library inflateStream: controlBlock flush: anInteger).
	self updateFromControlBlock.

	^ status.!

isBinary
	"answer whether we producet binary output"

	^ outBuffer isKindOf: ByteArray.!

openZLibStream
	"private -- open the zlib stream or throw an error"

	^ self checkStatus: (self library inflateInit2Stream: controlBlock windowBits: zlibWindowBits).!

requestDecompressionDictionary
	"private -- trigger a *resumable* error stating that zlib cannor carry on without
	being given a decompression dictionay.  The tag of the exception is the adler
	checksum that zlib uses to 'identify' which dictionary it needs.
	If you want to handle this, then you should #resume: the exeption with the correct
	dictionary.
	The word 'dictionary' here is a bit misleading, all the library really wants is
	some example text"

	| dictionary |

	dictionary := ZLibDictionaryNeededException signalWith: (controlBlock adler).

	self checkStatus: (self library
				inflateSetDictionary: controlBlock
				dictionary: dictionary yourAddress
				dictLength: dictionary size).
!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_out.
! !
!InflaterWriteStream categoriesFor: #basicBeBinary:!modes!private! !
!InflaterWriteStream categoriesFor: #closeZLibStreamAbruptly!operations!private! !
!InflaterWriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!InflaterWriteStream categoriesFor: #flushInBuffer!operations!private! !
!InflaterWriteStream categoriesFor: #flushOutBuffer!operations!private! !
!InflaterWriteStream categoriesFor: #flushZLibStream:!operations!private! !
!InflaterWriteStream categoriesFor: #isBinary!modes!public!testing! !
!InflaterWriteStream categoriesFor: #openZLibStream!operations!private! !
!InflaterWriteStream categoriesFor: #requestDecompressionDictionary!exceptions!helpers!private! !
!InflaterWriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !

!InflaterWriteStream class methodsFor!

decompressBinary: aByteArray
	"answer a ByteArray containing the result of decompressing the given ByteArray"

	^ (self forBinaryOn: ByteArray writeStream)
		nextPutAll: aByteArray;
		contents.!

decompressBinary: aByteArray zlibFormat: aSymbol
	"answer a ByteArray containing the result of decompressing the given ByteArray, which is
	assumed to have been compressed with the given format (see instance-size #zlibFormat	
	for details)"

	^ (self forBinaryOn: ByteArray writeStream)
		zlibFormat: aSymbol;
		nextPutAll: aByteArray;
		contents.!

decompressText: aByteArray
	"answer a String containing the result of decompressing the given ByteArray"

	^ (self forTextOn: String writeStream)
		nextPutAll: aByteArray;
		contents.!

decompressText: aByteArray zlibFormat: aSymbol
	"answer a String containing the result of decompressing the given ByteArray, which is
	assumed to have been compressed with the given format (see instance-size #zlibFormat	
	for details)"

	^ (self forTextOn: String writeStream)
		zlibFormat: aSymbol;
		nextPutAll: aByteArray;
		contents.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is processed.
	Since this is only used to reduce the number of calls to the zlib library
	there is no great pressure to use a large buffer"

	"since we are expecting more output than input"
	^ 2 * super defaultOutBufferSize.!

exampleDecompressingAByteArray
	"decompressing a ByteArray into another ByteArray

		self exampleDecompressingAByteArray.
	"

	| compressedData |

	compressedData :=  DeflaterWriteStream exampleCompressingAByteArray.

	"you may find it instructive to trace through this since it uses more of
	the public API"
	^ InflaterWriteStream decompressBinary: compressedData.!

exampleDecompressingAShortStringUsingADictionary
	"decompressing text that has been compressed more effectively by pre-conditioning the compressor
	with a fixed, known, 'dictionary' of sample text.

		self exampleDecompressingAShortStringUsingADictionary.

	"

	| dictionary compressedData |

	"we have to be able to provide the inflater with the same example text as was
	used to compress it.  To keep it easy, we just hardwire that text here"
	dictionary := 'The cat sat on the mat sat on the rat sat on the cat sat on the mat'.

	"this is data compressed using the same dictionary"
	compressedData :=  DeflaterWriteStream exampleCompressingAShortStringUsingADictionary.

	"try to decompress the data, at some point (determined by zlib) it will throw a
	resumable ZLibDictionaryNeededException, which we -- if we wish to continue -- must
	resume with the 'disctionary' text"
	[^ InflaterWriteStream decompressText: compressedData]
		on: ZLibDictionaryNeededException 
		do: [:ex | ex resume: dictionary].
!

exampleDecompressingAString
	"decompressing a string from a ByteArray.

		self exampleDecompressingAString.
	"

	| compressedData |

	compressedData :=  DeflaterWriteStream exampleCompressingAString.

	"you may find it instructive to trace through this, since it uses more of
	the public API"
	^ InflaterWriteStream decompressText: compressedData.!

forBinary
	"answer a new instance that will decompress the data that is written to it,
	and forward the uncompressed bytes to its internal write stream"

	^ self forBinaryOn: ByteArray writeStream.!

forBinaryOn: aBinaryWriteStream
	"answer a new instance that will decompress the data that is written to it,
	and forward the uncompressed bytes to aBinaryWriteStream"

	^ (self new)
		beBinary;
		outStream: aBinaryWriteStream;
		yourself.
		!

forText
	"answer a new instance that will decompress the data that is written to it,
	and forward the uncompressed characters to its internal write stream"

	^ self forTextOn: String writeStream.!

forTextOn: aTextWriteStream
	"answer a new instance that will decompress the data that is written to it,
	and forward the uncompressed characters to aTextWriteStream"

	^ (self new)
		beText;
		outStream: aTextWriteStream;
		yourself.
		!

on: aTextWriteStream
	"answer a new instance that will decompress the data that is written to it,
	and forward the uncompressed characters to aTextWriteStream"

	^ self forTextOn: aTextWriteStream.! !
!InflaterWriteStream class categoriesFor: #decompressBinary:!decompressing!public! !
!InflaterWriteStream class categoriesFor: #decompressBinary:zlibFormat:!decompressing!public! !
!InflaterWriteStream class categoriesFor: #decompressText:!decompressing!public! !
!InflaterWriteStream class categoriesFor: #decompressText:zlibFormat:!decompressing!public! !
!InflaterWriteStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!InflaterWriteStream class categoriesFor: #exampleDecompressingAByteArray!examples!public! !
!InflaterWriteStream class categoriesFor: #exampleDecompressingAShortStringUsingADictionary!examples!public! !
!InflaterWriteStream class categoriesFor: #exampleDecompressingAString!examples!public! !
!InflaterWriteStream class categoriesFor: #forBinary!instance creation!public! !
!InflaterWriteStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!InflaterWriteStream class categoriesFor: #forText!instance creation!public! !
!InflaterWriteStream class categoriesFor: #forTextOn:!instance creation!public! !
!InflaterWriteStream class categoriesFor: #on:!instance creation!public! !

"Binary Globals"!

"Resources"!

