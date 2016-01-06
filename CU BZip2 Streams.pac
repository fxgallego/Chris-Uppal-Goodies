| package |
package := Package name: 'CU BZip2 Streams'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Three Stream classes built on top of bzip2 compression.

Two are write streams. One, BZip2CompressingWriteStream, that compresses any data that is written to it.  The other, BZip2DecompressingWriteStream, decompresses anything that is written to it.  In both cases the processed data is then written to an underlying WriteStream.

Also includes a decompressing read stream, BZip2DecompressingReadStream.  That wraps an existing ReadStream onto a source of compressed data, and decompresses it as you read.  Also includes a limited abillity to "push back" data, so as to make (for instance) splitting the data into lines easier.

There is no corresponding BZip2CompressingReadStream because I can''t think of a use for it.

See the class comments for more information.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

2.00
-	Added BZip2{CompressingWrite/DecompressingRead}Stream class>>file:text:.
-	More efficient implementation of BZip2DecompressingReadStream>>skip:

1.00
-	First release.

'.

package basicPackageVersion: '2.01'.


package classNames
	add: #BZip2CompressingWriteStream;
	add: #BZip2DecompressingReadStream;
	add: #BZip2DecompressingWriteStream;
	add: #BZip2WriteStream;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	add: 'CU BZip2 Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

AbstractReadStream subclass: #BZip2DecompressingReadStream
	instanceVariableNames: 'controlBlock inBuffer firstIn lastIn outBuffer nextOut lastOut inStream isOpen isClosed useSmallDecompressor atEnd skipBackLimit'
	classVariableNames: ''
	poolDictionaries: 'BZip2Constants'
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #BZip2WriteStream
	instanceVariableNames: 'controlBlock inBuffer firstIn lastIn outBuffer lastOut outStream isOpen isClosed'
	classVariableNames: ''
	poolDictionaries: 'BZip2Constants'
	classInstanceVariableNames: ''!
BZip2WriteStream subclass: #BZip2CompressingWriteStream
	instanceVariableNames: 'blockSize workFactor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BZip2WriteStream subclass: #BZip2DecompressingWriteStream
	instanceVariableNames: 'useSmallDecompressor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BZip2DecompressingReadStream guid: (GUID fromString: '{608F03E1-6975-4A05-8516-0C51C8A450A2}')!
BZip2DecompressingReadStream comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

One of these wraps a (binary) ReadStream; it reads data from the input stream and uses the bzip2 library to decompress it, then makes the decompressed data available via the usual read stream protocol.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

The stream maintains a limited buffer to allow it to be positioned backwards.  By default it can skip back over the previously read object -- i.e. a #skipBackLimit of 1.  The limit: can be changed *before any data is read* to a higher value or to zero.  The bigger the skip back buffer, the less efficient reading will be.

Note that *arbitrary* positioning is not supported -- that would require buffering the entire output, and if you are going to do that then you may as well just read the whole thing into a String/ByteArray and use that directly.  That means that although methods that use limited lookahead (e.g: #nextLine) will work OK, methods like #contents which jump about arbitrarily will not work.  Methods that don''t use positioning, such as #upToEnd, are fine, of course, as are methods that only skip forwards (like #setToEnd).

Since we don''t know the size of the uncompressed data until we have decompressed it, #size isn''t supported either.

Since the decompressor is reading from the underlying stream into its own (and bzip2''s) buffers, there''s a danger that it can overrun the limits of the compressed part of a stream.  If that matters to you, then you can either turn off input buffering, at some cost in execution time, by sending #unBufferInput before you start reading; or (if the underlying stream is positionable) use #pushBackUnconsumedInput: after you''ve finished reading.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to acquire and release resource in the BZIP2 library.  (They do close themselves by finalisation, if necessary).

Also see the class-side methods in the ''examples'' category.
'!
!BZip2DecompressingReadStream categoriesForClass!Unclassified! !
!BZip2DecompressingReadStream methodsFor!

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

beText
	"set whether we produce text or binary output"

	self beBinary: false.!

beText: aBool
	"set whether we produce text or binary output"

	self beBinary: aBool not.!

close
	"complete or abandon the processes of decompressing our input data, and
	release all allocated resources.
	NB: this neither #close-s nor #flush-es the output stream, you have to
	do that yourself if you are, say, writing to a file"

	isClosed ifTrue: [^ self].

	"complete the bzip2 stuff.
	if nothing has been written then that is an error (since 0 bytes is not valid
	compressed data), but there seems little point in worrying about it"
	isOpen ifTrue: [self reallyClose].

	self beClosed.
!

closeAbruptly
	"private -- ensure that we have no further resources managed by BZip2.
	This is different from #discardBZip2Connection in that it assumes that
	bzip2 is still valid and so has to be cleaned up, and from #close in that it
	doesn't attempt to flush any outstanding data or check for errors"

	isOpen ifTrue: [self reallyCloseAbruptly; beClosed].!

closeBZip2Stream
	"private -- close the bzip2 stream or throw an error"

	^ self library decompressEnd: controlBlock.
!

closeBZip2StreamAbruptly
	"private -- close the bzip2 stream without any error checking"

	"use the raw method to avoid the library's error checking"
	^ self library BZ2_bzDecompressEnd: controlBlock.
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
	"private -- answer the BZIP2Stream that holds the state of our processing"

	^ controlBlock.!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ self class defaultInBufferSize.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ self class defaultOutBufferSize.!

defaultSkipBackLimit
	"answer how many extra objects we keep in our buffer in order
	to support negative positioning (e.g. in #peek) by default"

	^ self class defaultSkipBackLimit.!

discardBZip2Connection
	"private -- ensure that we will make no further use of resources managed by BZip2.
	This is different from #closeAbruptly in that it assumes that bzip2 is already
	dead"

	self isOpen ifTrue: [self beClosed].!

errorAlreadyClosed
	"private -- trigger a BZip2Error to say that the stream has already been closed"

	^ BZip2Error
		signal: 'already closed'
		with: BZ_SEQUENCE_ERROR.!

errorAlreadyOpen
	"private -- trigger a BZip2Error to say that the stream has already been opened"

	^ BZip2Error
		signal: 'already open'
		with: BZ_SEQUENCE_ERROR.!

errorNotPositionable: anInteger
	"private -- trigger an error because we cannot change position to anInteger"

	"this may be marginally more helpful than throwing a custom Error, and it's much easier ;-)"
	^ self errorSubscriptBounds: anInteger.
!

errorNotYetClosed
	"private -- trigger a BZip2Error to say that the stream has not yet been closed"

	^ BZip2Error
		signal: 'not yet closed'
		with: BZ_SEQUENCE_ERROR.!

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

	"loop until bzip2 has managed to put some data into the out buffer, or it
	signals eond-of-stream.  We need the loop since the in buffer may not
	hold enough data for bzip2 to produce *anything* on one itteration"
	[self fillOutBufferStep] whileFalse.
!

fillOutBufferStep
	"private -- ask bzip2 to generate some data in our out buffer (which is assumed to be empty).
	Answers whether it has managed to do so or has found end-of-stream, in either case we
	can stop bothering bzip2 for now.
	(Incidentally, the only real reason for this method to exist was to factor out the body of
	an awkward loop in #fillOutBuffer -- one of the cases where Smalltalk's flow-of-control
	syle forces a clumsy factoring)"

	| status |

	"already finished ?"
	atEnd ifTrue: [^ true].

	"try to fill the in buffer (null-op if the input's at EOF or the buffer's full)"
	self fillInBuffer.

	"ask bzip2 to fill the out buffer"
	status := self flushBZip2Stream.

	"has bzip2 found end-of-stream in its data ?  If so, then ideally we'd put the leftover data back
	on the input stream, but that assumes that the input stream is positionable, which it may not be.
	So we leave the stream where it is, but we provide an accessor to get any leftover data in case
	it's needed -- #unconsumedInput, and also (which may not work, but it's handy to have),
	#pushBackUnconsumedInput.  Alternatively you can avoid the problem, at the expense of
	efficiency, running with 'no' input buffer"
	status = BZ_STREAM_END ifTrue: [atEnd := true].

	^ atEnd or: [lastOut >= nextOut].!

finalize
	"we should ensure that we have released any resouces used
	internally by bzip2"

	self closeAbruptly.!

flushBZip2Stream
	"private -- flush the compression stream.
	Answers the status (BZ_OK or BZ_STREAM_END)"

	| status |

	self updateControlBlock.
	status := self library decompress: controlBlock.
	self updateFromControlBlock.

	^ status.!

inBufferSize
	"answer the size of the buffer used to hold data before
	decompression"

	^ inBuffer size.!

inBufferSize: anInteger
	"set the size of the buffer used for holding data before processsing.
	This influences the number of external calls to the bzip2 library, so
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

	controlBlock := BZIP2Stream new.
	isOpen := isClosed := atEnd := false.
	skipBackLimit := self defaultSkipBackLimit.
	useSmallDecompressor := false.

	self allocateBuffers.!

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
	NB: untill this returns true, the contents of the output stream are incomplete"

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

	^ BZip2Library default.!

makeBuffer: anInteger binary: aBoolean
	"private -- allocate a buffer to use for holding char or byte data while the bzip2 library works on it"

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
	to use resources that were allocated by bzip2 in the previous session"

	self discardBZip2Connection.!

open
	"initialize the underlying bzip2 library's data structures for processing
	our data.
	It is not normally necessary to call this explicitly since we #open
	lazily when we first are asked for any output"

	isOpen ifFalse: [self reallyOpen; beOpen].!

openBZip2Stream
	"private -- open the bzip2 stream or throw an error"

	^ self library decompressInit: controlBlock small:  useSmallDecompressor.!

outBufferSize
	"answer the size of the buffer used to hold data after
	decompression.  This value excludes the size of the
	skipBackBuffer (if any)"

	^ outBuffer size - skipBackLimit.!

outBufferSize: anInteger
	"set the size of the buffer used for holding data after processsing.
	This influences the number of external calls to the bzip2 library, and
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
	which we have just decompressed data up to the point where the bzip2 library states
	that the compressed stream is finished.  In such cases we may have read more bytes
	into our own buffers than we needed to.  This assumes that the input stream is positionable"

	firstIn <= lastIn ifTrue:
		[inStream skip: firstIn - lastIn - 1.	"negative"
		self resetInBuffer].


	!

reallyClose
	"private -- complete or abandon the process of decompression"

	self closeBZip2Stream.
!

reallyCloseAbruptly
	"private -- ensure that we have no further resources managed by BZip2"

	self closeBZip2StreamAbruptly.!

reallyOpen
	"private -- initialize the underlying bzip2 library's data structures for decompressing
	our data"

	self
		openBZip2Stream;
		fillOutBuffer.!

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
	which we have just decompressed data up to the point where the bzip2 library states
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
		avail_out: (outBuffer size - lastOut).				"number of bytes before the end of the buffer"
!

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

useSmallDecompressor
	"answer whether we use the less memory-intensive decompressor.
	According to the zlib doc, the small decompressor uses about half as much
	memory, but is about twice as slow"

	^ useSmallDecompressor.!

useSmallDecompressor: aBool
	"set whether we will tell bzlib to use the less memory-intensive decompressor.
	According to the zlib doc, the small decompressor uses about half as much
	memory, but is about twice as slow.
	By default we use the faster decompressor.
	Note that it is an error to call this after we have been #open-ed"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	useSmallDecompressor := aBool.! !
!BZip2DecompressingReadStream categoriesFor: #allocateBuffers!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #atEnd!public!streaming!testing! !
!BZip2DecompressingReadStream categoriesFor: #basicAtEnd!private!streaming!testing! !
!BZip2DecompressingReadStream categoriesFor: #beBinary!modes!public! !
!BZip2DecompressingReadStream categoriesFor: #beBinary:!modes!public! !
!BZip2DecompressingReadStream categoriesFor: #beClosed!modes!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #beOpen!modes!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #beText!initializing!modes!public! !
!BZip2DecompressingReadStream categoriesFor: #beText:!modes!public! !
!BZip2DecompressingReadStream categoriesFor: #close!operations!public! !
!BZip2DecompressingReadStream categoriesFor: #closeAbruptly!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #closeBZip2Stream!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #closeBZip2StreamAbruptly!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #compressedSize!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #compressionRatio!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #compressionRatioSoFar!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #contentsSpecies!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #controlBlock!accessing!private! !
!BZip2DecompressingReadStream categoriesFor: #defaultInBufferSize!constants!public! !
!BZip2DecompressingReadStream categoriesFor: #defaultOutBufferSize!constants!public! !
!BZip2DecompressingReadStream categoriesFor: #defaultSkipBackLimit!constants!public! !
!BZip2DecompressingReadStream categoriesFor: #discardBZip2Connection!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #errorAlreadyClosed!exceptions!private! !
!BZip2DecompressingReadStream categoriesFor: #errorAlreadyOpen!exceptions!private! !
!BZip2DecompressingReadStream categoriesFor: #errorNotPositionable:!exceptions!positioning!private! !
!BZip2DecompressingReadStream categoriesFor: #errorNotYetClosed!exceptions!private! !
!BZip2DecompressingReadStream categoriesFor: #fillInBuffer!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #fillOutBuffer!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #fillOutBufferStep!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #finalize!finalizing!public! !
!BZip2DecompressingReadStream categoriesFor: #flushBZip2Stream!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #inBufferSize!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #inBufferSize:!accessing!initializing!public! !
!BZip2DecompressingReadStream categoriesFor: #initialize!initializing!private! !
!BZip2DecompressingReadStream categoriesFor: #inStream!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #inStream:!initializing!private! !
!BZip2DecompressingReadStream categoriesFor: #isBinary!modes!public!testing! !
!BZip2DecompressingReadStream categoriesFor: #isClosed!modes!public!testing! !
!BZip2DecompressingReadStream categoriesFor: #isOpen!modes!public!testing! !
!BZip2DecompressingReadStream categoriesFor: #isText!modes!public!testing! !
!BZip2DecompressingReadStream categoriesFor: #library!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #makeBuffer:binary:!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #makeInBufferBinary:!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #makeOutBufferBinary:!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #next!public!reading!streaming! !
!BZip2DecompressingReadStream categoriesFor: #next:into:startingAt:!public!reading! !
!BZip2DecompressingReadStream categoriesFor: #onStartup!event handling!private! !
!BZip2DecompressingReadStream categoriesFor: #open!operations!public! !
!BZip2DecompressingReadStream categoriesFor: #openBZip2Stream!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #outBufferSize!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #outBufferSize:!accessing!initializing!public! !
!BZip2DecompressingReadStream categoriesFor: #position!accessing!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #position:!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #printOn:!printing!public! !
!BZip2DecompressingReadStream categoriesFor: #pushBackUnconsumedInput!operations!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #reallyClose!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #reallyCloseAbruptly!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #reallyOpen!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #resetInBuffer!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #resetOutBuffer!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #resetOutBufferToSkipback!helpers!operations!private! !
!BZip2DecompressingReadStream categoriesFor: #setToEnd!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #size!accessing!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #skip:!positioning!public! !
!BZip2DecompressingReadStream categoriesFor: #skipBackLimit!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #skipBackLimit:!accessing!initializing!public! !
!BZip2DecompressingReadStream categoriesFor: #unBufferInput!initializing!modes!public! !
!BZip2DecompressingReadStream categoriesFor: #uncompressedSize!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #unconsumedInput!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #updateControlBlock!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #updateFromControlBlock!helpers!private! !
!BZip2DecompressingReadStream categoriesFor: #upToEnd!public!reading! !
!BZip2DecompressingReadStream categoriesFor: #useSmallDecompressor!accessing!public! !
!BZip2DecompressingReadStream categoriesFor: #useSmallDecompressor:!initializing!modes!public! !

!BZip2DecompressingReadStream class methodsFor!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ 1024.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
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

	| compressedData decompressor |

	compressedData :=  BZip2CompressingWriteStream exampleCompressingAByteArray.

	decompressor := BZip2DecompressingReadStream forBinaryOn: compressedData readStream.

	Transcript nextPutAll: 'Decompressed: #('.
	decompressor do: [:each | Transcript display:each; space].
	Transcript nextPutAll: ')'; cr.

	decompressor close.!

exampleDecompressingAString1
	"decompressing a String from a ByteArray and writing it to the Transcript.

		self exampleDecompressingAString1.
	"

	| compressedData decompressor |

	compressedData :=  BZip2CompressingWriteStream exampleCompressingAString.

	decompressor := BZip2DecompressingReadStream forTextOn: compressedData readStream.

	Transcript nextPutAll: 'Decompressed: '''.
	decompressor do: [:each | Transcript nextPut:each].
	Transcript nextPutAll: ''''; cr.

	decompressor close.
!

exampleDecompressingAString2
	"decompressing a String from a ByteArray and writing it to the Transcript.
	Similar to exampleDecompressingAString1 except that we use more of
	the <ReadStream> protocol.

		self exampleDecompressingAString2.
	"

	| compressedData decompressor |

	compressedData :=  BZip2CompressingWriteStream exampleCompressingAString.

	decompressor := BZip2DecompressingReadStream forTextOn: compressedData readStream.

	[decompressor atEnd] whileFalse:
		[| word |
		word := decompressor nextWord.
		Transcript display:word; cr].

	decompressor close.
!

exampleDecompressingFromAStream1
	"decompression several lines of text which have been compressed
	'in-line' in an otherwise normal stream of bytes.

		self exampleDecompressingFromAStream1.
	"

	| inStream header decompressor trailer |

	"this data should consist of an 9-byte 'header', followed by an unknown amount
	of compressed data, followed by an 9-byte trailer"
	inStream :=  BZip2CompressingWriteStream exampleCompressingToAStream readStream.

	"first get the header"
	header := inStream next: 9.
	Transcript display: 'Header: '; display: header; cr.

	"open an BZip2DecompressingReadStream on the input"
	decompressor := BZip2DecompressingReadStream forTextOn: inStream.

	"read lines until we read EOF"
	[decompressor atEnd] whileFalse:
		[| line |
		line := decompressor nextLine.
		Transcript nextPutAll: line; cr].

	"now the decompressor will almost certainly have read beyond the compressed part of the
	input, so we have to adjust"
	decompressor
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

	| inStream header decompressor trailer |

	"this data should consist of an 9-byte 'header', followed by an unknown amount
	of compressed data, followed by an 9-byte trailer"
	inStream :=  BZip2CompressingWriteStream exampleCompressingToAStream readStream.

	"first get the header"
	header := inStream next: 9.
	Transcript display: 'Header: '; display: header; cr.

	"open an BZip2DecompressingReadStream on the input.  In this example we ensure that
	it will not buffer more input that it will use, hence it cannot read over the end
	of the compressed data (but that will make it run more slowly"
	decompressor := BZip2DecompressingReadStream forTextOn: inStream.
	decompressor unBufferInput.

	"read lines until we read EOF"
	[decompressor atEnd] whileFalse:
		[| line |
		line := decompressor nextLine.
		Transcript nextPutAll: line; cr].

	decompressor close.

	"now we can read the original trailer from the input, since the decompressor wasn't buffered it should not
	have consumed any of the data from the trailer"
	trailer := inStream upToEnd.
	Transcript display: 'Trailer: '; display: trailer; cr.!

file: aFilename text: aBool
	"answer a new instance that reads its compressed data from the given
	file, which is assumed to be in BZip2 format"

	^ (self new)
		inStream: (FileStream read: aFilename text: false);
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
!BZip2DecompressingReadStream class categoriesFor: #defaultInBufferSize!constants!public! !
!BZip2DecompressingReadStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!BZip2DecompressingReadStream class categoriesFor: #defaultSkipBackLimit!constants!public! !
!BZip2DecompressingReadStream class categoriesFor: #exampleDecompressingAByteArray!examples!public! !
!BZip2DecompressingReadStream class categoriesFor: #exampleDecompressingAString1!examples!public! !
!BZip2DecompressingReadStream class categoriesFor: #exampleDecompressingAString2!examples!public! !
!BZip2DecompressingReadStream class categoriesFor: #exampleDecompressingFromAStream1!examples!public! !
!BZip2DecompressingReadStream class categoriesFor: #exampleDecompressingFromAStream2!examples!public! !
!BZip2DecompressingReadStream class categoriesFor: #file:text:!instance creation!public! !
!BZip2DecompressingReadStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!BZip2DecompressingReadStream class categoriesFor: #forTextOn:!instance creation!public! !
!BZip2DecompressingReadStream class categoriesFor: #initialize!initializing!private! !
!BZip2DecompressingReadStream class categoriesFor: #new!instance creation!private! !
!BZip2DecompressingReadStream class categoriesFor: #on:!instance creation!public! !
!BZip2DecompressingReadStream class categoriesFor: #onStartup!event handling!private! !
!BZip2DecompressingReadStream class categoriesFor: #uninitialize!initializing!private! !

BZip2WriteStream guid: (GUID fromString: '{DE3EFEE1-82E0-4A70-B1CE-611BEE075397}')!
BZip2WriteStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Abstract superclass that collects commonality between {De/In}flaterWriteStream -- it is purely an implementation detail.

See the subclass comments, and class-side methods in the ''examples'' category.'!
!BZip2WriteStream categoriesForClass!Unclassified! !
!BZip2WriteStream methodsFor!

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
	"set whether we produce text or binary output / expect text or binary input"

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

beText
	"set whether we produce text or binary output / expect text or binary input"

	self beBinary: false.!

beText: aBool
	"set whether we produce text or binary output / expect text or binary input"

	self beBinary: aBool not.!

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

	"complete the bzip2 stuff"
	isOpen ifTrue: [self reallyClose].

	self beClosed.
!

closeAbruptly
	"private -- ensure that we have no further resources managed by BZip2.
	This is different from #discardBZip2Connection in that it assumes that
	bzip2 is still valid and so has to be cleaned up, and from #close in that it
	doesn't attempt to flush any outstanding data or check for errors"

	isOpen ifTrue: [self reallyCloseAbruptly; beClosed].!

closeBZip2Stream
	"private -- close the bzip2 stream or throw an error"

	self subclassResponsibility.!

closeBZip2StreamAbruptly
	"private -- close the bzip2 stream without any error checking"

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
	"private -- answer the BZIP2Stream that holds the state of our processing"

	^ controlBlock.!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ self class defaultInBufferSize.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is decompressed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ self class defaultOutBufferSize.!

discardBZip2Connection
	"private -- ensure that we will make no further use of resources managed by bzip2.
	This is different from #closeAbruptly in that it assumes that bzip2 is already
	dead"

	self isOpen ifTrue: [self beClosed].!

errorAlreadyClosed
	"private -- trigger a BZip2Error to say that the stream has already been closed"

	^ BZip2Error
		signal: 'already closed'
		with: BZ_SEQUENCE_ERROR.!

errorAlreadyOpen
	"private -- trigger a BZip2Error to say that the stream has already been opened"

	^ BZip2Error
		signal: 'already open'
		with: BZ_SEQUENCE_ERROR.!

errorNotPositionable: anInteger
	"private -- trigger an error because we cannot change position to anInteger"

	"this may be marginally more helpful than throwing a custom Error, and it's much easier ;-)"
	^ self errorSubscriptBounds: anInteger.
!

errorNotYetClosed
	"private -- trigger a BZip2Error to say that the stream has not yet been closed"

	^ BZip2Error
		signal: 'not yet closed'
		with: BZ_SEQUENCE_ERROR.!

finalize
	"we should ensure that we have released any resouces used
	internally by bzip2"

	self closeAbruptly.!

finishBZip2Stream
	"private -- flush the compression stream at the end of processing or throw an error.
	Answers the status (which may take several OK values)"

	self subclassResponsibility.
!

flushBZip2Stream
	"private -- flush the compression stream or throw an error.
	Answers the status (which may take several OK values)"

	self subclassResponsibility.
!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	self subclassResponsibility.!

inBufferSize
	"answer the size of the buffer used to hold data before
	processing"

	^ inBuffer size.!

inBufferSize: anInteger
	"set the size of the buffer used for holding data before processsing.
	This influences the number of external calls to the bzip2 library, so
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

	controlBlock := BZIP2Stream new.
	isOpen := isClosed := false.

	self allocateBuffers.!

isBinary
	"answer whether we producet text output / expect text input"

	self subclassResponsibility.!

isClosed
	"answer whether we have finished all the (de)compression we are going to do.
	NB: untill this returns true, the contents of the output stream are incomplete"

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

	^ BZip2Library default.!

makeBuffer: anInteger binary: aBoolean
	"private -- allocate a buffer to use for holding char or byte data while the bzip2 library works on it"

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
	to use resources that were allocated by bzip2 in the previous session"

	self discardBZip2Connection.!

open
	"initialize the underlying bzip2 library's data structures for processing
	our data.
	It is not normally necessary to call this explicitly since we #open
	lazily when we first need to flush our input data"

	isOpen ifFalse: [self reallyOpen; beOpen].!

openBZip2Stream
	"private -- open the bzip2 stream or throw an error"

	self subclassResponsibility.!

outBufferSize
	"answer the size of the buffer used to hold data after
	processing"

	^ outBuffer size.!

outBufferSize: anInteger
	"set the size of the buffer used for holding data after processsing.
	This influences the number of external calls to the bzip2 library, and
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

	"keep trying to 'FINISH' until bzip2 has flushed any data it holds internally"
	[| status |
	status := self finishBZip2Stream.
	self flushOutBuffer.
	status ~= BZ_STREAM_END] whileTrue.

	"finally we can close the [de]compressor"
	self closeBZip2Stream.!

reallyCloseAbruptly
	"private -- ensure that we have no further resources managed by BZip2"

	self closeBZip2StreamAbruptly.!

reallyOpen
	"private -- initialize the underlying bzip2 library's data structures for (de)compressing
	our data"

	self
		openBZip2Stream;
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
		avail_out: (outBuffer size - lastOut).				"number of bytes before the end of the buffer"
!

updateFromControlBlock
	"private -- update our current buffers with data from the control block"

	firstIn := controlBlock next_in asInteger - inBuffer yourAddress asInteger + 1.			"first byte that contains valid data"
	lastIn := firstIn + controlBlock avail_in - 1.													"last byte that contains valid data"
	lastOut := controlBlock next_out asInteger - outBuffer yourAddress asInteger.			"last byte that contains valid data"
! !
!BZip2WriteStream categoriesFor: #allocateBuffers!helpers!private! !
!BZip2WriteStream categoriesFor: #basicBeBinary:!modes!private! !
!BZip2WriteStream categoriesFor: #beBinary!modes!public! !
!BZip2WriteStream categoriesFor: #beBinary:!modes!public! !
!BZip2WriteStream categoriesFor: #beClosed!modes!operations!private! !
!BZip2WriteStream categoriesFor: #beOpen!modes!operations!private! !
!BZip2WriteStream categoriesFor: #beText!modes!public! !
!BZip2WriteStream categoriesFor: #beText:!modes!public! !
!BZip2WriteStream categoriesFor: #close!operations!public! !
!BZip2WriteStream categoriesFor: #closeAbruptly!operations!private! !
!BZip2WriteStream categoriesFor: #closeBZip2Stream!operations!private! !
!BZip2WriteStream categoriesFor: #closeBZip2StreamAbruptly!operations!private! !
!BZip2WriteStream categoriesFor: #compressedSize!accessing!public! !
!BZip2WriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!BZip2WriteStream categoriesFor: #compressionRatio!accessing!public! !
!BZip2WriteStream categoriesFor: #compressionRatioSoFar!accessing!public! !
!BZip2WriteStream categoriesFor: #contents!accessing!public! !
!BZip2WriteStream categoriesFor: #controlBlock!accessing!private! !
!BZip2WriteStream categoriesFor: #defaultInBufferSize!constants!public! !
!BZip2WriteStream categoriesFor: #defaultOutBufferSize!constants!public! !
!BZip2WriteStream categoriesFor: #discardBZip2Connection!operations!private! !
!BZip2WriteStream categoriesFor: #errorAlreadyClosed!exceptions!private! !
!BZip2WriteStream categoriesFor: #errorAlreadyOpen!exceptions!private! !
!BZip2WriteStream categoriesFor: #errorNotPositionable:!exceptions!positioning!private! !
!BZip2WriteStream categoriesFor: #errorNotYetClosed!exceptions!private! !
!BZip2WriteStream categoriesFor: #finalize!finalizing!public! !
!BZip2WriteStream categoriesFor: #finishBZip2Stream!operations!private! !
!BZip2WriteStream categoriesFor: #flushBZip2Stream!operations!private! !
!BZip2WriteStream categoriesFor: #flushInBuffer!operations!private! !
!BZip2WriteStream categoriesFor: #inBufferSize!accessing!public! !
!BZip2WriteStream categoriesFor: #inBufferSize:!accessing!initializing!public! !
!BZip2WriteStream categoriesFor: #initialize!initializing!private! !
!BZip2WriteStream categoriesFor: #isBinary!modes!public!testing! !
!BZip2WriteStream categoriesFor: #isClosed!modes!public!testing! !
!BZip2WriteStream categoriesFor: #isOpen!modes!public!testing! !
!BZip2WriteStream categoriesFor: #isText!modes!public!testing! !
!BZip2WriteStream categoriesFor: #library!helpers!private! !
!BZip2WriteStream categoriesFor: #makeBuffer:binary:!helpers!private! !
!BZip2WriteStream categoriesFor: #makeInBufferBinary:!helpers!private! !
!BZip2WriteStream categoriesFor: #makeOutBufferBinary:!helpers!private! !
!BZip2WriteStream categoriesFor: #next:putAll:startingAt:!public!writing! !
!BZip2WriteStream categoriesFor: #nextPut:!public!writing! !
!BZip2WriteStream categoriesFor: #onStartup!event handling!private! !
!BZip2WriteStream categoriesFor: #open!operations!public! !
!BZip2WriteStream categoriesFor: #openBZip2Stream!operations!private! !
!BZip2WriteStream categoriesFor: #outBufferSize!accessing!public! !
!BZip2WriteStream categoriesFor: #outBufferSize:!accessing!initializing!public! !
!BZip2WriteStream categoriesFor: #outStream!accessing!public! !
!BZip2WriteStream categoriesFor: #outStream:!initializing!private! !
!BZip2WriteStream categoriesFor: #outStreamContents!accessing!public! !
!BZip2WriteStream categoriesFor: #position!accessing!positioning!public! !
!BZip2WriteStream categoriesFor: #position:!positioning!public! !
!BZip2WriteStream categoriesFor: #printOn:!printing!public! !
!BZip2WriteStream categoriesFor: #reallyClose!operations!private! !
!BZip2WriteStream categoriesFor: #reallyCloseAbruptly!operations!private! !
!BZip2WriteStream categoriesFor: #reallyOpen!operations!private! !
!BZip2WriteStream categoriesFor: #resetInBuffer!helpers!private! !
!BZip2WriteStream categoriesFor: #resetOutBuffer!helpers!private! !
!BZip2WriteStream categoriesFor: #uncompressedSize!accessing!public! !
!BZip2WriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!BZip2WriteStream categoriesFor: #updateControlBlock!helpers!private! !
!BZip2WriteStream categoriesFor: #updateFromControlBlock!helpers!private! !

!BZip2WriteStream class methodsFor!

defaultInBufferSize
	"answer how big a buffer to use to hold data before it is processed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	^ 1024.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is processed.
	Since this is only used to reduce the number of calls to the bzip2 library
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
!BZip2WriteStream class categoriesFor: #defaultInBufferSize!constants!public! !
!BZip2WriteStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!BZip2WriteStream class categoriesFor: #initialize!initializing!private! !
!BZip2WriteStream class categoriesFor: #new!instance creation!private! !
!BZip2WriteStream class categoriesFor: #onStartup!event handling!private! !
!BZip2WriteStream class categoriesFor: #uninitialize!initializing!private! !

BZip2CompressingWriteStream guid: (GUID fromString: '{D55817AD-09A7-4FF2-B0E1-A62C9A549F98}')!
BZip2CompressingWriteStream comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

One of these wraps a (binary) WriteStream and arranges that any data writen to itself will be compressed with the bzip2 library, and the compressed data sent onwards to the output stream.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

You can set options to control the compression, this is only allowed before the stream is opened (or first written-to).  A later version may also support ''resetting'' the compressor state in mid-stream, and re-synchronising after data errors -- this version doesn''t.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to aquire and release resource in the BZIP2 library.  (They do close themselves by finalisation, if necessary, but don''t rely on that producing correct results, since data is not necessarily flushed through to the underlying stream until the #close).

Note that these streams close themselves on image startup.

Also see the class-side methods in the ''examples'' category.
'!
!BZip2CompressingWriteStream categoriesForClass!Unclassified! !
!BZip2CompressingWriteStream methodsFor!

basicBeBinary: aBool
	"private -- set whether we expect text or binary input"

	self makeInBufferBinary: aBool.!

blockSize
	"answer the block size (in 100K units) that we will tell the bzip2
	library to use.  See #blockSize: for more discussion
	of what that means"

	^ blockSize.!

blockSize: anInteger
	"set the block size we will ask the bzip2 library to use for compression.
	The algorithm works by splitting data into blocks, massaging each block
	to make it 'more compressible', and and then pushing the results through
	a conventional compressor.
	The block size determines how big a block is used (in 100K) and should be
	in the range BZ_MIN_BLOCK_SIZE (1) to BZ_MAX_BLOCK_SIZE (9) inclusive.
	Higher values naturally use more memory, but can result in better compression
	and may be somewhat slower.
	There is no point in using a block that is larger than the total text to be
	compressed.
	This package uses a default of 3.
	Note that it is an error to call this after we have been #open-ed"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	blockSize := anInteger.!

closeBZip2Stream
	"private -- close the bzip2 stream or throw an error"

	^ self library compressEnd: controlBlock.
!

closeBZip2StreamAbruptly
	"private -- close the bzip2 stream without any error checking"

	"use the raw method to avoid the library's error checking"
	^ self library BZ2_bzCompressEnd: controlBlock.
!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_out.!

defaultBlockSize
	"answer the block size (in 100K units) that we will tell the bzip2
	library to use by default.  See #blockSize: for more discussion
	of what that means"

	^ self class defaultBlockSize.!

finishBZip2Stream
	"private -- flush the compression stream at the end of processing or throw an error.
	Answers the status (which may take several OK values)"

	| status |

	self updateControlBlock.
	status := self library compressFinish: controlBlock.
	self updateFromControlBlock.

	^ status.
!

flushBZip2Stream
	"private -- flush the compression stream or throw an error.
	Answers the status (which may take several OK values)"

	| status |

	self updateControlBlock.
	status := self library compressRun: controlBlock.
	self updateFromControlBlock.

	^ status.!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	"keep compressing stuff untill there's nothing left to do.
	Note that we flush the output buffer completely each time around
	which may be a little inefficient, but it does make the logic slightly
	easier since we never have to worry that the out buffer may be
	full before we do the flush"
	[firstIn <= lastIn] whileTrue:
				[self
					flushBZip2Stream;
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

initialize
	"private -- establish a coherent initial state"

	super initialize.

	blockSize := self defaultBlockSize.
	workFactor := BZ_DEFAULT_WORK_FACTOR.


!

isBinary
	"answer whether we expect binary input"

	^ inBuffer isKindOf: ByteArray.
!

openBZip2Stream
	"private -- open the bzip2 stream or throw an error"

	^self library
		compressInit: controlBlock
		blockSize: blockSize
		workFactor: workFactor.

!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_in.!

useBestCompression
	"set ourselves to use the best (but marginally slowest) compression.
	Note that it is an error to call this after we have been #open-ed"

	self blockSize: BZ_MAX_BLOCK_SIZE.!

useBestSpeed
	"set ourselves to use the lowest memory consumption, and
	marginally best speed, but worst compression.
	Note that this has no effect after we have been #opened"

	self blockSize: BZ_MIN_BLOCK_SIZE.
!

workFactor
	"answer the 'work factor' that we will tell / have told the bzip2
	library to use.  See #workFactor: for a discussion of what that
	means"

	^ workFactor.!

workFactor: anInteger
	"set the work factor we will ask the bzip2 library to use for compression.
	The compression algorithm makes heavy use of sorting internally, and the the work factor
	is a measure of how readily it will fall back from a sort routine that is quick, but has a poor
	worst case, to one that is slower (~3x) but has no pathological behaviour.
	This should be in the range BZ_MIN_WORK_FACTOR (=1) to BZ_MAX_WORK_FACTOR (=230)
	inclusive or be BZ_DEFAULT_WORK_FACTOR (=0, and equvalent to 30).
	Higher values mean that it is willing to do more work before deciding that the supposedly
	'quick' algorithm isn't appropriate.
	Note that it is an error to call this after we have been #open-ed"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	workFactor := anInteger.! !
!BZip2CompressingWriteStream categoriesFor: #basicBeBinary:!modes!private! !
!BZip2CompressingWriteStream categoriesFor: #blockSize!accessing!public! !
!BZip2CompressingWriteStream categoriesFor: #blockSize:!initializing!public! !
!BZip2CompressingWriteStream categoriesFor: #closeBZip2Stream!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #closeBZip2StreamAbruptly!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!BZip2CompressingWriteStream categoriesFor: #defaultBlockSize!constants!public! !
!BZip2CompressingWriteStream categoriesFor: #finishBZip2Stream!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #flushBZip2Stream!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #flushInBuffer!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #flushOutBuffer!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #initialize!initializing!private! !
!BZip2CompressingWriteStream categoriesFor: #isBinary!modes!public!testing! !
!BZip2CompressingWriteStream categoriesFor: #openBZip2Stream!operations!private! !
!BZip2CompressingWriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!BZip2CompressingWriteStream categoriesFor: #useBestCompression!initializing!modes!public! !
!BZip2CompressingWriteStream categoriesFor: #useBestSpeed!initializing!modes!public! !
!BZip2CompressingWriteStream categoriesFor: #workFactor!accessing!public! !
!BZip2CompressingWriteStream categoriesFor: #workFactor:!initializing!public! !

!BZip2CompressingWriteStream class methodsFor!

compressBinary: aByteArray
	"answer a ByteArray containing the result of compressing the given ByteArray
	using the default block size"

	^ (self forBinary)
		nextPutAll: aByteArray;
		contents.!

compressBinary: aByteArray blockSize: anInteger
	"answer a ByteArray containing the result of compressing the given ByteArray.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ (self forBinaryWithBlockSize: anInteger)
		nextPutAll: aByteArray;
		contents.!

compressText: aString
	"answer a ByteArray containing the result of compressing the given string
	using the default block size"

	^ (self forText)
		nextPutAll: aString;
		contents.!

compressText: aString blockSize: anInteger
	"answer a ByteArray containing the result of compressing the given String.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ (self forTextWithBlockSize: anInteger)
		nextPutAll: aString;
		contents.!

defaultBlockSize
	"answer the default block size (in 100K units) that our instances
	will use"

	"there's very little guidance.  Note that this is wastefull of memory
	if we compress less than 300K bytes"
	^ 3.!

exampleCompressingAByteArray
	"compressing a ByteArray into another ByteArray using all the defaults.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAByteArray.
	"

	"you may find it instructive to trace through this since it uses more of the public API"
	^ BZip2CompressingWriteStream compressBinary: #(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 0).!

exampleCompressingAString
	"compressing a string into a ByteArray using all the defaults.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingAString.
	"

	"you may find it instructive to trace through this since it uses more of
	the public API"
	^ BZip2CompressingWriteStream compressText: 'The cat sat on the mat'.!

exampleCompressingToAStream
	"compressing several lines of text using the deflater explicitly as a Stream
	which writes to an existing stream.
	NB: answers the byte array so that it can be used by the decompression
	examples

		self exampleCompressingToAStream.
	"

	| stream compressor |

	"set up the stream that we'll add to, put a 'header' on it"
	stream := ByteArray writeStream.
	stream nextPutAll: #[0 1 2 3 4 5 6 7 9].

	"create a deflater that will append to the existing stream"
	compressor := BZip2CompressingWriteStream forTextOn: stream.

	"write some lines of text to it"
	compressor
		nextPutAll: 'The rat sat on the mat'; cr;
		nextPutAll: 'The cat sat on the rat'; cr;
		nextPutAll: 'Which was unlucky for the rat'; cr.

	"now we *must* close the deflater of the compressed data will not be flushed
	through the bzip2 library and into our orginal stream"
	compressor close.

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

	| compressor |

	"create a deflater write stream that will build a ByteArray using an internal WriteStream"
	compressor := BZip2CompressingWriteStream forText.

	"write some lines of text to it"
	compressor
		nextPutAll: 'The rat sat on the mat'; cr;
		nextPutAll: 'The cat sat on the rat'; cr;
		nextPutAll: 'Which was unlucky for the rat'; cr.

	"get the compressed text back.  Note that this implicitly closes the stream"
	^ compressor contents.!

file: aFilename text: aBool
	"answer a new instance that writes compressed data to the given
	file in BZip2 format (a .bz2 file extension is conventional)"

	^ (self new)
		outStream: (FileStream write: aFilename text: false);
		beText: aBool;
		yourself.
!

forBinary
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to its internal write stream.
	This will use the default block size"

	^ self forBinaryOn: ByteArray writeStream.!

forBinaryOn: aBinaryWriteStream
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default block size"

	^ (self new)
		beBinary;
		outStream: aBinaryWriteStream;
		yourself.
!

forBinaryOn: aBinaryWriteStream blockSize: anInteger
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ (self new)
		beBinary;
		blockSize: anInteger;
		outStream: aBinaryWriteStream;
		yourself.
		!

forBinaryWithBlockSize: anInteger
	"answer a new instance that will compress binary data that is written to it,
	and forward the compressed bytes to its internal write stream.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ self forBinaryOn: ByteArray writeStream blockSize: anInteger.
		!

forText
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to its internal write stream.
	This will use the default block size"

	^ self forTextOn: ByteArray writeStream.!

forTextOn: aBinaryWriteStream
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default block size"

	^ (self new)
		beText;
		outStream: aBinaryWriteStream;
		yourself.
		!

forTextOn: aBinaryWriteStream blockSize: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ (self new)
		beText;
		blockSize: anInteger;
		outStream: aBinaryWriteStream;
		yourself.
		!

forTextWithBlockSize: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to its internal write stream.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ self forTextOn: ByteArray writeStream blockSize: anInteger
!

on: aBinaryWriteStream
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	This will use the default block size"

	^ self forTextOn: aBinaryWriteStream.
		!

on: aBinaryWriteStream blockSize: anInteger
	"answer a new instance that will compress text data that is written to it,
	and forward the compressed bytes to aBinaryWriteStream.
	The block size is set by anInteger and should be a value from
	1 to 9 inclusive or one of the values from BZip2Constants; see
	#blockSize: on the instance-side for more information"

	^ self forTextOn: aBinaryWriteStream blockSize: anInteger.
		! !
!BZip2CompressingWriteStream class categoriesFor: #compressBinary:!compressing!public! !
!BZip2CompressingWriteStream class categoriesFor: #compressBinary:blockSize:!compressing!public! !
!BZip2CompressingWriteStream class categoriesFor: #compressText:!compressing!public! !
!BZip2CompressingWriteStream class categoriesFor: #compressText:blockSize:!compressing!public! !
!BZip2CompressingWriteStream class categoriesFor: #defaultBlockSize!constants!public! !
!BZip2CompressingWriteStream class categoriesFor: #exampleCompressingAByteArray!examples!public! !
!BZip2CompressingWriteStream class categoriesFor: #exampleCompressingAString!examples!public! !
!BZip2CompressingWriteStream class categoriesFor: #exampleCompressingToAStream!examples!public! !
!BZip2CompressingWriteStream class categoriesFor: #exampleCompressingWithAStream!examples!public! !
!BZip2CompressingWriteStream class categoriesFor: #file:text:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forBinary!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forBinaryOn:blockSize:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forBinaryWithBlockSize:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forText!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forTextOn:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forTextOn:blockSize:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #forTextWithBlockSize:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #on:!instance creation!public! !
!BZip2CompressingWriteStream class categoriesFor: #on:blockSize:!instance creation!public! !

BZip2DecompressingWriteStream guid: (GUID fromString: '{C304F034-5A24-4F2F-B1AD-FDB7714777AB}')!
BZip2DecompressingWriteStream comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these wraps a WriteStream and arranges that any data writen to itself will be decompressed with the bzip2 library, and the decompressed data sent onwards to the output stream.  This process is incremental -- unless you arrange otherwise, neither the input nor the output is all held in memory at one time (unless it''s small enough to fit into one of the buffers, of course).

NB: For most purposes BZip2DecompressingReadStream is a better bet for the task of decompressing data, however this version is occasionally usefull (and quite a bit faster), so I''ve left it in.

It is important to realise that these streams *MUST* be opened before use (it''ll do that automatically if you haven''t done it explicitly) and closed after use.  That is necessary in order to aquire and release resource in the BZIP2 library.  (They do close themselves by finalisation, if necessary, but don''t rely on that producing correct results, since data is not necessarily flushed through to the underlying stream until the #close).

Note that these streams close themselves on image startup.

Also see the class-side methods in the ''examples'' category.
'!
!BZip2DecompressingWriteStream categoriesForClass!Unclassified! !
!BZip2DecompressingWriteStream methodsFor!

basicBeBinary: aBool
	"private -- set whether we produce text or binary output"

	self makeOutBufferBinary: aBool.
!

closeBZip2Stream
	"private -- close the bzip2 stream or throw an error"

	^ self library decompressEnd: controlBlock.
!

closeBZip2StreamAbruptly
	"private -- close the bzip2 stream without any error checking"

	"use the raw method to avoid the library's error checking"
	^ self library BZ2_bzDecompressEnd: controlBlock.
!

compressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_in.
!

finishBZip2Stream
	"private -- flush the compression stream at the end of processing or throw an error.
	Answers the status (which may take several OK values)"

	^ self flushBZip2Stream.!

flushBZip2Stream
	"private -- flush the compression stream.
	Answers the status (BZ_OK or BZ_STREAM_END)"

	| status |

	self updateControlBlock.
	status := self library decompress: controlBlock.
	self updateFromControlBlock.

	^ status.!

flushInBuffer
	"private -- ensure that we are holding no data in our inBuffer"

	| status |

	"keep decompressing stuff until there's nothing left in the buffer or until
	bzip2 has discovered end-of-stream on the input.
	We will keep pushing data into bzip2 until EOS at which point we close
	the stream and return.
	Note that we flush the output buffer completely each time around
	which may be a little inefficient, but it does make the logic slightly
	easier since we never have to worry that the out buffer may be
	full before calling bzip2"
	[firstIn <= lastIn] whileTrue:
		[status := self flushBZip2Stream.
		self flushOutBuffer.
		status = BZ_STREAM_END ifTrue: [self closeBZip2Stream; beClosed. ^ self]].

	"start again at the begining of the in buffer"
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

initialize
	"private -- establish a coherent initial state"

	super initialize.

	useSmallDecompressor := false.

!

isBinary
	"answer whether we producet binary output"

	^ outBuffer isKindOf: ByteArray.!

openBZip2Stream
	"private -- open the bzip2 stream or throw an error"

	^ self library decompressInit: controlBlock small:  useSmallDecompressor.!

uncompressedSizeSoFar
	"answer the compressed size of our data.
	Note that this is approximate until we have been #close-ed"

	^ controlBlock total_out.
!

useSmallDecompressor
	"answer whether we use the less memory-intensive decompressor.
	According to the zlib doc, the small decompressor uses about half as much
	memory, but is about twice as slow"

	^ useSmallDecompressor.!

useSmallDecompressor: aBool
	"set whether we will tell bzlib to use the less memory-intensive decompressor.
	According to the zlib doc, the small decompressor uses about half as much
	memory, but is about twice as slow.
	By default we use the faster decompressor.
	Note that it is an error to call this after we have been #open-ed"

	isOpen ifTrue: [^ self errorAlreadyOpen].

	useSmallDecompressor := aBool.! !
!BZip2DecompressingWriteStream categoriesFor: #basicBeBinary:!modes!private! !
!BZip2DecompressingWriteStream categoriesFor: #closeBZip2Stream!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #closeBZip2StreamAbruptly!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #compressedSizeSoFar!accessing!public! !
!BZip2DecompressingWriteStream categoriesFor: #finishBZip2Stream!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #flushBZip2Stream!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #flushInBuffer!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #flushOutBuffer!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #initialize!initializing!private! !
!BZip2DecompressingWriteStream categoriesFor: #isBinary!modes!public!testing! !
!BZip2DecompressingWriteStream categoriesFor: #openBZip2Stream!operations!private! !
!BZip2DecompressingWriteStream categoriesFor: #uncompressedSizeSoFar!accessing!public! !
!BZip2DecompressingWriteStream categoriesFor: #useSmallDecompressor!accessing!public! !
!BZip2DecompressingWriteStream categoriesFor: #useSmallDecompressor:!initializing!modes!public! !

!BZip2DecompressingWriteStream class methodsFor!

decompressBinary: aByteArray
	"answer a ByteArray containing the result of decompressing the given ByteArray"

	^ (self forBinaryOn: ByteArray writeStream)
		nextPutAll: aByteArray;
		contents.!

decompressText: aByteArray
	"answer a String containing the result of decompressing the given ByteArray"

	^ (self forTextOn: String writeStream)
		nextPutAll: aByteArray;
		contents.!

defaultOutBufferSize
	"answer how big a buffer to use to hold data after it is processed.
	Since this is only used to reduce the number of calls to the bzip2 library
	there is no great pressure to use a large buffer"

	"since we are expecting more output than input"
	^ 2 * super defaultOutBufferSize.!

exampleDecompressingAByteArray
	"decompressing a ByteArray into another ByteArray

		self exampleDecompressingAByteArray.
	"

	| compressedData |

	compressedData :=  BZip2CompressingWriteStream exampleCompressingAByteArray.

	"you may find it instructive to trace through this since it uses more of
	the public API"
	^ BZip2DecompressingWriteStream decompressBinary: compressedData.!

exampleDecompressingAString
	"decompressing a string from a ByteArray.

		self exampleDecompressingAString.
	"

	| compressedData |

	compressedData :=  BZip2CompressingWriteStream exampleCompressingAString.

	"you may find it instructive to trace through this, since it uses more of
	the public API"
	^ BZip2DecompressingWriteStream decompressText: compressedData.!

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
!BZip2DecompressingWriteStream class categoriesFor: #decompressBinary:!decompressing!public! !
!BZip2DecompressingWriteStream class categoriesFor: #decompressText:!decompressing!public! !
!BZip2DecompressingWriteStream class categoriesFor: #defaultOutBufferSize!constants!public! !
!BZip2DecompressingWriteStream class categoriesFor: #exampleDecompressingAByteArray!examples!public! !
!BZip2DecompressingWriteStream class categoriesFor: #exampleDecompressingAString!examples!public! !
!BZip2DecompressingWriteStream class categoriesFor: #forBinary!instance creation!public! !
!BZip2DecompressingWriteStream class categoriesFor: #forBinaryOn:!instance creation!public! !
!BZip2DecompressingWriteStream class categoriesFor: #forText!instance creation!public! !
!BZip2DecompressingWriteStream class categoriesFor: #forTextOn:!instance creation!public! !
!BZip2DecompressingWriteStream class categoriesFor: #on:!instance creation!public! !

"Binary Globals"!

"Resources"!

