| package |
package := Package name: 'CU TarFiles'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Rough analogs of the ZipFile objects that understand UNIX TAR file format.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.02'.


package classNames
	add: #TarFile;
	add: #TarFileEntry;
	add: #TarFileError;
	yourself.

package methodNames
	add: #FILETIME -> #unixTime;
	yourself.

package globalNames
	add: #TarFileConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU BZip2 Streams';
	add: 'CU ZLib Streams';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #TarFile
	instanceVariableNames: 'stream name entries toc modified openEntry nextWritePosition'
	classVariableNames: ''
	poolDictionaries: 'TarFileConstants'
	classInstanceVariableNames: ''!
Object subclass: #TarFileEntry
	instanceVariableNames: 'stream positionInStream filename mode uid gid size filetime checksum type linkname magic uname gname devmajor devminor'
	classVariableNames: 'DefaultFiletime FileTypes'
	poolDictionaries: 'TarFileConstants'
	classInstanceVariableNames: ''!
Error subclass: #TarFileError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!FILETIME methodsFor!

unixTime
	"Answer the number of seconds to the time the reciever represents since the UNIX epoch"
#CUadded.

	^ self asInteger - UnixEpoch // 10000000.

! !
!FILETIME categoriesFor: #unixTime!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #TarFileConstants put: (PoolConstantsDictionary named: #TarFileConstants)!
TarFileConstants at: 'TAR_BLOCK_SIZE' put: 16r200!
TarFileConstants at: 'TAR_CHECKSUM_OFFSET' put: 16r94!
TarFileConstants at: 'TAR_CHECKSUM_SIZE' put: 16r8!
TarFileConstants at: 'TAR_DEVMAJOR_SIZE' put: 16r8!
TarFileConstants at: 'TAR_DEVMINOR_SIZE' put: 16r8!
TarFileConstants at: 'TAR_FILE_MODE_SIZE' put: 16r8!
TarFileConstants at: 'TAR_FILE_SIZE_SIZE' put: 16rC!
TarFileConstants at: 'TAR_FILENAME_SIZE' put: 16r64!
TarFileConstants at: 'TAR_GNU_DUMP' put: 'GNUtar '!
TarFileConstants at: 'TAR_GROUP_ID_SIZE' put: 16r8!
TarFileConstants at: 'TAR_GROUPNAME_SIZE' put: 16r20!
TarFileConstants at: 'TAR_LINKNAME_SIZE' put: 16r64!
TarFileConstants at: 'TAR_MAGIC' put: 'ustar  '!
TarFileConstants at: 'TAR_MAGIC_SIZE' put: 16r8!
TarFileConstants at: 'TAR_MODE_GEXEC' put: 16r8!
TarFileConstants at: 'TAR_MODE_GREAD' put: 16r20!
TarFileConstants at: 'TAR_MODE_GWRITE' put: 16r10!
TarFileConstants at: 'TAR_MODE_OEXEC' put: 16r1!
TarFileConstants at: 'TAR_MODE_OREAD' put: 16r4!
TarFileConstants at: 'TAR_MODE_OWRITE' put: 16r2!
TarFileConstants at: 'TAR_MODE_SGID' put: 16r400!
TarFileConstants at: 'TAR_MODE_SUID' put: 16r800!
TarFileConstants at: 'TAR_MODE_SVTX' put: 16r200!
TarFileConstants at: 'TAR_MODE_UEXEC' put: 16r40!
TarFileConstants at: 'TAR_MODE_UREAD' put: 16r100!
TarFileConstants at: 'TAR_MODE_UWRITE' put: 16r80!
TarFileConstants at: 'TAR_MODTIME_SIZE' put: 16rC!
TarFileConstants at: 'TAR_PREFIX_SIZE' put: 16r9B!
TarFileConstants at: 'TAR_TYPE_BLOCK_SPECIAL' put: $4!
TarFileConstants at: 'TAR_TYPE_CHAR_SPECIAL' put: $3!
TarFileConstants at: 'TAR_TYPE_CONTIGUOUS' put: $7!
TarFileConstants at: 'TAR_TYPE_DIRECTORY' put: $5!
TarFileConstants at: 'TAR_TYPE_HARDLINK' put: $1!
TarFileConstants at: 'TAR_TYPE_NAMED_PIPE' put: $6!
TarFileConstants at: 'TAR_TYPE_REGULAR' put: $0!
TarFileConstants at: 'TAR_TYPE_SYMLINK' put: $2!
TarFileConstants at: 'TAR_TYPEFLAG_SIZE' put: 16r1!
TarFileConstants at: 'TAR_USER_ID_SIZE' put: 16r8!
TarFileConstants at: 'TAR_USERNAME_SIZE' put: 16r20!
TarFileConstants shrink!

"Classes"!

TarFile guid: (GUID fromString: '{820E1800-8E59-40CF-8F84-7668E27F76D7}')!
TarFile comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

These represent TAR format data stored either on-disk or held as a ByteArray in memory.  It provides the abillity to read exisiting files (or memory structures, but I''m not going to go on repeating that mantra), creating new ones, or appending to existing ones.  It does not provide a way to make modifications to the contents of an existing file (that would be possible, but it''d be just about as inefficient as copying, without providing much benefit, so we don''t bother).

In fact one of our instances doesn''t really do very much except hold a table of contents, which is a map from the String names of entries to the entries themselves.

One additional role we have is to manage the underlying stream.  Closing and flushing it (and writing an EOF marker to it) as necessary.

Instances can be opened (readonly) on binary or text streams -- the only difference it makes is to the default type answered by our entries'' #contents methods (and so also affects which of #textContents or #binaryContents can be implemented without an additional conversion).  However we can only write, or append to, binary streams (it would be possible to allow text streams too, but it''s too much messing for too little gain).

By and large, I have attempted to duplicate the ZipFile API here (and on our entries), but please don''t be fooled into thinking that the two classes are really all that similar.  In particular, we are less efficient than ZipFiles because we don''t have the ability to read the contents of an entry incdementally (i.e. the contents are always copied completely into memory, even if one of the #withReaderDo:-style methods is used -- a later version of this package will probably fix that).'!
!TarFile categoriesForClass!Unclassified! !
!TarFile methodsFor!

addEntry: aTarFileEntry
	"privatge -- add the given entry to our list and table of contents"

	| key |

	entries addLast: aTarFileEntry.

	key := aTarFileEntry filename.
	(toc includesKey: key) ifTrue: [Notification signal: 'Duplicate entry name in TAR File' with: key].
	toc at: key put: aTarFileEntry.!

at: aString
	"if we contain an entry with the given name then answer it, otherwise throw an error"

	^ toc at: aString.!

at: aString ifAbsent: a0Block
	"if we contain a central directory element with the given name then answer it,
	otherwise answer the result of evaluating a0Block"

	^ toc at: aString ifAbsent: a0Block.!

at: aString ifPresent: a1Block
	"if we contain a central directory element with the given name then answer
	the result of evaluating a1Block passing the entry as a parameter,
	otherwise answer nil"

	^ toc at: aString ifPresent: a1Block.!

close
	"close our underlying stream (which may be a null-op).  More importantly,
	ensure that any additions we have made to the TAR File have been flushed
	out to the stream and an EOF marker written.
	This is a null-op if we have not made any modifications to the underlying
	file/data (i.e. you don't need to bother to #close instances that are only 
	used for reading TAR files)"

	self flush.

	stream close.!

closeEntry
	"finish the process of writing the entry opened with #openEnty:"

	self assert: [openEntry notNil].

	"make sure the open entry has been written to disk"
	openEntry hasBeenWritten ifFalse: [openEntry write].

	"remember where we'll be writing the next addition"
	nextWritePosition := openEntry positionOfNextInStream.

	openEntry  := nil.!

createEntry: aString
	"private -- answer a newly created TarFileEntry ready to be added to ourself"

	self assert: [nextWritePosition notNil].

	^ (TarFileEntry newAt: nextWritePosition on: stream)
		filename: aString;
		timestampNow;
		yourself.
!

defaultToc
	"private -- answer the <Dictionary> that we will use to map filenames
	onto entries by default.
	TAR files are case-sensitive and distinguish between,
	say, 'dir/filename' and 'dir/filename/' with the later being the
	name of a directory (called 'filename') whereas the former is
	the name of a file"

	^ LookupTable new.!

displayOn: aStream
	"write a user-centric representation of ourselves to aStream"

	aStream display: (name ifNil: ['<in memory>']).!

do: a1Block
	"evaluate a1Block for each entry we contain.
	We iterate over them in file order"

	entries do: a1Block.!

entries
	"answer a list (a shallow copy) of all our entries"

	^ OrderedCollection withAll: entries.!

fileFormatError: aString
	"private -- trigger a standard TarFile error with the given message"

	self fileFormatError: aString with: nil.
!

fileFormatError: aString with: anObject
	"private -- trigger a standard TarFile error with the given message and tag"

	self class fileFormatError: aString with: anObject.
!

filename
	"there are so many contexts that assume that #name means 'filename'
	that we may as well have the alias"

	^ self name.!

flush
	"flush our underlying stream (which may be a null-op).  More importantly,
	ensure that any additions we have made to the TAR File have been flushed
	out to the stream, and that an EOF marker has been added  After this has been
	called the on-disk structure is once again a valid TAR file.
	Note that calling this, and then adding new entries, will cause extra work,
	since the new entries will overwrite the EOF marker that we add here"

	modified ifTrue:
		[self writeEOFMarker.
		stream flush.
		modified := false].!

hasOpenEntry
	"answer true iff we have had an entry opened but not yet closed"

	^ openEntry  notNil.!

includes: aString
	"answer whether we contain an entry with the given name"

	^ toc includesKey: aString.!

initialize
	"private -- establish a coherent initial state"

	stream := nil.
	modified := false.
	self initializeEntryLists.
!

initializeEntryLists
	"private -- set up our entry lists to be empty"

	toc := self defaultToc.
	entries := OrderedCollection new.
	openEntry := nil.
	nextWritePosition := nil.
!

isEmpty
	"answer whether we (yet) have any entries"

	^ self size < 1.!

name
	"answer our name, typically the name of the file we are reading/writing.
	May be nil"

	^ name.!

name: aString
	"private -- set the receiver's name to aString (often the name of the file we are to
	read or write)"

	name := aString.
!

openEntry
	"answer the entry, if there is one, that has been opened but not yet closed"

	^ openEntry.!

openEntry: aString
	"add a new entry with the given name.
	Will throw an error if there is already an entry of the same name.
	NB: this does *not* write the entry to the underlying stream, and so
	until #closeEntry has been called, we are in an inconsistent state"

	self assert: [openEntry isNil].

	(self includes: aString) ifTrue: [self fileFormatError: 'Cannot add entry with duplicate name' with: aString].

	"create the new entry and add it to list of entries and to the the table of contents"
	openEntry := self createEntry: aString.
	self addEntry: openEntry.

	"remember that we've been modified"
	modified := true.

	^ openEntry .!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' on: ';
		display: self.!

readEntries
	"private -- read all the entries from the input stream and add them to our
	entry lists.
	Leaves the stream positioned just after the last entry (so it's either just before
	the EOF marker, or at the end of the stream depending on whether there is	
	an EOF marker"

	| entry |

	self initializeEntryLists.

	[stream atEnd ifTrue: [^ self].
	entry := TarFileEntry from: stream.
	entry isEndMarker] whileFalse:
		[self addEntry: entry.
		entry skip].

	stream position: entry positionInStream.!

readStream: aStream
	"set the stream that we are reading from"

	stream := aStream.
	self readEntries.!

readWriteStream: aBinaryReadWriteStream
	"set the stream that we are appending to.
	Note, this will #reset the stream and then read as many entries
	as possible, and then be prepared to stick new ones on the end"

	stream := aBinaryReadWriteStream.

	"have to start by reading the existing entries, the next item we
	add will start overwriting the EOF marker (if any), and then when
	we are #closed or #flushed, we will write a new EOF then"
	aBinaryReadWriteStream reset.
	self readEntries.
	nextWritePosition := aBinaryReadWriteStream position.

	"note that we don't set the modifiedf flag here -- the existing
	EOF marker is OK until we actually add anything"
!

select: a1Block
	"answer a list of the entries we contain for which a1Block evaluates to true"

	^ entries select: a1Block.!

select: a1Block thenDo: another1Block
	"evaluate a1Block for each entry we contain for which a1Block evaluates
	to true"

	entries do: [:each | (a1Block value: each) ifTrue: [another1Block value: each]].!

size
	"answer how many file or directory entries this TAR file contains (does not
	include the EOF marker)"

	^ entries size.!

stream
	"answer the Stream we are reading/writing"

	^ stream.!

withNewEntry: aString do: a1Block
	"open a new entry and then evaluate a1Block ensuring that we close it again
	afterwards.
	Answers the result of evauating the block"

	self openEntry: aString.
	^ [a1Block value: openEntry] ensure: [self closeEntry].!

writeEOFMarker
	"private -- write a TAR file EOF marker to our stream"

	(TarFileEntry newAt: nextWritePosition on: stream)
		beEndMarker;
		write;
		yourself.

	"NB: we don't update nextWritePosition since we want the next
	added entry (if any) to overwrite the EOF marker"!

writeStream: aBinaryWriteStream
	"set the stream that we are writing to.
	We will write to the stream starting at its current position"

	stream := aBinaryWriteStream.

	self initializeEntryLists.
	nextWritePosition := aBinaryWriteStream position.

	"technically, an empty file is a valid TAR file, but we always add an EOF marker, even if no entries
	are added, so we treat ourself as modified from the off"
	modified := true.
! !
!TarFile categoriesFor: #addEntry:!accessing!private! !
!TarFile categoriesFor: #at:!accessing!public! !
!TarFile categoriesFor: #at:ifAbsent:!accessing!public! !
!TarFile categoriesFor: #at:ifPresent:!accessing!public! !
!TarFile categoriesFor: #close!operations!public!writing! !
!TarFile categoriesFor: #closeEntry!public!writing! !
!TarFile categoriesFor: #createEntry:!helpers!private! !
!TarFile categoriesFor: #defaultToc!constants!private! !
!TarFile categoriesFor: #displayOn:!displaying!public! !
!TarFile categoriesFor: #do:!enumerating!public! !
!TarFile categoriesFor: #entries!accessing!public! !
!TarFile categoriesFor: #fileFormatError:!exceptions!private! !
!TarFile categoriesFor: #fileFormatError:with:!exceptions!private! !
!TarFile categoriesFor: #filename!accessing!public! !
!TarFile categoriesFor: #flush!operations!public!writing! !
!TarFile categoriesFor: #hasOpenEntry!public!testing!writing! !
!TarFile categoriesFor: #includes:!public!testing! !
!TarFile categoriesFor: #initialize!initializing!private! !
!TarFile categoriesFor: #initializeEntryLists!initializing!private! !
!TarFile categoriesFor: #isEmpty!public!testing! !
!TarFile categoriesFor: #name!accessing!public! !
!TarFile categoriesFor: #name:!initializing!private! !
!TarFile categoriesFor: #openEntry!public!writing! !
!TarFile categoriesFor: #openEntry:!public!writing! !
!TarFile categoriesFor: #printOn:!printing!public! !
!TarFile categoriesFor: #readEntries!private!reading! !
!TarFile categoriesFor: #readStream:!initializing!public! !
!TarFile categoriesFor: #readWriteStream:!initializing!public! !
!TarFile categoriesFor: #select:!enumerating!public! !
!TarFile categoriesFor: #select:thenDo:!enumerating!public! !
!TarFile categoriesFor: #size!accessing!public! !
!TarFile categoriesFor: #stream!accessing!public! !
!TarFile categoriesFor: #withNewEntry:do:!public!writing! !
!TarFile categoriesFor: #writeEOFMarker!private!writing! !
!TarFile categoriesFor: #writeStream:!initializing!public! !

!TarFile class methodsFor!

appendingFile: aFilename
	"answer a new instance that will first read from the named file and
	then allow new entries to be added to the end of it.
	Note that from the time when the first entry has been appended,
	to the time when the TarFile is #closed or #flushed, the TAR file's structure
	on disk will be invalid"

	^ (self name: aFilename)
		readWriteStream: (FileStream write: aFilename mode: #append check: true text: false);
		yourself.!

fileFormatError: aString with: anObject
	"trigger a standard TarFileError with the given message and tag"

	TarFileError signal: aString with: anObject.!

fromBytes: aByteArray
	"answer a new instance that will interpret the given bytes as a
	readonly TAR file structure held in memory"

	^ self readStream: (aByteArray readStream).!

fromString: aString
	"answer a new instance that will interpret the given string as a
	readonly TAR file structure held in memory"

	^ self readStream: (aString readStream).!

inMemory
	"answer a new instance that will create a TAR file structure in memory"

	^ self writeStream: (ReadWriteStream with: ByteArray new).!

inMemory: aByteArray
	"answer a new instance that will interpret the given bytes as a
	TAR file structure held in memory and which will append to it as
	necessary (use:
		anInstance stream contents
	to get the resultant extended ByteArray)"

	"it doesn't matter that ReadWriteStream class>>with: is initially positioned
	at the end of the array, since the new instance will #position: the stream
	as necessary"
	^ self readWriteStream: (ReadWriteStream with:aByteArray).!

name: aString
	"answer a new instance that is only initialised, but not yet connected to
	any stream.  It will consider itself to have the given name (often the name
	of a file)"

	^ (self new)
		name: aString;
		yourself.!

new
	"answer a new instance that is only initialised, but not yet connected to
	any stream"

	^ (self basicNew)
		initialize;
		yourself.!

readBZip2File: aFilename
	"read the BZip2 compressed data from the named file, decompress it into
	a memory buffer, and then answer an instance that will interpret that
	data.
	Please note that the entire file is decompressed and copied into memory;
	the file itself will be closed before this returns (unlike, say. #readingFile:),
	but the RAM consumption may be excessive."

	^ self readBZip2File: aFilename text: false.!

readBZip2File: aFilename text: aBool
	"read the BZip2 compressed data from the named file, decompress it into
	a memory buffer, and then answer an instance that will interpret that
	data.
	Please note that the entire file is decompressed and copied into memory;
	the file itself will be closed before this returns (unlike, say. #readingFile:),
	but the RAM consumption may be excessive."

	^ self
		readCompressedFile: aFilename
		text: aBool
		streamClass: BZip2DecompressingReadStream.!

readCompressedFile: aFilename text: aBool streamClass: aClass
	"private -- read compressed data from the named file, decompress it into
	a memory buffer, and then answer an instance that will interpret that
	data.  The type of compression is identified by the class of decompressing
	read stream, aClass.
	Please note that the entire file is decompressed and copied into memory;
	the file itself will be closed before this returns (unlike, say. #readingFile:),
	but the RAM consumption may be excessive."

	| stream data |

	stream := aClass file: aFilename text: aBool.
	data := stream upToEnd.
	stream close.

	^ (self name: aFilename)
		readStream: (data readStream);
		yourself.
!

readGZipFile: aFilename
	"read the GZip compressed data from the named file, decompress it into
	a memory buffer, and then answer an instance that will interpret that
	data.
	Please note that the entire file is decompressed and copied into memory;
	the file itself will be closed before this returns (unlike, say. #readingFile:),
	but the RAM consumption may be excessive."

	^ self readGZipFile: aFilename text: false.!

readGZipFile: aFilename text: aBool
	"read the GZip compressed data from the named file, decompress it into
	a memory buffer, and then answer an instance that will interpret that
	data.
	Please note that the entire file is decompressed and copied into memory;
	the file itself will be closed before this returns (unlike, say. #readingFile:),
	but the RAM consumption may be excessive."

	^ self
		readCompressedFile: aFilename
		text: aBool
		streamClass: InflaterReadStream.
!

readingFile: aFilename
	"answer a new instance that will read the named file in binary mode"

	^ self readingFile: aFilename text: false.!

readingFile: aFilename text: aBool
	"answer a new instance that will read the named file.  The boolean determines
	how we open the file, if we open it as text then entries will be text by default,
	otherwise binary"

	^ (self name: aFilename)
		readStream: (FileStream read: aFilename text: aBool);
		yourself.
!

readStream: aReadStream
	"answer a new instance that will read from the given ReadStream"

	^ (self new)
		readStream: aReadStream;
		yourself.!

readWriteStream: aBinaryReadWriteStream
	"answer a new instance that will first read the entries from the given stream,
	and then be able to append further entries"

	^ (self new)
		readWriteStream: aBinaryReadWriteStream;
		yourself.!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"

	| nul |

	nul := Character codePoint: 0.

	"the following constants are taken from the description of the tar file format in GNU tar.h,
	modified by reading Max Maischein's description of the format at www.wotsit.org"

	(Smalltalk at: #TarFileConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'TAR_BLOCK_SIZE' put: 512;

		"fields in file headers.
		Unless otherwise stated, numbers are octal, left-padded with 0s and nul-terminated.
		Strings are nul-terminated if the null fits"
		at: 'TAR_FILENAME_SIZE'			put: 100;
		at: 'TAR_FILE_MODE_SIZE'		put:    8;
		at: 'TAR_USER_ID_SIZE'			put:    8;
		at: 'TAR_GROUP_ID_SIZE'		put:    8;
		at: 'TAR_FILE_SIZE_SIZE'			put:  12;
		at: 'TAR_MODTIME_SIZE'			put:  12;
		at: 'TAR_CHECKSUM_SIZE'		put:    8;
		at: 'TAR_CHECKSUM_OFFSET'		put:    (100+8+8+8+12+12);		"zero-based"
		at: 'TAR_TYPEFLAG_SIZE'			put:    1;
		at: 'TAR_LINKNAME_SIZE'			put: 100;
		at: 'TAR_MAGIC_SIZE'			put:    8;		"must be TAR_MAGIC + nul-terminator"
		at: 'TAR_USERNAME_SIZE'		put:  32;		"nul-terminated"
		at: 'TAR_GROUPNAME_SIZE'			put:  32;		"nul-terminated"
		at: 'TAR_DEVMAJOR_SIZE'		put:    8;
		at: 'TAR_DEVMINOR_SIZE'		put:    8;
		at: 'TAR_PREFIX_SIZE'			put: 155;

		"magic number(s)"
		at: 'TAR_MAGIC'				put: 'ustar  ';	"with two trailing spaces"
		at: 'TAR_GNU_DUMP'			put: 'GNUtar ';	"used for GNU dump format (whatever that is)"

		"values for typeflag, expressed as characters not strings"
		at: 'TAR_TYPE_REGULAR'		put: $0;			"regular file, preferred value"
		at: 'TAR_TYPE_ALT_REG'			put: nul;		"alternate form for regular file"
		at: 'TAR_TYPE_HARDLINK'		put: $1;			"hard link"
		at: 'TAR_TYPE_SYMLINK'			put: $2;			"soft link"
		at: 'TAR_TYPE_CHAR_SPECIAL'		put: $3;			"character-special device"
		at: 'TAR_TYPE_BLOCK_SPECIAL'	put: $4;			"block-special device"
		at: 'TAR_TYPE_DIRECTORY'		put: $5;			"directory"
		at: 'TAR_TYPE_NAMED_PIPE'		put: $6;			"named pipe"
		at: 'TAR_TYPE_CONTIGUOUS'		put: $7;			"contiguous file"

		"the bits in a file mode (in octal, as is traditional), these, of course will only make sense if you know UNIX"
		at: 'TAR_MODE_SUID'			put: 8r4000;		"set uid"
		at: 'TAR_MODE_SGID'			put: 8r2000;		"set gid"
		at: 'TAR_MODE_SVTX'			put: 8r1000;		"the famous 'sticky bit' !!"
		at: 'TAR_MODE_UREAD'			put: 8r0400;		"user read permission"
		at: 'TAR_MODE_UWRITE'			put: 8r0200;		"user write permission"
		at: 'TAR_MODE_UEXEC'			put: 8r0100;		"user exec permission"
		at: 'TAR_MODE_GREAD'			put: 8r0040;		"same for group"
		at: 'TAR_MODE_GWRITE'			put: 8r0020;
		at: 'TAR_MODE_GEXEC'			put: 8r0010;
		at: 'TAR_MODE_OREAD'			put: 8r0004;		"same for 'other' (i.e. world)"
		at: 'TAR_MODE_OWRITE'			put: 8r0002;
		at: 'TAR_MODE_OEXEC'			put: 8r0001;

		shrink!

writeStream: aWriteStream
	"answer a new instance that will write to the given WriteStream"

	^ (self new)
		writeStream: aWriteStream;
		yourself.!

writingFile: aFilename
	"answer a new instance that will write the named file.
	NB: if the file already exists then it will be truncated and overwritten."

	^ (self name: aFilename)
		writeStream: (FileStream write: aFilename text: false);
		yourself.! !
!TarFile class categoriesFor: #appendingFile:!files!instance creation!public! !
!TarFile class categoriesFor: #fileFormatError:with:!exceptions!public! !
!TarFile class categoriesFor: #fromBytes:!instance creation!memory resident!public! !
!TarFile class categoriesFor: #fromString:!instance creation!memory resident!public! !
!TarFile class categoriesFor: #inMemory!instance creation!memory resident!public! !
!TarFile class categoriesFor: #inMemory:!instance creation!memory resident!public! !
!TarFile class categoriesFor: #name:!instance creation!public! !
!TarFile class categoriesFor: #new!instance creation!public! !
!TarFile class categoriesFor: #readBZip2File:!files!instance creation!public! !
!TarFile class categoriesFor: #readBZip2File:text:!files!instance creation!public! !
!TarFile class categoriesFor: #readCompressedFile:text:streamClass:!files!instance creation!private! !
!TarFile class categoriesFor: #readGZipFile:!files!instance creation!public! !
!TarFile class categoriesFor: #readGZipFile:text:!files!instance creation!public! !
!TarFile class categoriesFor: #readingFile:!files!instance creation!public! !
!TarFile class categoriesFor: #readingFile:text:!files!instance creation!public! !
!TarFile class categoriesFor: #readStream:!instance creation!public! !
!TarFile class categoriesFor: #readWriteStream:!instance creation!public! !
!TarFile class categoriesFor: #rebuildPoolConstants!development!private! !
!TarFile class categoriesFor: #writeStream:!instance creation!public! !
!TarFile class categoriesFor: #writingFile:!files!instance creation!public! !

TarFileEntry guid: (GUID fromString: '{B64F8806-CA1D-4CCF-B4E8-E23BEF6C326B}')!
TarFileEntry comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

One of these represents one of the entries in a TAR file, or in a block of memory formatted in TAR file format.

Tar files are very UNIX-centric, so most of the data in an entry is meaningless on Windows, or is at least difficult to translate into Window''s terms.  In fact, that makes little difference to the code since this package inlcudes no attempt to provide methods for adding *files* to tarfiles, nor for extracting them to disk.  It''s up to you how, or even whether, you try to interpret them.
'!
!TarFileEntry categoriesForClass!Unclassified! !
!TarFileEntry methodsFor!

beArchaic
	"set that we represent an entry in an old TAR file, in the format
	used in v7 UNIX (I think)"

	magic := uname := gname := nil.!

beBlockSpecialMajor: anInteger minor: anotherInteger
	"set that we represent a block-special device with the given major/minor device numbers"

	devmajor := anInteger.
	devminor := anotherInteger.
	type := TAR_TYPE_BLOCK_SPECIAL.!

beCharacterSpecialMajor: anInteger minor: anotherInteger
	"set that we represent a character-special device with the given major/minor device numbers"

	devmajor := anInteger.
	devminor := anotherInteger.
	type := TAR_TYPE_CHAR_SPECIAL.!

beContiguousFile
	"set that we represent a 'contiguous' file"

	type := TAR_TYPE_CONTIGUOUS.!

beDirectory
	"set that we represent a directory"

	type := TAR_TYPE_DIRECTORY.
	(filename isNil or: [filename endsWith: '/']) ifFalse:
		[filename := filename , '/'].!

beEndMarker
	"set that we are the special entry that marks EOF"

	filename := '<<end of archive>>'.

	magic
		:= linkname
		:= uid
		:= gid
		:= uname
		:= gname
		:= type
		:= filetime
		:= mode
		:= size
		:= devmajor
		:= devminor
		:= checksum
			:= nil.

!

beHardLinkTo: aString
	"set that we represent a hard link to the named file.
	Note that there is a limit of 100 characters on the name"

	self check: aString size: TAR_LINKNAME_SIZE field: 'link name'.

	type := TAR_TYPE_HARDLINK.
	linkname := aString.
!

beNamedPipe
	"set that we represent a named pipe"

	type := TAR_TYPE_NAMED_PIPE.!

beSoftLinkTo: aString
	"set that we represent a soft (symbolic) link to the named file.
	Note that there is a limit of 100 characters on the name"

	self check: aString size: TAR_LINKNAME_SIZE field: 'link name'.

	type := TAR_TYPE_SYMLINK.
	linkname := aString.!

binaryContents
	"answer our contents as a (possibly empty) ByteArray or nil if we don't have any"

	^ self contents ifNotNil: [:it | it asByteArray].!

check: aString size: anInteger field: aName
	"private -- check that the given string is OK for a field with the
	given max size"

	aString isEmpty ifTrue: [^ self fileFormatError: (aName , ' must not be empty')].

	"< (not <=) because of nul-termination"
	aString size < anInteger ifFalse: [^ self fileFormatError: (aName , ' must be less than ' , anInteger , ' characters')].!

checksum
	"answer our checksum, note this is the checksum of the *header* and
	cannot be used to validate our contents.
	Will be nil unless we were created by reading from disk, or have been
	written to disk subsequently"

	^ checksum.
!

contents
	"answer our contents as a (possibly empty) String/ByteArray or nil if we don't have any.
	Whether we answer a String or ByteArray is determined by the contents type of our underlying
	stream"

	| contents |

	self hasContents ifFalse: [^ nil].

	self positionStreamAt: (self positionInStream + TAR_BLOCK_SIZE).

	contents := stream next: size.

	"this will consume any padding"
	self skip.

	^ contents.
!

contentsAsBinary: aBool
	"answer our contents or nil if we don't have any.
	The contents will be a ByteArray or a String according as aBool is true"

	^ aBool
		ifTrue: [self binaryContents]
		ifFalse: [self textContents].!

contentsSize
	"answer the size of our contents"

	^ self hasContents
		ifTrue: [size]
		ifFalse: [0].
!

defaultFiletime
	"private -- answer the filetime to use by default"

	^ DefaultFiletime.!

describeOn: aStream
	"append a user-centric description of ourself to aStream.
	We actually write a string that is formatted the same way as UNIX
	tar -tv would print us (but please see the note about daylight saving
	and Windows time formatting in #filetime)"

	| locale time ownString sizeString spaces |

	self isEndMarker ifTrue: [^ self displayOn: filename].

	time := filetime asSYSTEMTIME.
	locale := Locale default.
	ownString := (self unameOrUidString) , '/' , (self gnameOrGidString).
	sizeString := self sizeOrDevicesString.
	spaces := (19 - ownString size - sizeString size) max: 1.
	spaces := (String writeStream)
			next: spaces put: Character space;
			contents.

	aStream
		nextPutAll: self modeString;
		space;
		nextPutAll: ownString;
		nextPutAll: spaces;
		nextPutAll: sizeString;
		space;
		nextPutAll: (locale printSysDate: time format: 'yyyy-MM-dd' flags: 0);
		space;
		nextPutAll: (locale printTime: time format: nil flags: 0);
		space;
		nextPutAll: filename.

	self isLink ifFalse: [^ self].

	aStream
		nextPutAll: (self isHardLink ifTrue: [' link to '] ifFalse: [' -> ']);
		nextPutAll: linkname.!

description
	"answer a user-centric description of ourself"

	| str |

	str := String writeStream: 64 + filename size.
	self describeOn: str.
	^ str contents.!

devmajor
	"answer our major device number"

	^ devmajor.
!

devminor
	"answer our minor device number"

	^ devminor.
!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: filename.
!

fileFormatError: aString
	"private -- trigger a standard TarFile error with the given message"

	self class fileFormatError: aString with: self.
!

filename
	"answer our filename"

	^ filename.
!

filename: aString
	"set our filename to aString.  Note, this should use UNIX forward /s not \s
	If the filename ends with a / then we represent a directory,
	The filename should not start with a / (i.e. should not be an absolute path).
	NB: there is a limit to the size of filenames that can be stored,
	this is 255 characters for the modern TAR file format, 100 characters for
	the archaic form"

"	self assert: [aString notEmpty].		"
"	self assert: [aString first ~= $/].		"
"	self assert: [(aString includes: $\) not].	-- not actually illegal since $\ is a valid UNIX filename character"

	#CUtodo.  "support GNU long filename form"

	self check: aString size: self maxFilenameSize field: 'filename'.

	filename := aString.
	(aString last = $/) ifTrue: [self beDirectory].!

filetime
	"answer our filetime, this is a FILETIME representing the UTC time that
	our file was last modified.  This is stored as a FILETIME here, but the
	external representation only has 1 second granularity.
	Note also that, although the FILETIME does accurately represent the
	UNIX-style time, Windows may *display* it inaccurately if daylight saving
	time (such as BST) is currently in effect since Windows wrongly applies the
	daylight saving offset to all times irrespective of whether daylight saving
	*was* in effect at that time"

	^ filetime.
!

filetime: aFILETIME
	"set our filetime, this is a FILETIME representing the UTC time that
	our file was last modified.  This is stored as a FILETIME here, but the
	external representation only has 1 second granularity"

	filetime := aFILETIME asUTC.
!

gid
	"answer our gid (group identifier)"

	^ gid.
!

gid: anInteger
	"set our gid (group identifier)"

	gid := anInteger.
!

gname
	"answer the name of our owning group"

	^ gname.
!

gname: aString
	"set the name of our owning group"

	self check: aString size: TAR_GROUPNAME_SIZE field: 'group name'.

	gname := aString.
!

gnameOrGidString
	"answer a String representing our group name or, if we don't
	have one, the string representation of our gid.
	This is really only usefull for display purposes"

	^ gname ifNil: [gid displayString].!

groupId: anInteger name: aString
	"set our group identifier (gid) and name"

	self
		gid: anInteger;
		gname: aString.!

hasBeenWritten
	"answer whether this entry has yet been written to disk"

	"hacky -- instances that have not been written have nil checksums"
	^ checksum notNil.!

hasContents
	"answer whether this entry has or can have contents (directories do
	not, zero-length files *do*)"

	^ self isRegularFile or: [self isContiguousFile].
!

initialize
	"private -- establish a coherent initial state"

	"we set up to be a regular file, owned by root, of zero size, but without a legal name"
	magic := TAR_MAGIC.
	filename := ''.
	linkname := ''.
	uid := 0.
	gid := 0.
	uname := 'root'.
	gname := 'root'.
	type := TAR_TYPE_REGULAR.
	filetime := self defaultFiletime.
	mode := 8r777.
	size := 0.
	devmajor := 0.
	devminor := 0.
	checksum := nil.
!

isArchaic
	"answer whether we represent an entry in an old TAR file, in the format
	used in v7 UNIX (I think)"

	^ magic isNil.!

isBlockSpecial
	"answer whether we represent a block-special device"

	^ type = TAR_TYPE_BLOCK_SPECIAL.!

isCharacterSpecial
	"answer whether we represent a character-special device"

	^ type = TAR_TYPE_CHAR_SPECIAL.!

isContiguousFile
	"answer whether we represent a 'contiguous' file (are there any such ??)"

	^ type = TAR_TYPE_CONTIGUOUS.!

isDevice
	"answer whether we represent a character-special or block-special device"

	^ self isCharacterSpecial or: [self isBlockSpecial].!

isDirectory
	"answer whether we represent a directory"

	^ type = TAR_TYPE_DIRECTORY.!

isEndMarker
	"answer whether we are the special EOF marker that *may* be used
	to mark the end of the tar file"

	^ type isNil.!

isHardLink
	"answer whether we represent a hard link"

	^ type = TAR_TYPE_HARDLINK.!

isLink
	"answer whether we represent a (hard or soft) link"

	^ self isHardLink or: [self isSoftLink].!

isNamedPipe
	"answer whether we represent a named pipe"

	^ type = TAR_TYPE_NAMED_PIPE.!

isRegularFile
	"answer whether we represent a regular file"

	"hacky test to avoid messing around.
	NB: the spec requires us to treat unknown types as regular files"
	^ ('1234567' includes: type) not.!

isSoftLink
	"answer whether we represent a soft (symbolic) link"

	^ type = TAR_TYPE_SYMLINK.!

linkname
	"answer our linkname, i.e. the name of the file to which we represent
	a soft (symbolic) link"

	^ linkname.
!

magic
	"answer our magic identifying string"

	^ magic.
!

maxFilenameSize
	"answer the maximum size of filenames that can be stored, this is 255 characters
	for the modern TAR file format, 100 characters for the archaic form"

	^ self isArchaic
		ifTrue: [TAR_FILENAME_SIZE]
		ifFalse: [TAR_FILENAME_SIZE + TAR_PREFIX_SIZE].!

mode
	"answer our mode/permissions bitmap.
	NB: this is not the same as the numeric value that can be uses with UNIX chmod(1) but
	is a combination of the TAR_MODE_XXX constants in the TarFileConstants pool"

	^ mode.
!

mode: anInteger
	"set our mode/permissions bitmap.
	NB: this is not the same as the numeric value that can be uses with UNIX chmod(1) but
	is a combination of the TAR_MODE_XXX constants in the TarFileConstants pool"

	mode := anInteger.
!

modeString
	"answer a String representing our mode bits.
	This is really only usefull for display purposes"

	^ (String writeStream)
		nextPut: ('-lcbdp?' at: type digitValue ifAbsent: [$-]);
		nextPut: ((mode allMask: TAR_MODE_UREAD) ifTrue: [$r] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_UWRITE) ifTrue: [$w] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_SUID)
					ifTrue: [(mode allMask: TAR_MODE_UEXEC) ifTrue: [$s] ifFalse: [$S]]
					ifFalse: [(mode allMask: TAR_MODE_UEXEC) ifTrue: [$x] ifFalse: [$-]]);
		nextPut: ((mode allMask: TAR_MODE_GREAD) ifTrue: [$r] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_GWRITE) ifTrue: [$w] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_SGID)
					ifTrue: [(mode allMask: TAR_MODE_GEXEC) ifTrue: [$s] ifFalse: [$S]]
					ifFalse: [(mode allMask: TAR_MODE_GEXEC) ifTrue: [$x] ifFalse: [$-]]);
		nextPut: ((mode allMask: TAR_MODE_OREAD) ifTrue: [$r] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_OWRITE) ifTrue: [$w] ifFalse: [$-]);
		nextPut: ((mode allMask: TAR_MODE_SVTX)
					ifTrue: [(mode allMask: TAR_MODE_OEXEC) ifTrue: [$t] ifFalse: [$T]]
					ifFalse: [(mode allMask: TAR_MODE_OEXEC) ifTrue: [$x] ifFalse: [$-]]);
		nextPut: ((mode allMask: TAR_MODE_OEXEC) ifTrue: [$x] ifFalse: [$-]);
		contents.
!

populateFrom: aReadStream
	"private -- populate our instance data by reading from the given stream.
	Throws errors if the checksum doesn't make sense"

	| block bytes checksumBytes headers prefix sum gnuLongFilename |

	block := aReadStream next: TAR_BLOCK_SIZE.
	bytes := block asByteArray.

	(bytes allSatisfy: [:each | each = 0]) ifTrue:
		[self beEndMarker.
		^ self].

	headers := block asString readStream.
	filename := self readStringFrom: headers max: TAR_FILENAME_SIZE.
	mode := self readModeFrom: headers max: TAR_FILE_MODE_SIZE.
	uid := self readNumberFrom: headers max: TAR_USER_ID_SIZE.
	gid := self readNumberFrom: headers max: TAR_GROUP_ID_SIZE.
	size := self readNumberFrom: headers max: TAR_FILE_SIZE_SIZE.
	filetime := self readFiletimeFrom: headers max: TAR_MODTIME_SIZE.
	checksum := self readNumberFrom: headers max: TAR_CHECKSUM_SIZE.
	type := self readTypeFrom: headers.
	linkname := self readStringFrom: headers max: TAR_LINKNAME_SIZE.
	magic := self readStringFrom: headers max: TAR_MAGIC_SIZE.
	uname := self readStringFrom: headers max: TAR_USERNAME_SIZE.
	gname := self readStringFrom: headers max: TAR_GROUPNAME_SIZE.
	devmajor := self readNumberFrom: headers max: TAR_DEVMAJOR_SIZE.
	devminor := self readNumberFrom: headers max: TAR_DEVMINOR_SIZE.
	prefix := self readStringFrom: headers max: TAR_PREFIX_SIZE.

	uname isEmpty ifTrue: [uname := nil].
	gname isEmpty ifTrue: [gname := nil].
	magic isEmpty ifTrue: [magic := uname := gname := nil].
	prefix isEmpty ifFalse: [filename := prefix , filename].

	bytes
		replaceFrom: TAR_CHECKSUM_OFFSET+1
		to: TAR_CHECKSUM_OFFSET+TAR_CHECKSUM_SIZE
		withObject: Character space codePoint.
	sum := bytes inject: 0 into: [:acc :each | acc + each].
	(checksum = sum) ifFalse: [self fileFormatError: 'Bad checksum'].

	"allow for bloody GNU incompatabilities (again!!)"
	type = $L ifTrue:
		[gnuLongFilename := self textContents trimNulls.
		self skip.
		positionInStream := aReadStream position.
		self populateFrom: aReadStream.
		filename := gnuLongFilename].!

positionInStream
	"answer the position in the stream where this record started"

	^ positionInStream.!

positionInStream: anInteger
	"private -- set the position in the stream where this record started"

	positionInStream := anInteger.!

positionOfNextInStream
	"answer the position in the stream where the next record after this
	should start"

	^ positionInStream + self recordSize.!

positionStreamAt: anInteger
	"private -- set the position of our underlying stream"

	"note that the test for being already in the correct place means that
	we can read from streams that understand #position but do not
	allow (or restrict) #position:.  (Its faster too!!)"
	stream position = anInteger ifFalse: [stream position: anInteger].!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' (';
		display: self;
		nextPutAll: ')'.!

readData
	"private -- populate our instance data by reading the next entry from our stream.
	Throws errors if the contents' checksum doesn't match"

	self populateFrom: stream.!

readFiletimeFrom: aReadStream max: anInteger
	"private -- read a time expressed as seconds since the UNIX epoch (1st Jan 1970) from the given stream"

	^ FILETIME fromUnixTime: (self readNumberFrom: aReadStream max: anInteger).!

readModeFrom: aReadStream max: anInteger
	"private -- read a file permissions/mode-bitset from the given stream"

	^ 8r7777 bitAnd: (self readNumberFrom: aReadStream max: anInteger).!

readNext
	"read and answer the next entry from our stream or nil if we are at EOF.
	Note that this differs from the class-side newFrom: method in that it knows
	to skip over any unread contents of *this* entry in the stream, and in that
	it will suppress EOF markers"

	| next |

	"this will position the stream after this entry and any padding"
	self skip.

	stream atEnd ifTrue: [^ nil].

	next := self class from: stream.

	^ next isEndMarker
		ifTrue: [nil]
		ifFalse: [next].!

readNumberFrom: aReadStream max: anInteger
	"private -- read an octal number of the given length from the given stream, the data may be nul-terminated
	or may be terminated by the maximum number of bytes given"

	| answer |

	answer := 0.
	(self readStringFrom: aReadStream max: anInteger) trimBlanks do:
		[:ch | 
		answer := answer * 8 + ch digitValue].

	^ answer.!

readStringFrom: aReadStream max: anInteger
	"private -- read a String of the given length from the given stream, the string may be nul-terminated
	or may be terminated by the maximum number of bytes given"

	^ (aReadStream next: anInteger) trimNulls.!

readTypeFrom: aReadStream
	"private -- read a file type from the given stream"

	^ aReadStream next.!

recordSize
	"answer the number of bytes we take up in the stream, including padding"

	^ (TAR_BLOCK_SIZE + self contentsSize) roundUpTo: TAR_BLOCK_SIZE.!

shortName
	"answer the last segment of our filename"

	| index |

	"note that we leave the trailing '/' on the end of directory names"
	index := filename prevIndexOf: $/ from: filename size - 1 to: 1.
	^ index = 0
		ifTrue: [filename]
		ifFalse: [filename copyFrom: index + 1].!

sizeOrDevicesString
	"answer a String representing our size or, if we stand
	for a special device, our device numbers.
	This is really only usefull for display purposes"

	^ self isDevice
		ifTrue: ['%d,%d' sprintfWith: devmajor with: devminor]
		ifFalse: [self size displayString].!

skip
	"skip over any contents we may have (has no effect if we have
	already read our contents"

	self positionStreamAt: self positionOfNextInStream.
!

stream
	"answer the Stream we are reading/writing"

	^ stream.!

stream: aStream
	"private -- set the stream from which we were read or to which we will write"

	stream := aStream.!

textContents
	"answer our contents as a (possibly empty) String or nil if we don't have any"

	^ self contents ifNotNil: [:it | it asString].!

timestampNow
	"set the last modified date and time to the current values taken from the
	system"

	self filetime: FILETIME now.!

type
	"answer the type of file we represent, this is a Character from the
	values in TAR_TYPE_*"

	^ type.
!

typeName
	"answer the Symbol name of the type of file we represent"

	^ type
		ifNil: [#EndMarker]
		ifNotNil: [:it | self class fileType: it].
!

uid
	"answer our uid (user identifier)"

	^ uid.
!

uid: anInteger
	"set our uid (user identifier)"

	uid := anInteger.
!

uname
	"answer the name of our owning user"

	^ uname.
!

uname: aString
	"set the name of our owning user"

	self check: aString size: TAR_USERNAME_SIZE field: 'user name'.

	uname := aString.
!

unameOrUidString
	"answer a String representing our user name or, if we don't
	have one, the string representation of our uid.
	This is really only usefull for display purposes"

	^ uname ifNil: [uid displayString].!

userId: anInteger name: aString
	"set our user identifier (uid) and name"

	self
		uid: anInteger;
		uname: aString.!

withBinaryReaderDo: a1Block
	"evaluate the <monadicValuable> a1Block passing a binary ReadStream that can be used to read the
	contents of this entry.  If we do not have contents (we represent a directory) then a1Block is not evaluated.
	If we have contents, but the contents are zero-length (an empty file) then the ReadStream will already be #atEnd"

	self binaryContents ifNotNil: [:it | a1Block value: it readStream].
!

withBinaryWriterDo: a1Block
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a binary WriteStream that
	can be used to write the contents of this entry (not called if we don't have data), and finally clean up by ensuring
	that the sizes and checksums on disk are correct"

	self withWriterDo: a1Block binary: true.!

withReaderDo: a1Block
	"evaluate the <monadicValuable> a1Block passing a ReadStream that can be used to read the
	contents of this entry.  The stream will be in text or binary mode according as our underlying stream.
	If we do not have contents (we represent a directory) then a1Block is not evaluated.
	If we have contents, but the contents are zero-length (an empty file) then the ReadStream will already be #atEnd"

	self contents ifNotNil: [:it | a1Block value: it readStream].!

withReaderDo: a1Block binary: aBool
	"evaluate the <monadicValuable> a1Block passing a ReadStream that can be used to read the
	contents of this entry.  The stream will be in text or binary mode according to aBool.
	If we do not have contents (we represent a directory) then a1Block is not evaluated.
	If we have contents, but the contents are zero-length (an empty file) then the ReadStream will already be #atEnd"

	aBool
		ifTrue: [self withBinaryReaderDo: a1Block]
		ifFalse: [self withTextReaderDo: a1Block].
!

withTextReaderDo: a1Block
	"evaluate the <monadicValuable> a1Block passing a text ReadStream that can be used to read the
	contents of this entry.  If we do not have contents (we represent a directory) then a1Block is not evaluated.
	If we have contents, but the contents are zero-length (an empty file) then the ReadStream will already be #atEnd"

	self textContents ifNotNil: [:it | a1Block value: it readStream].
!

withTextWriterDo: a1Block
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a text WriteStream that
	can be used to write the contents of this entry (not called if we don't have data), and finally clean up by ensuring
	that the sizes and checksums on disk are correct"

	self withWriterDo: a1Block binary: false.!

withWriterDo: a1Block binary: aBool
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a WriteStream that
	can be used to write the contents of this entry (not called if we don't have data), and finally clean up by ensuring
	that the sizes and checksums on disk are correct"

	| writer |

	self hasContents ifFalse: [^ self write].

	"despite the above comment, for now, we don't have the ability to write incrementally so we work out what to write first"
	writer := (aBool ifTrue: [ByteArray] ifFalse: [String])
			writeStream.
	a1Block value: writer.
	self write: writer contents.!

write
	"write ourself, followed by NO contents, on our stream, ensuring that the sizes and checksums on disk are correct"

	"this is only a safe implementation because we know that
	#write: has the necessary checks for empty arrays"
	self write: #[].

!

write: aByteArrayOrString
	"write a file entry followed by the given contents on our stream"

	self writeBinary: aByteArrayOrString asByteArray.!

writeBinary: aByteArray
	"write a file entry followed by the given contents on our stream.
	This is actually the bottom level of the layers of #write: methods, that's
	because we 'know' that our underlying stream is binary"

	| block headers prefix tail padding |

	self assert: [self hasContents or: [aByteArray isEmpty]].
	size := aByteArray size.

	self positionStreamAt: self positionInStream.

	self isEndMarker ifTrue:
		[checksum := size := 0.
		stream next: TAR_BLOCK_SIZE put: 0.
		^ self].

	filename size > TAR_FILENAME_SIZE
		ifTrue:
			[prefix := filename allButLast: TAR_FILENAME_SIZE.
			tail := filename last: TAR_FILENAME_SIZE]
		ifFalse:
			[prefix := nil.
			tail := filename].

	block := String new: TAR_BLOCK_SIZE.
	headers := block writeStream.

	self
		writeString: tail on: headers max: TAR_FILENAME_SIZE;
		writeNumber: mode on: headers max: TAR_FILE_MODE_SIZE;
		writeNumber: uid on: headers max: TAR_USER_ID_SIZE;
		writeNumber: gid on: headers max: TAR_GROUP_ID_SIZE;
		writeNumber: size on: headers max: TAR_FILE_SIZE_SIZE;
		writeFiletime: filetime on: headers max: TAR_MODTIME_SIZE;
		writeSpaces: TAR_CHECKSUM_SIZE on: headers;
		writeType: type on: headers;
		writeString: linkname on: headers max: TAR_LINKNAME_SIZE;
		writeString: magic on: headers max: TAR_MAGIC_SIZE;
		writeString: uname on: headers max: TAR_USERNAME_SIZE;
		writeString: gname on: headers max: TAR_GROUPNAME_SIZE;
		writeNumber: devmajor on: headers max: TAR_DEVMAJOR_SIZE;
		writeNumber: devminor on: headers max: TAR_DEVMINOR_SIZE;
		writeString: prefix on: headers max: TAR_PREFIX_SIZE.

	checksum := block inject: 0 into: [:acc :each | acc + each codePoint].
	headers position: TAR_CHECKSUM_OFFSET.
	self writeNumber: checksum on: headers max: TAR_CHECKSUM_SIZE.

	#CUtodo. "create write equivalent of the following reading code"
	"bloody GNU incompatabilities (again!!)"
"	type = $L ifTrue:
		[gnuLongFilename := self textContents trimNulls.
		self skip.
		positionInStream := stream position.
		self populateFrom: stream.
		filename := gnuLongFilename].
"
	stream
		nextPutAll: block asByteArray;
		nextPutAll: aByteArray.

	padding := self positionOfNextInStream - stream position.
	stream next: padding put: 0.!

writeFiletime: aFILETIME on: aStream max: anInteger
	"private -- write the given FILETIME on aStream formatted as a number
	counting the number of seconds since the UNIX epoch"

	self writeNumber: aFILETIME unixTime on: aStream max: anInteger
!

writeNumber: anInteger on: aStream max: aSize
	"private -- write anInteger on the given stream in octal, padding out to aSize
	with nulls"

	self
		writeString: ('%o' sprintfWith: anInteger)
		on: aStream
		max: aSize.!

writeSpaces: anInteger on: aStream
	"private -- write the given number of spaces on aStream"

	aStream next: anInteger put: Character space.
!

writeString: aString on: aStream max: anInteger
	"private -- write the given String on the given stream, padding with nulls up to the given maximum"

	self assert: [aString size <= anInteger].

	aStream
		nextPutAll: (aString ifNil: ['']);
		next: (anInteger - aString size) put: Character null.!

writeText: aString
	"write a file entry followed by the given contents on our stream"

	self writeBinary: aString asByteArray.!

writeType: aCharacter on: aStream
	"private -- write the given type code on aStream"

	aStream nextPut: aCharacter.! !
!TarFileEntry categoriesFor: #beArchaic!public!record types! !
!TarFileEntry categoriesFor: #beBlockSpecialMajor:minor:!public!record types! !
!TarFileEntry categoriesFor: #beCharacterSpecialMajor:minor:!public!record types! !
!TarFileEntry categoriesFor: #beContiguousFile!public!record types! !
!TarFileEntry categoriesFor: #beDirectory!public!record types! !
!TarFileEntry categoriesFor: #beEndMarker!public!record types! !
!TarFileEntry categoriesFor: #beHardLinkTo:!public!record types! !
!TarFileEntry categoriesFor: #beNamedPipe!public!record types! !
!TarFileEntry categoriesFor: #beSoftLinkTo:!public!record types! !
!TarFileEntry categoriesFor: #binaryContents!accessing!public!reading! !
!TarFileEntry categoriesFor: #check:size:field:!exceptions!private! !
!TarFileEntry categoriesFor: #checksum!accessing!public! !
!TarFileEntry categoriesFor: #contents!accessing!public!reading! !
!TarFileEntry categoriesFor: #contentsAsBinary:!public!reading! !
!TarFileEntry categoriesFor: #contentsSize!accessing!public! !
!TarFileEntry categoriesFor: #defaultFiletime!constants!private! !
!TarFileEntry categoriesFor: #describeOn:!displaying!public! !
!TarFileEntry categoriesFor: #description!displaying!public! !
!TarFileEntry categoriesFor: #devmajor!accessing!public! !
!TarFileEntry categoriesFor: #devminor!accessing!public! !
!TarFileEntry categoriesFor: #displayOn:!displaying!public! !
!TarFileEntry categoriesFor: #fileFormatError:!exceptions!private! !
!TarFileEntry categoriesFor: #filename!accessing!public! !
!TarFileEntry categoriesFor: #filename:!accessing!public! !
!TarFileEntry categoriesFor: #filetime!accessing!public! !
!TarFileEntry categoriesFor: #filetime:!accessing!public! !
!TarFileEntry categoriesFor: #gid!accessing!public! !
!TarFileEntry categoriesFor: #gid:!accessing!public! !
!TarFileEntry categoriesFor: #gname!accessing!public! !
!TarFileEntry categoriesFor: #gname:!accessing!public! !
!TarFileEntry categoriesFor: #gnameOrGidString!displaying!public! !
!TarFileEntry categoriesFor: #groupId:name:!accessing!public! !
!TarFileEntry categoriesFor: #hasBeenWritten!public!testing!writing! !
!TarFileEntry categoriesFor: #hasContents!public!record types!testing! !
!TarFileEntry categoriesFor: #initialize!initializing!private! !
!TarFileEntry categoriesFor: #isArchaic!public!record types!testing! !
!TarFileEntry categoriesFor: #isBlockSpecial!public!record types!testing! !
!TarFileEntry categoriesFor: #isCharacterSpecial!public!record types!testing! !
!TarFileEntry categoriesFor: #isContiguousFile!public!record types!testing! !
!TarFileEntry categoriesFor: #isDevice!public!record types!testing! !
!TarFileEntry categoriesFor: #isDirectory!public!record types!testing! !
!TarFileEntry categoriesFor: #isEndMarker!public!record types!testing! !
!TarFileEntry categoriesFor: #isHardLink!public!record types!testing! !
!TarFileEntry categoriesFor: #isLink!public!record types!testing! !
!TarFileEntry categoriesFor: #isNamedPipe!public!record types!testing! !
!TarFileEntry categoriesFor: #isRegularFile!public!record types!testing! !
!TarFileEntry categoriesFor: #isSoftLink!public!record types!testing! !
!TarFileEntry categoriesFor: #linkname!accessing!public! !
!TarFileEntry categoriesFor: #magic!accessing!public! !
!TarFileEntry categoriesFor: #maxFilenameSize!accessing!public! !
!TarFileEntry categoriesFor: #mode!accessing!public! !
!TarFileEntry categoriesFor: #mode:!accessing!public! !
!TarFileEntry categoriesFor: #modeString!displaying!public! !
!TarFileEntry categoriesFor: #populateFrom:!private!reading! !
!TarFileEntry categoriesFor: #positionInStream!accessing!positioning!public! !
!TarFileEntry categoriesFor: #positionInStream:!initializing!positioning!private! !
!TarFileEntry categoriesFor: #positionOfNextInStream!positioning!public! !
!TarFileEntry categoriesFor: #positionStreamAt:!positioning!private! !
!TarFileEntry categoriesFor: #printOn:!printing!public! !
!TarFileEntry categoriesFor: #readData!private!reading! !
!TarFileEntry categoriesFor: #readFiletimeFrom:max:!private!reading! !
!TarFileEntry categoriesFor: #readModeFrom:max:!private!reading! !
!TarFileEntry categoriesFor: #readNext!public!reading! !
!TarFileEntry categoriesFor: #readNumberFrom:max:!private!reading! !
!TarFileEntry categoriesFor: #readStringFrom:max:!private!reading! !
!TarFileEntry categoriesFor: #readTypeFrom:!private!reading! !
!TarFileEntry categoriesFor: #recordSize!accessing!positioning!public! !
!TarFileEntry categoriesFor: #shortName!accessing!public! !
!TarFileEntry categoriesFor: #sizeOrDevicesString!displaying!public! !
!TarFileEntry categoriesFor: #skip!positioning!public!reading! !
!TarFileEntry categoriesFor: #stream!accessing!public! !
!TarFileEntry categoriesFor: #stream:!initializing!private! !
!TarFileEntry categoriesFor: #textContents!accessing!public!reading! !
!TarFileEntry categoriesFor: #timestampNow!operations!public! !
!TarFileEntry categoriesFor: #type!accessing!public!record types! !
!TarFileEntry categoriesFor: #typeName!accessing!public!record types! !
!TarFileEntry categoriesFor: #uid!accessing!public! !
!TarFileEntry categoriesFor: #uid:!accessing!public! !
!TarFileEntry categoriesFor: #uname!accessing!public! !
!TarFileEntry categoriesFor: #uname:!accessing!public! !
!TarFileEntry categoriesFor: #unameOrUidString!displaying!public! !
!TarFileEntry categoriesFor: #userId:name:!accessing!public! !
!TarFileEntry categoriesFor: #withBinaryReaderDo:!public!reading! !
!TarFileEntry categoriesFor: #withBinaryWriterDo:!public!writing! !
!TarFileEntry categoriesFor: #withReaderDo:!public!reading! !
!TarFileEntry categoriesFor: #withReaderDo:binary:!public!reading! !
!TarFileEntry categoriesFor: #withTextReaderDo:!public!reading! !
!TarFileEntry categoriesFor: #withTextWriterDo:!public!writing! !
!TarFileEntry categoriesFor: #withWriterDo:binary:!public!writing! !
!TarFileEntry categoriesFor: #write!public!writing! !
!TarFileEntry categoriesFor: #write:!public!writing! !
!TarFileEntry categoriesFor: #writeBinary:!public!writing! !
!TarFileEntry categoriesFor: #writeFiletime:on:max:!private!writing! !
!TarFileEntry categoriesFor: #writeNumber:on:max:!private!writing! !
!TarFileEntry categoriesFor: #writeSpaces:on:!private!writing! !
!TarFileEntry categoriesFor: #writeString:on:max:!private!writing! !
!TarFileEntry categoriesFor: #writeText:!public!writing! !
!TarFileEntry categoriesFor: #writeType:on:!private!writing! !

!TarFileEntry class methodsFor!

fileFormatError: aString with: anObject
	"trigger a standard TarFileError with the given message and tag"

	TarFileError signal: aString with: anObject.!

fileType: aCharacter
	"answer the Symbol name of the filetype associated with the given
	character"

	"as per spec, we treat all file types that we don't recognise as regular files"
	^ FileTypes at: aCharacter ifAbsent: [#RegularFile].!

from: aReadStream
	"answer a new instance populated with data read from aReadStream.
	Note that the answered instance may be an EOF marker if (as is normally
	the case) the tar file uses an explicit EOF marker"

	^ (self newAt: aReadStream position on: aReadStream)
		readData;
		yourself.!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	"this is not only the default, but also the earliest time we can represent"
	DefaultFiletime := FILETIME fromUnixTime: 0.

	FileTypes := (LookupTable new)
			at: $0 put: #RegularFile;
		"	at: nul put: #RegularFile;			"
			at: $1 put: #Hardlink;
			at: $2 put: #Symlink;
			at: $3 put: #CharacterSpecial;
			at: $4 put: #BlockSpecial;
			at: $5 put: #Directory;
			at: $6 put: #NamedPipe;
			at: $7 put: #ContiguousFile;
			shrink;
			yourself.!

new
	"answer a new instance with default initialisation"

	^ (self basicNew)
		initialize;
		yourself.!

newAt: anInteger on: aReadStream
	"answer a new instance that knows that it will be found at the
	given offset on the given stream"

	^ (self new)
		stream: aReadStream;
		positionInStream: anInteger;
		yourself.! !
!TarFileEntry class categoriesFor: #fileFormatError:with:!exceptions!public! !
!TarFileEntry class categoriesFor: #fileType:!helpers!public! !
!TarFileEntry class categoriesFor: #from:!instance creation!public! !
!TarFileEntry class categoriesFor: #initialize!initializing!private! !
!TarFileEntry class categoriesFor: #new!instance creation!public! !
!TarFileEntry class categoriesFor: #newAt:on:!instance creation!public! !

TarFileError guid: (GUID fromString: '{9A3A80BC-EB78-4980-82A9-BA587BCF9203}')!
TarFileError comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org
'!
!TarFileError categoriesForClass!Unclassified! !
"Binary Globals"!

"Resources"!

