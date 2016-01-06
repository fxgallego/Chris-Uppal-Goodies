| package |
package := Package name: 'CU ZipFiles'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Uses the ZLib streams to encode or decode ZIP files.  It is particularly aimed at manipulating such files in memory without having to extract files to disk to read them.  Similarly you can add data directly to a Zip file without it having to be written to an external file first.

We have only limited abilities to handle Zip files.  We don''t handle encryption, and we only understand "Deflate" (type 8) compression which is what zlib provides.  We do also understand "Strored" (type 0, uncompressed) entries as well.   There are several other compression formats that are part of the Zip file specification and which can be used by the original PKWare utilities.  However these formats seem to be rarely used -- I have never seen them myself.  One last restriiction is that we have no support for multi-volume archives (but does anyone use those anymore ?).

Our ability to manipulate Zip format is limited to creating files (as real files on disk, or as binary data in memory) and to reading existing files.  We can also append new data to an exiting file, but do not have the ability to make arbitrary changes in-place. (though it is easy enough to create a copy, applying any desired modifications as you go).

See the class comments for ZipFile and ZipFileEntry for more details.

The document "InfoZip\appnote.txt" in the Docs folder contains the version of the Zip file spec that I was working from.  It is the *unoffical* Info-Zip version of the real spec provided by PKWare.  See the Info-Zip website (http://www.info-zip.org/) for more information about Info-Zip.  Note that this package was written from the spec, but it does not include, or link to, Info-Zip or PKWare code.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.


	-- chris


History:

3.00  *** WARNING: includes (minor) breaking changes ***
-	Changed the contract of ZipFileEntry>>withReaderDo:binary: (and the derived methods).
	If the entry represents a directory then the block is no longer called at all (previously
	it was called with a null argument, which was a silly bit of mis-design on my part -- sorry!!).
-	Renamed ZipFile instance creation methods #read: to #readingFile: (and #write: and #append: similarly)
	to make it clear that the file is held open. 
	The old names are now deprecated and will vanish in the next release
-	Made ZipFileEntryBase>>withBinaryReaderDo: slightly more robust.
-	Removed some method from ZipFileEntryBase that shouldn''t have been there and never worked.
-	Moved some methods around the ZipFileEntry hierarchy to get a cleaner factoring.

2.00
-	Fixed silly bug in ZipFileEntry>>withBinaryReaderDo:.

1.00
-	First release.'.

package basicPackageVersion: '3.00'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU ZipFiles'')
	propertyAt: #ExternalResourceFileNames
	put: #(''Docs\InfoZip\appnote.txt'').
!!'.

package classNames
	add: #ZipFile;
	add: #ZipFileCentralDirectoryEnd;
	add: #ZipFileCentralDirectoryEntry;
	add: #ZipFileDataDescriptor;
	add: #ZipFileElement;
	add: #ZipFileEntry;
	add: #ZipFileEntryBase;
	add: #ZipFileError;
	yourself.

package globalNames
	add: #ZipFileConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Null Streams';
	add: 'CU Sortblocks';
	add: 'CU Stream Extensions';
	add: 'CU ZLib Base';
	add: 'CU ZLib Streams';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package setManualPrerequisites: #(
	'CU Stream Extensions').

package!

"Class Definitions"!

Object subclass: #ZipFile
	instanceVariableNames: 'stream name toc centralDirectoryEntries centralDirectoryEnd modified nextWritePosition entryCompressionMethod entryCompressionLevel openCDEntry'
	classVariableNames: ''
	poolDictionaries: 'ZipFileConstants ZLib1Constants'
	classInstanceVariableNames: ''!
Object subclass: #ZipFileElement
	instanceVariableNames: 'stream positionInStream'
	classVariableNames: ''
	poolDictionaries: 'ZipFileConstants'
	classInstanceVariableNames: ''!
Error subclass: #ZipFileError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZipFileElement subclass: #ZipFileCentralDirectoryEnd
	instanceVariableNames: 'diskNumber directoryDiskNumber entriesInDirectoryOnThisDisk entriesInAllDirectories directorySize directoryOffset comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZipFileElement subclass: #ZipFileDataDescriptor
	instanceVariableNames: 'crc compressedSize uncompressedSize'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZipFileElement subclass: #ZipFileEntryBase
	instanceVariableNames: 'versionToExtractHigh versionToExtractLow flags compressionMethod lastModifiedTime lastModifiedDate crc compressedSize uncompressedSize filename extraField pairedEntry compressionLevel'
	classVariableNames: 'DefaultDate DefaultTime'
	poolDictionaries: 'ZLib1Constants'
	classInstanceVariableNames: ''!
ZipFileEntryBase subclass: #ZipFileCentralDirectoryEntry
	instanceVariableNames: 'versionMadeByHigh versionMadeByLow diskNumberStart internalAttributes externalAttributes offsetOfLocalHeader comment'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ZipFileEntryBase subclass: #ZipFileEntry
	instanceVariableNames: 'rawContentsPosition mustWriteSizes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

Smalltalk at: #ZipFileConstants put: (PoolConstantsDictionary named: #ZipFileConstants)!
ZipFileConstants at: 'ZF_CONTENT_IS_ENCRYPTED_MASK' put: 16r1!
ZipFileConstants at: 'ZF_CONTENT_IS_TEXT_MASK' put: 16r1!
ZipFileConstants at: 'ZF_DEFLATE_COMPRESSION' put: 16r8!
ZipFileConstants at: 'ZF_DEFLATE_STRENGTH_MASK' put: 16r6!
ZipFileConstants at: 'ZF_IMPLODE_8K_DICTIONARY_MASK' put: 16r2!
ZipFileConstants at: 'ZF_IMPLODE_COMPRESSION' put: 16r6!
ZipFileConstants at: 'ZF_IMPLODE_SF_TREES_MASK' put: 16r4!
ZipFileConstants at: 'ZF_INTERNAL_USE_MASK_1' put: 16r2000!
ZipFileConstants at: 'ZF_INTERNAL_USE_MASK_2' put: 16r4000!
ZipFileConstants at: 'ZF_INTERNAL_USE_MASK_3' put: 16r8000!
ZipFileConstants at: 'ZF_INTERNAL_USE_MASK_ALL' put: 16rE000!
ZipFileConstants at: 'ZF_MSDOS_DIRECTORY_MASK' put: 16r10!
ZipFileConstants at: 'ZF_MSDOS_HIDDEN_MASK' put: 16r2!
ZipFileConstants at: 'ZF_MSDOS_NOTARCHIVED_MASK' put: 16r20!
ZipFileConstants at: 'ZF_MSDOS_READONLY_MASK' put: 16r1!
ZipFileConstants at: 'ZF_MSDOS_SYSTEM_MASK' put: 16r4!
ZipFileConstants at: 'ZF_NO_COMPRESSION' put: 16r0!
ZipFileConstants at: 'ZF_NO_CONTENTS_SIZE_MASK' put: 16r8!
ZipFileConstants at: 'ZF_PATCH_DATA_MASK' put: 16r20!
ZipFileConstants at: 'ZF_PKWARE_DCL_COMPRESSION' put: 16rA!
ZipFileConstants at: 'ZF_REDUCE_1_COMPRESSION' put: 16r2!
ZipFileConstants at: 'ZF_REDUCE_2_COMPRESSION' put: 16r3!
ZipFileConstants at: 'ZF_REDUCE_3_COMPRESSION' put: 16r4!
ZipFileConstants at: 'ZF_REDUCE_4_COMPRESSION' put: 16r5!
ZipFileConstants at: 'ZF_SHRINK_COMPRESSION' put: 16r1!
ZipFileConstants at: 'ZF_VERSION_ACORN_RISCOS' put: 16rD!
ZipFileConstants at: 'ZF_VERSION_AMIGA' put: 16r1!
ZipFileConstants at: 'ZF_VERSION_ATARI' put: 16r5!
ZipFileConstants at: 'ZF_VERSION_BEOS' put: 16r10!
ZipFileConstants at: 'ZF_VERSION_CPM' put: 16r9!
ZipFileConstants at: 'ZF_VERSION_FAT_FS' put: 16r0!
ZipFileConstants at: 'ZF_VERSION_HPFS' put: 16r6!
ZipFileConstants at: 'ZF_VERSION_MACINTOSH' put: 16r7!
ZipFileConstants at: 'ZF_VERSION_MVS' put: 16rF!
ZipFileConstants at: 'ZF_VERSION_NTFS' put: 16rB!
ZipFileConstants at: 'ZF_VERSION_SMS_QDOS' put: 16rC!
ZipFileConstants at: 'ZF_VERSION_TANDEM' put: 16r11!
ZipFileConstants at: 'ZF_VERSION_TOPS20' put: 16rA!
ZipFileConstants at: 'ZF_VERSION_UNIX' put: 16r3!
ZipFileConstants at: 'ZF_VERSION_VM_CS' put: 16r4!
ZipFileConstants at: 'ZF_VERSION_VMS' put: 16r2!
ZipFileConstants at: 'ZF_VERSION_Z_SYS' put: 16r8!
ZipFileConstants shrink!

"Classes"!

ZipFile guid: (GUID fromString: '{8BA6C635-B8EF-4184-88FF-CB7D6E9EF479}')!
ZipFile comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

These represent ZIP format data stored either on-disk or held as a ByteArray in memory.  It provides the abillity to read exisiting files (or memory structures, but I''m not going to go on repeating that mantra), creating new ones, or appending to existing ones.  It does not provide a way to make modifications to the contents of an existing file (that would be possible, but it''d be just about as inefficient as copying, without providing much benefit, so we don''t bother).

Zip files are peculiar and rather complex beasts, they naturally support either sequential or random-access reading, but only sequential writing.  This nature means that the underlying oddness of the file format must necessarily show through in the API for manipulating them.  Hence this class also has a somewhat odd API.  For reading an exisiting file, these allow you to treat a Zip file in much the same way as a dictionary (though, I''ve made no great effort to duplicate the Dictionary API) which maps String names to "entries" which describe files.  For writing to a new file (or appending) you use it in a more sequential way, creating a new entry (#openEntry:), set any data that you care about, write the contents of the "file" , and then close the entry (#closeEntry).  You do that for each sub-file, and then #close the entire Zip file, at which time it will write the data which lives at the end of a Zip format file.  It is only after the #close (or a #flush) that the on-disk representation is a valid Zip file.

Zip files consist of a series of entries, one for each contained file, followed by a "central directory" which is basically a table of contents for the file.  So, straight-away we have two ways of scanning a Zip file: we can either start at the begining and process each entry, or start at the end and read the table of contents.   We use the second approach here and treat the Zip file like a random access dictionary.  See ZipFileElement class>>elementsFrom[NonPositionableStream]:do: for examples of how to use the other approach, and just read the entries starting at the begining of the file, ignoring the table of contents (which can be useful for retrieving data from corrupt Zip files, and well as necessary if the data is on a medium which doesn''t support random access).

When an instance is created for reading, it must be attatched to a positionable binary ReadStream, it will find the table of contents (the "central directory") and build up a map of the Zip File''s contents.  The contents can then be accessed by methods like #at: or #do: or #entries.  In each case the elements are ZipFileCentralDirectoryEntries, these can in turn be asked for their contents or contents stream or for other associated data.

If you are writing a file, then the new instance should be connected to a binary WriteStream.  As you add entries, it will write corresponding data to the stream, and then when you #close the ZipFile, it will write out the table of contents which must be at the end of any Zip file.

If you want to append to an exising file then the above operations are combined.  You attatch a new instance to a binary ReadWriteStream, and it will read the table of contents.  As you add entries, it will write them to the stream *overwriting* the existing table of contents.  Then, when you #close, it will write out a new table and the end of the extended file.

There are convenience methods (#read: #write: and #append:) which take a filename, and answer a new instance with appropriate {Read/Wite}Streams set up for you.

See the class-side methods in category ''examples'' for examples of how to read and write ZIP format data.  You can also look at the "Unit tests" if you want, though I''ve never found test code very helpful either as a guide or as a source of examples.'!
!ZipFile categoriesForClass!Unclassified! !
!ZipFile methodsFor!

addCentralDirectoryEntry: aCDEntry
	"private -- add the given entry to our lists and toc"

	self addToToc: aCDEntry.

	centralDirectoryEntries addLast: aCDEntry.

	centralDirectoryEnd
		entriesInDirectoryOnThisDisk: centralDirectoryEntries size;
		entriesInAllDirectories: centralDirectoryEntries size.
!

addToToc: aCentralDirectoryEntry
	"private -- add the given entry to our Table of Contents.
	If there already is an entry of the given name then overwrite the
	original (but log it)"

	| key |

	key := aCentralDirectoryEntry filename ifNil: [''].

	(toc includesKey: key) ifTrue: [Notification signal: 'Duplicate entry name in Zip File' with: key].

	toc at: key put: aCentralDirectoryEntry.!

at: aString
	"if we contain a central directory element with the given name (which can be nil)
	then answer it, otherwise throw an error"

	^ toc at: (aString ifNil: ['']).!

at: aString ifAbsent: a0Block
	"if we contain a central directory element with the given name (which can be nil)
	then answer it, otherwise answer the result of evaluating a0Block"

	^ toc at: (aString ifNil: ['']) ifAbsent: a0Block.!

at: aString ifPresent: a1Block
	"if we contain a central directory element with the given name (which can be nil)
	then answer the result of evaluating a1Block passing the entry as a parameter,
	otherwise answer nil"

	^ toc at: (aString ifNil: ['']) ifPresent: a1Block.!

centralDirectoryEnd
	"private -- answer the receiver's ZipFileCentralDirectoryEnd"

	^ centralDirectoryEnd.
!

centralDirectoryEntries
	"private -- answer the receiver's list of ZipFIleentralDirectoryEntries"

	^ centralDirectoryEntries.
!

close
	"close our underlying stream (which may be a null-op).  More importantly,
	ensure that any additions we have made to the Zip File have been flushed
	out to the stream.
	This is a null-op if we have not made any modifications to the underlying
	file/data (i.e. you don't need to bother to #close instances that are only 
	used for reading Zip files)"

	self flushCentralDirectory.

	stream close.!

closeEntry
	"finish the process of writing the entry opened with #openEnty:"

	| entry |

	self assert: [openCDEntry notNil].

	"have to make sure that the file sizes are written to disk"
	entry := openCDEntry fileEntry.
	entry hasBeenWritten ifFalse: [entry write].

	"remember where we'll be writing the next addition"
	nextWritePosition := entry positionOfNextInStream.

	"and make the position fields in the central directory all OK again"
	self validateCentralDirectoryPositions.

	openCDEntry := nil.!

comment
	"answer the comment stored in this Zip file, may be nil"

	^ centralDirectoryEnd comment.!

comment: aString
	"set the comment stored in this Zip file"

	centralDirectoryEnd comment: aString.!

compressEntries
	"answer whether newly added entries are initialised to compress their contents"

	self entryCompressionMethod ~= ZF_NO_COMPRESSION.!

compressEntries: aBool
	"sets whether newly added entries are initialised to compress their contents.
	This can be changed at any time, but will, of course, only affect subsequenty
	added entries"

	| method |

	"we only support DEFLATE compression"
	method := aBool
			ifTrue: [ZF_DEFLATE_COMPRESSION]
			ifFalse: [ZF_NO_COMPRESSION].

	self entryCompressionMethod: method.!

createCentralDirectoryEntry: aZipFileEntry
	"private -- answer a newly created CentralDirectoryEntry corresponding to
	the given file entry, and ready to be added to ourself"

	^ ZipFileCentralDirectoryEntry for: aZipFileEntry.
!

createEntry: aString
	"private -- answer a newly created ZipFileEntry ready to be added to ourself"

	self assert: [nextWritePosition notNil].

	^ (ZipFileEntry newAt: nextWritePosition on: stream)
		filename: aString;
		compressionMethod: entryCompressionMethod;
		compressionLevel: entryCompressionLevel;
		timestampNow;
		yourself.
!

defaultEntryCompressionLevel
	"answer the Z_*_COMPRESSION (level) that our entries will be initialised to use"

	^ Z_DEFAULT_COMPRESSION.!

defaultEntryCompressionMethod
	"answer the ZF_*_COMPRESSION value that instances will use by default"

	^ ZF_DEFLATE_COMPRESSION.
!

defaultToc
	"private -- answer the <Dictionary> that we will use to map filenames
	onto entries by default.
	ZipFiles normally are case-sensitive and distinguish between,
	say, 'dir/filename' and 'dir/filename/' with the later being the
	name of a directory (called 'filename') whereas the former is
	the name of a file"

	^ LookupTable new.!

displayOn: aStream
	"write a user-centric representation of ourselves to aStream"

	aStream display: (name ifNil: ['<in memory>']).!

do: a1Block
	"evaluate a1Block for each central directory entry we contain"

	centralDirectoryEntries do: a1Block.!

entries
	"answer a list (a shallow copy) of all our central directory entries"

	^ OrderedCollection withAll: centralDirectoryEntries.!

entryCompressionLevel
	"answer the Z_*_COMPRESSION (level) value that sets the initial compression
	method of newly added entries"

	^ entryCompressionLevel.!

entryCompressionLevel: anInteger
	"set the Z_*_COMPRESSION (level) value that sets the initial compression
	method of newly added entries.
	This can be changed at any time, but will, of course, only affect subsequenty
	added entries"

	entryCompressionLevel := anInteger.!

entryCompressionMethod
	"answer the ZF_*_COMPRESSION value that sets the initial compression
	method of newly added entries"

	^ entryCompressionMethod.!

entryCompressionMethod: anInteger
	"set the ZF_*_COMPRESSION value that sets the initial compression
	method of newly added entries.
	This can be changed at any time, but will, of course, only affect subsequenty
	added entries"

	entryCompressionMethod := anInteger.!

fileFormatError: aString
	"private -- trigger a standard ZipFile error with the given message"

	self fileFormatError: aString with: nil.
!

fileFormatError: aString with: anObject
	"private -- trigger a standard ZipFile error with the given message and tag"

	self class fileFormatError: aString with: anObject.
!

filename
	"there are so many contexts that assume that #name means 'filename'
	that we may as well have the alias"

	^ self name.!

flush
	"flush our underlying stream (which may be a null-op).  More importantly,
	ensure that any additions we have made to the Zip File have been flushed
	out to the stream.  After this has been called the on-disk structure is once
	again a valid Zip file.
	Note that calling this, and then adding new entries, will cause extra work,
	since the new entries will overwrite the central directory that we add here"

	self flushCentralDirectory.

	stream flush.!

flushCentralDirectory
	"private -- if any changes have been made, then we need to overwrite
	the existing central directory"

	modified ifFalse: [^ self].

	"we may as well do this automatically as throw an error"
	self hasOpenEntry ifTrue: [self closeEntry].

	"write out the data"
	centralDirectoryEntries do: [:each | each writeData].
	centralDirectoryEnd writeData.

	"and since we have now saved the changes..."
	modified := false.
!

getCentralDirectoryEnd
	"private -- tries to find the 'central directory end record' at the end of the file and answer its contents.
	Throws a format error if the file does not appear to be a ZIP file"

	| tail signature offset |

	"the ZIP file format has to be *the* most mindless, stupid, broken, piece of design
	that I have ever seen"

	"get the last 64K or so of the file and search backwards through it to see if we can
	find the record we are looking for -- faugh!!"
	tail := self tailEndOfInput reverse.
	signature := ZipFileCentralDirectoryEnd signature reverse.
	offset := ZipFileCentralDirectoryEnd fixedRecordSizeLessSignature + 1.
	[
		offset := tail indexOfSubCollection: signature startingAt: offset.
		offset = 0 ifTrue: [self fileFormatError: 'Cannot find "Central Directory"'].
		(self getCentralDirectoryEndAt: stream size + 1 - offset) ifNotNil: [:it | ^ it].
		offset := offset + 1. "we could probably increment by more than 1, but it's not worth the effort"
	] repeat.!

getCentralDirectoryEndAt: anInteger
	"private -- tries to find the 'central directory end record' at the given offset from the end of
	the file; answers the record if it finds it, nil otherwise"

	| record |

	stream position: anInteger.
	record := [ZipFileCentralDirectoryEnd from: stream] on: Error do: [:err | ^ nil].

	"sanity checking -- we need this since we don't actually *know* where the CD record is
	supposed to be, even in a correctly formed ZIP file (sigh...)"
	stream atEnd ifFalse: [^ nil].
	(self isCentralDirectoryEndPlausible: record) ifFalse: [^ nil].

	^ record.!

getCentralDirectoryEntries
	"private -- read and return the the central directory entries from the underlyng stream"

	| entryMark endMark signature entries |

	entryMark := ZipFileCentralDirectoryEntry signature.
	endMark := ZipFileCentralDirectoryEnd signature.

	"find the start of the directory"
	stream position: centralDirectoryEnd directoryOffset.

	"read in its contents"
	entries := OrderedCollection new.
	[signature := stream next: entryMark size.
	signature = entryMark] whileTrue:
		[entries addLast: (ZipFileCentralDirectoryEntry from: stream)].

	"sanity check"
	signature = endMark ifFalse: [self fileFormatError: 'Unknown signature where end-of-directory expected' with: signature].

	^ entries.!

hasOpenEntry
	"answer true iff we have had an entry opened but not yet closed"

	^ openCDEntry notNil.!

includes: aString
	"answer whether we contain a central directory element with the given name (which can be nil)"

	^ toc includesKey: (aString ifNil: ['']).!

initialize
	"private -- establish a coherent initial state"

	stream := nil.
	toc := self defaultToc.
	modified := false.
	nextWritePosition := nil.
	entryCompressionMethod := self defaultEntryCompressionMethod.
	entryCompressionLevel := self defaultEntryCompressionLevel.
!

invalidateCentralDirectoryPositions
	"private -- this is as much to prevent accidents as anything, when an
	entry is 'opened', and until it is subsequently 'closed', the toc elements
	don't know where they should be positioned in the file, so we ensure
	that they *know* that they don't know"

	centralDirectoryEntries do: [:each | each positionInStream: nil].

	centralDirectoryEnd
		positionInStream: nil;
		directoryOffset: nil;
		directorySize: nil.
!

isCentralDirectoryEndPlausible: aRecord
	"private -- do some basic sanity checking on a putative Central Directory RecordEnd"

	"do the internal values make sense ?"
	aRecord isPlausible ifFalse: [^ false].

	"is there is a correct CD record at the indicated offset ?"
	(aRecord entriesInDirectoryOnThisDisk = 0 or: [self signatureAt: aRecord directoryOffset isForA: ZipFileCentralDirectoryEntry])
		ifFalse: [^ false].

	^ true.
!

isCentralDirectorySorted 
	"private -- answer whether our central directory list is in the same order as the files the zip file"

	| last |

	last := -1.
	self do:
		[:each || this |
		this := each offsetOfLocalHeader.
		this < last ifTrue: [^ false].
		last := this].

	^ true.!

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

	^ openCDEntry.!

openEntry: aString
	"add a new entry with the given name (which can be nil).
	Will throw an error if there is already an entry of the same name.
	The new entry will be pre-set to use a  #compressionLevel taken from our
	#entryCompressionMethod and #compressionType from our #entryCompressionLevel
	(either can be changed).  It will have its #lastModified{Date/Time} set to the system's
	current date and time.
	NB: this does *not* write the entry to the underlying stream, and so
	untill #closeEntry has been called, we are in an inconsistent state"

	| entry |

	self assert: [openCDEntry isNil].

	(self includes: aString) ifTrue: [self fileFormatError: 'Cannot add entry with duplicate name' with: aString].

	"create the new entry and the corresponding central directory entry"
	entry := self createEntry: aString.
	openCDEntry := self createCentralDirectoryEntry: entry.

	"add it to the table of contents"
	self addCentralDirectoryEntry: openCDEntry.

	"we don't know where the CD will be written, so..."
	self invalidateCentralDirectoryPositions.

	"remember that we've been modified"
	modified := true.

	^ openCDEntry.!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' on: ';
		display: self.!

readCentralDirectory
	"private -- real the central directory from the input stream"

	"start by finding the central directory at the end of the data"
	centralDirectoryEnd := self getCentralDirectoryEnd.

	"then read in the elements thereof"
	centralDirectoryEntries := self getCentralDirectoryEntries.

	"sanity check"
	(centralDirectoryEntries size = centralDirectoryEnd entriesInDirectoryOnThisDisk) ifFalse:
		[self fileFormatError: 'Inconsistant "central directory" data'].

	"it's handy, though not necessary, to keep the entries sorted in the same order as the file entries"
	self sortCentralDirectoryEntries.

	"and build the name->entry map"
	centralDirectoryEntries do: [:each | self addToToc: each].
	!

readStream: aBinaryStream
	"set the stream that we are reading from"

	stream := aBinaryStream.
	self readCentralDirectory.!

readWriteStream: aBinaryReadWriteStream
	"set the stream that we are appending to"

	stream := aBinaryReadWriteStream.

	"have to start by reading the existing entries, the next item we
	add will start overwriting the central directory, and then when
	we are #closed or #flushed, we will re-write the entire central
	directory, including any additions"
	self readCentralDirectory.
	nextWritePosition := centralDirectoryEnd directoryOffset.

	"note that we don't set the modifiedf flag here -- the existing
	CentralDirectoryEnd record is OK until we actually add anything"
!

select: a1Block
	"answer a list of the central directory entry we contain for which a1Block evaluates
	to true"

	^ centralDirectoryEntries select: a1Block.!

select: a1Block thenDo: another1Block
	"evaluate a1Block for each central directory entry we contain for which a1Block evaluates
	to true"

	centralDirectoryEntries do: [:each | (a1Block value: each) ifTrue: [another1Block value: each]].!

signatureAt: anInteger isForA: ZipFileElementClass
	"private -- does our stream contain a valid signature for the given element class at the given position ?"

	| shouldBe is |

	shouldBe := ZipFileElementClass signature.
	is := stream
		position: anInteger;
		next: shouldBe size.

	^ shouldBe = is.


!

size
	"answer how many file or directory entries this Zip file contains"

	^ centralDirectoryEntries size.!

sortCentralDirectoryEntries
	"private -- sort our central directory list in order that their files appear in the zip file"

	| sorted |

	"it usually is sorted, so it's worth checking first"
	self isCentralDirectorySorted ifTrue: [^ self].

	sorted := centralDirectoryEntries  asSortedCollection: (SortAscending by: #offsetOfLocalHeader).
	centralDirectoryEntries := sorted asOrderedCollection.
!

stream
	"answer the binary Stream we are reading/writing"

	^ stream.!

tailEndOfInput
	"private -- answer the last 64K or so of our file, this is where the 'central directory
	end record' may be lurking"

	| tailStart |

	"this is the earlierst *possible* position, which most probably is not the
	actual position"
	tailStart := stream size - ZipFileCentralDirectoryEnd maxRecordSize.

	^ stream
		position: (tailStart max: 0);
		upToEnd.!

validateCentralDirectoryPositions
	"private -- undo the tempory damge caused by #invalidateCentralDirectoryPositions"

	| pos |

	pos := nextWritePosition.
	centralDirectoryEntries do:
		[:each |
		each positionInStream: pos.
		pos := each positionOfNextInStream].

	centralDirectoryEnd
		positionInStream: pos;
		entriesInDirectoryOnThisDisk: centralDirectoryEntries size;
		entriesInAllDirectories: centralDirectoryEntries size;
		directoryOffset: nextWritePosition;
		directorySize: pos - nextWritePosition.!

withNewEntry: aString do: a1Block
	"open a new entry and then evaluate a1Block ensuring that we close it again
	afterwards.
	Answers the result of evauating the block"

	self openEntry: aString.
	^ [a1Block value: openCDEntry] ensure: [self closeEntry].!

writeStream: aBinaryWriteStream
	"set the stream that we are writing to.
	We will write to the stream starting at its current position"

	stream := aBinaryWriteStream.

	nextWritePosition := aBinaryWriteStream position.
	centralDirectoryEntries := OrderedCollection new.
	centralDirectoryEnd := ZipFileCentralDirectoryEnd newAt: nextWritePosition on: stream.

	"an empty file is *not* a valid Zip File, so even if no entries are added, we
	will still need to write the central directory when we are closed, so we
	treat ourself as modified from the off"
	modified := true.
! !
!ZipFile categoriesFor: #addCentralDirectoryEntry:!helpers!private! !
!ZipFile categoriesFor: #addToToc:!helpers!private! !
!ZipFile categoriesFor: #at:!accessing!public! !
!ZipFile categoriesFor: #at:ifAbsent:!accessing!public! !
!ZipFile categoriesFor: #at:ifPresent:!accessing!public! !
!ZipFile categoriesFor: #centralDirectoryEnd!accessing!private! !
!ZipFile categoriesFor: #centralDirectoryEntries!accessing!private! !
!ZipFile categoriesFor: #close!operations!public!writing! !
!ZipFile categoriesFor: #closeEntry!public!writing! !
!ZipFile categoriesFor: #comment!accessing!public! !
!ZipFile categoriesFor: #comment:!accessing!public! !
!ZipFile categoriesFor: #compressEntries!accessing!public!testing! !
!ZipFile categoriesFor: #compressEntries:!accessing!public! !
!ZipFile categoriesFor: #createCentralDirectoryEntry:!helpers!private! !
!ZipFile categoriesFor: #createEntry:!helpers!private! !
!ZipFile categoriesFor: #defaultEntryCompressionLevel!constants!public! !
!ZipFile categoriesFor: #defaultEntryCompressionMethod!constants!public! !
!ZipFile categoriesFor: #defaultToc!constants!private! !
!ZipFile categoriesFor: #displayOn:!displaying!public! !
!ZipFile categoriesFor: #do:!enumerating!public! !
!ZipFile categoriesFor: #entries!accessing!public! !
!ZipFile categoriesFor: #entryCompressionLevel!accessing!public! !
!ZipFile categoriesFor: #entryCompressionLevel:!accessing!public! !
!ZipFile categoriesFor: #entryCompressionMethod!accessing!public! !
!ZipFile categoriesFor: #entryCompressionMethod:!accessing!public! !
!ZipFile categoriesFor: #fileFormatError:!exceptions!private! !
!ZipFile categoriesFor: #fileFormatError:with:!exceptions!private! !
!ZipFile categoriesFor: #filename!accessing!public! !
!ZipFile categoriesFor: #flush!operations!public!writing! !
!ZipFile categoriesFor: #flushCentralDirectory!private!writing! !
!ZipFile categoriesFor: #getCentralDirectoryEnd!private!reading! !
!ZipFile categoriesFor: #getCentralDirectoryEndAt:!private!reading! !
!ZipFile categoriesFor: #getCentralDirectoryEntries!private!reading! !
!ZipFile categoriesFor: #hasOpenEntry!public!testing!writing! !
!ZipFile categoriesFor: #includes:!public!testing! !
!ZipFile categoriesFor: #initialize!initializing!private! !
!ZipFile categoriesFor: #invalidateCentralDirectoryPositions!helpers!private! !
!ZipFile categoriesFor: #isCentralDirectoryEndPlausible:!helpers!private!testing! !
!ZipFile categoriesFor: #isCentralDirectorySorted!helpers!private! !
!ZipFile categoriesFor: #isEmpty!public!testing! !
!ZipFile categoriesFor: #name!accessing!public! !
!ZipFile categoriesFor: #name:!initializing!private! !
!ZipFile categoriesFor: #openEntry!public!writing! !
!ZipFile categoriesFor: #openEntry:!public!writing! !
!ZipFile categoriesFor: #printOn:!printing!public! !
!ZipFile categoriesFor: #readCentralDirectory!private!reading! !
!ZipFile categoriesFor: #readStream:!initializing!public! !
!ZipFile categoriesFor: #readWriteStream:!initializing!public! !
!ZipFile categoriesFor: #select:!enumerating!public! !
!ZipFile categoriesFor: #select:thenDo:!enumerating!public! !
!ZipFile categoriesFor: #signatureAt:isForA:!helpers!private!testing! !
!ZipFile categoriesFor: #size!accessing!public! !
!ZipFile categoriesFor: #sortCentralDirectoryEntries!helpers!private! !
!ZipFile categoriesFor: #stream!accessing!public! !
!ZipFile categoriesFor: #tailEndOfInput!helpers!private!reading! !
!ZipFile categoriesFor: #validateCentralDirectoryPositions!helpers!private! !
!ZipFile categoriesFor: #withNewEntry:do:!public!writing! !
!ZipFile categoriesFor: #writeStream:!initializing!public! !

!ZipFile class methodsFor!

append: aFilename

	Notification deprecated.
	^ self appendingFile: aFilename.!

appendingFile: aFilename
	"answer a new instance that will first read from the named file and
	then allow new entries to be added to the end of it.
	Note that from the time when the first entry has been appended,
	to the time when the ZipFile is #closed or #flushed, the Zip file's structure
	on disk will be invalid"

	^ (self name: aFilename)
		readWriteStream: (FileStream write: aFilename mode: #append check: true text: false);
		yourself.!

bugs

	^ '
Will allow entries >64K in size (overlarge comments, etc).
'

!

exampleAppendingToAFile: aFilename
	"example of opening an existing Zip file and appending an new element.

		self exampleAppendingToAFile: (FileOpenDialog showModal).
	"

	| zipfile |

	(MessageBox confirm: 'OK to overwrite ' , aFilename) ifFalse: 	[^ self].

	zipfile := ZipFile appendingFile: aFilename.

	"let's just list the contents first"
	Transcript nextPutAll: 'Before:'; cr.
	zipfile do: [:each | Transcript display: each; cr].

	"we will just add one text entry, to add a little variety we willl also add it
	as uncompressed data"

	"a more complicated example for the binary data, we will set some of the
	associated data in the entry.  Other options we could set in the same way
	include whether the contents should be compresses, and what compression
	level to use.
	If an entry of the given name is already in the Zip file then this will throw an
	error"
	zipfile
		withNewEntry: 'added.txt'
		do: [:it | it
				beText;
				useCompression: false;
				writeText: self class comment].

	"now, since we have changed the file, we *must* #close, or the on-disk file will
	incomplete (and probably unreadable to most ZIP software)"
	zipfile close.

	"and list it again contents first"
	Transcript nextPutAll: 'After:'; cr.
	zipfile do: [:each | Transcript display: each; cr].
!

exampleCreatingADirectory: aFilename
	"Example of creating directory entries inside a zipfile;  the main point of
	this example is actually the opening comment....

		self exampleCreatingADirectory: (FileSaveDialog showModal).
	"

	| zipfile |

	"in general ZIP files do not contain explicit representations of directories, it is
	up to the reading application to infer the directoriy structure from the
	'/'-separated names of files.  (NB: it is *not* OK to use DOS-style '\' separators
	in filenames in a ZIP file).
	However you *can* add directory entries if you want to, just be aware that
	some readers may either ignore them or be confused by them"

	(File exists: aFilename) ifTrue:
		[(MessageBox confirm: 'OK to overwrite ' , aFilename) ifFalse:
			[^ self]].

	zipfile := ZipFile writingFile: aFilename.

	"create a directory to which we will later add something. The crucial indicator that it
	*is* a directory is the trailing '/' on the name"
	zipfile withNewEntry: 'directory/' do: [:it | ].

	"create a directory which we will leave empty. The crucial indicator that it
	*is* a directory is the trailing '/' on the name"
	zipfile withNewEntry: 'empty-directory/' do: [:it | ].

	"add soemthing in the first directory -- there is no connection between this item
	and the earlier one except for their names"
	zipfile withNewEntry: 'directory/text.txt' do: [:it | it writeText: 'some text'].

	"lastly add a text item that we haven't created a directory for"
	zipfile withNewEntry: 'haha/more.txt' do: [:it | it writeText: 'some more text'].

	zipfile close.!

exampleCreatingAZipStructureInMemory
	"this example creates a simple 'Zip file' in memory

		self exampleCreatingAZipStructureInMemory.
	"


	| zipfile bytes |

	"creates a ZipZfile that is empty and ready for writing.  If we'd wanted to
	append to an exisitng ByteArray containting a Zip structure, then we'd
	have used #inMemory:"
	zipfile := ZipFile inMemory.

	"just add some stuff"
	zipfile
		withNewEntry: 'Zeros.dat'
		do: [:it | it withBinaryWriterDo:
			[:writer |
			writer next: 1000000 put: 0]].
	zipfile close.

	"to get the ByteArray holding the Zip structure, we ask for the ZipFile's
	underlying stream, and ask that for its contents"
	bytes := zipfile stream contents.

	Transcript display: bytes size; cr.

	"in case you want to inspect it"
	^ zipfile.!

exampleReadingAFile: aFilename
	"example of opening an existing Zip file and iterating over
	its elements, writing brief details of each to the Transcript.

		self exampleReadingAFile: (FileOpenDialog showModal).
	"

	| zipfile |

	zipfile := ZipFile readingFile: aFilename.

	Transcript
		nextPutAll: 'Name: ';
		display: zipfile filename;
		cr;
		nextPutAll: 'Entries: ';
		display: zipfile size;
		cr;
		nextPutAll: 'Comment: ';
		display: (zipfile comment ifNil: ['<none>']);
		cr.

	"in this loop we call #knowsContentsSizes explicitly.  Normally we wouldn't have to bother
	with that since we'd be more interested in the contents of the files.  If needs be, we
	could send #getContentsSizes to the entry to ensure that it scanned its contents to
	find the sizes before we asked"
	zipfile do:
		[:each | 
		Transcript tab; display: each filename.
		each knowsContentsSizes ifTrue:
			[Transcript
				nextPutAll: ' size: ';
				display: each uncompressedSize;
				nextPutAll: ' compressed: ';
				display: each compressedSize].
		Transcript cr].

	"since we are only reading the file there's no need to #close it"

	"in case you want to inspect it"
	^ zipfile.
!

exampleReadingAnEntryWithAStream: aFilename
	"example of opening an existing Zip file and choosing one of its elements,
	getting a ReadStream on that entries contents, and then looping copying each
	line of text to the Transcript.

		self exampleReadingAnEntryWithAStream: (FileOpenDialog showModal).
	"

	| zipfile allNames theName entry |

	zipfile := ZipFile readingFile: aFilename.

	"slightly artificial -- we chose a name rather than just choosing an entry directly, but
	that will give us a chance to use #at:, below"
	allNames := zipfile entries collect: [:each | each filename].
	theName := ChoicePrompter choices: allNames caption: 'Please choose a *TEXT* entry'.
	theName isNil ifTrue: [^ self].

	"find the entry with the given name"
	entry := zipfile at: theName.

	"now, we ask explicitly for a text stream on its contents"
	entry withTextReaderDo:
		[:reader |
		"and consume that stream line-by-line (just to show that we can)"
		[reader atEnd] whileFalse:
			[| line |
			line := reader nextLine.
			Transcript nextPutAll: line; cr]].

	"since we are only reading the file there's no need to #close it"!

exampleReadingEntries: aFilename
	"example of opening an existing Zip file and iterating over
	its elements, writing the first few bytes/chars of each to the Transcript.

		self exampleReadingEntries: (FileOpenDialog showModal).
	"

	| zipfile |

	zipfile := ZipFile readingFile: aFilename.

	"in this loop we call #contents on each item.  This will answer either a ByteArray or a String
	depending on whether the creator of the file marked the element as containing binary data
	or text.  In most real applications we'd force the choice by calling #textContents or #binaryContents
	instead"
	zipfile do:
		[:each || contents |
		contents := each contents.
		Transcript display: each filename; nextPut: $:; tab.
		contents notNil ifTrue: [Transcript print: (contents first: 10)].
		Transcript cr].

	"since we are only reading the file there's no need to #close it"!

exampleWritingAFile: aFilename
	"example of creating a new Zip file and adding two elements to it.

		self exampleWritingAFile: (FileSaveDialog showModal).
	"

	| zipfile |

	(File exists: aFilename) ifTrue:
		[(MessageBox confirm: 'OK to overwrite ' , aFilename) ifFalse:
			[^ self]].

	zipfile := ZipFile writingFile: aFilename.

	"we will add two simple entries:
		Test.dat			-- contains binary data
		subdir/Test.txt		-- contains a String message
	Note how we 'create' sub-directories just by naming a file
	inside the directory.  That is normally the way that ZIP files are
	used (and some readers are upset if you don't follow the pattern).
	You can add explicit directories if you want to, see #exampleCreatingADirectory:.
	Also note that by universal convention we use Unix-style path separators"

	"create the text entry.  It will have default settings for compression, etc.  Also
	it will be marked as having the current date and time as its 'last-modified' time"
	zipfile
		withNewEntry: 'Test.dat'
		do: [:it | it writeBinary: (ByteArray withAll: (0 to: 255))].

	"a more complicated example for the binary data, we will set some of the
	associated data in the entry.  Other options we could set in the same way
	include whether the contents should be compresses, and what compression
	level to use"
	zipfile
		withNewEntry: 'subdir/Test.txt'
		do: [:it |
			"set the last modified times, must do this before writing the contents"
			it lastModifiedDate: (Date fromDays: (Date today asDays - 1)).	"yesterday"
			it lastModifiedTime: (Time fromSeconds: (12 * 60 * 60)).			"noon"

			"record a file comment"
			it comment: 'Hi!!'.

			"and we'll mark it explicitly as being text (we don't have to, it's just a
			helper for whatever application reads the file later"
			it beText.

			"and finally set its contents to some deeply interesting text..."
			it writeText: self class comment].

	"now, since we have changed the file, we *must* #close, or the on-disk file will
	incomplete (and probably unreadable to most ZIP software)"
	zipfile close.!

exampleWritingAnEntryWithAStream: aFilename
	"example of creating a new Zip file and adding two elements to it.

		self exampleWritingAnEntryWithAStream: (FileSaveDialog showModal).
	"

	| zipfile |

	(File exists: aFilename) ifTrue:
		[(MessageBox confirm: 'OK to overwrite ' , aFilename) ifFalse:
			[^ self]].

	zipfile := ZipFile writingFile: aFilename.

	"we will add a text entry that lists all the classes in the system"
	zipfile
		withNewEntry: 'All class names.txt'
		do: [:it || written |
			"and we'll mark it explicitly as being text (we don't have to, it's just a
			helper for whatever application reads the file later"
			it beText.

			"and we'll use maximum compression (there are symbolic constants
			defined for the compression, but most people prefer to use the numbers
			from 0 to 9)"
			it compressionLevel: 9.

			"open a write stream and write each class name to it"
			written := 0.
			it withWriterDo:
				[:writer | Object withAllSubclassesDo:
					[:each |
					writer display: each; cr.
					written := written + 1]].
		
			"dump some stats to the Transcript"
			Transcript
				display: 'Classes written: ';
				display: written;
				cr;
				display: 'Uncompressed size: ';
				display: it uncompressedSize;
				cr;
				display: 'Compressed size: ';
				display: it compressedSize;
				cr].


	"now, since we have changed the file, we *must* #close, or the on-disk file will
	incomplete (and probably unreadable to most ZIP software)"
	zipfile close.!

fileFormatError: aString with: anObject
	"trigger a standard ZipFile error with the given message and tag"

	ZipFileError signal: aString with: anObject.!

fromBytes: aByteArray
	"answer a new instance that will interpret the given bytes as a
	readonly Zip file structure held in memory"

	^ self readStream: (aByteArray readStream).!

inMemory
	"answer a new instance that will create a Zip file structure in memory"

	^ self writeStream: (ReadWriteStream with: ByteArray new).!

inMemory: aByteArray
	"answer a new instance that will interpret the given bytes as a
	Zip file structure held in memory and which will append to it as
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

read: aFilename

	Notification deprecated.
	^ self readingFile: aFilename.!

readingFile: aFilename
	"answer a new instance that will read the named file"

	^ (self name: aFilename)
		readStream: (FileStream read: aFilename text: false);
		yourself.
!

readStream: aBinaryReadStream
	"answer a new instance that will read from the given binary ReadStream"

	^ (self new)
		readStream: aBinaryReadStream;
		yourself.!

readWriteStream: aBinaryReadWriteStream
	"answer a new instance that will first read the entries from the given binary stream,
	and then be able to append further entries"

	^ (self new)
		readWriteStream: aBinaryReadWriteStream;
		yourself.!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"

	(Smalltalk at: #ZipFileConstants ifAbsentPut: [PoolConstantsDictionary new])

		"compression types (we only handle types 0 and 8)"
		at: 'ZF_NO_COMPRESSION'					put: 0;
		at: 'ZF_SHRINK_COMPRESSION'					put: 1;
		at: 'ZF_REDUCE_1_COMPRESSION'				put: 2;
		at: 'ZF_REDUCE_2_COMPRESSION'				put: 3;
		at: 'ZF_REDUCE_3_COMPRESSION'				put: 4;
		at: 'ZF_REDUCE_4_COMPRESSION'				put: 5;
		at: 'ZF_IMPLODE_COMPRESSION'				put: 6;
	"	at: 'ZF_TOKENISED_COMPRESSION'				put: 7;				-- reserved "
		at: 'ZF_DEFLATE_COMPRESSION'				put: 8;
	"	at: 'ZF_DEFLATE_PLUS_COMPRESSION'		put: 9;				-- reserved "
		at: 'ZF_PKWARE_DCL_COMPRESSION'				put: 10;

		"flag bits (we don't use all of these)"
		at: 'ZF_CONTENT_IS_ENCRYPTED_MASK'		put: 1;
		at: 'ZF_IMPLODE_8K_DICTIONARY_MASK'		put: 1<<1;
		at: 'ZF_IMPLODE_SF_TREES_MASK'				put: 1<<2;
		at: 'ZF_DEFLATE_STRENGTH_MASK'				put: 6;				"uses same bits as previous two"
		at: 'ZF_NO_CONTENTS_SIZE_MASK'				put: 1<<3;
		at: 'ZF_PATCH_DATA_MASK'					put: 1<<5;
		at: 'ZF_INTERNAL_USE_MASK_1'				put: 1<<13;		"reserved for use by application"
		at: 'ZF_INTERNAL_USE_MASK_2'				put: 1<<14;
		at: 'ZF_INTERNAL_USE_MASK_3'				put: 1<<15;
		at: 'ZF_INTERNAL_USE_MASK_ALL'				put: 16rE000;	"mask of previous three"

		"high byte of version {made by / to extract} values"
		at: 'ZF_VERSION_FAT_FS'						put: 0;			"includes NTFS, VFAT, ..."
		at: 'ZF_VERSION_AMIGA'						put: 1;	
		at: 'ZF_VERSION_VMS'						put: 2;	
		at: 'ZF_VERSION_UNIX'						put: 3;	
		at: 'ZF_VERSION_VM_CS'						put: 4;	
		at: 'ZF_VERSION_ATARI'						put: 5;	
		at: 'ZF_VERSION_HPFS'						put: 6;	
		at: 'ZF_VERSION_MACINTOSH'					put: 7;	
		at: 'ZF_VERSION_Z_SYS'						put: 8;			"wthf ?"
		at: 'ZF_VERSION_CPM'						put: 9;	
		at: 'ZF_VERSION_TOPS20'					put: 10;
		at: 'ZF_VERSION_NTFS'						put: 11;		"only used by Info-ZIP"
		at: 'ZF_VERSION_SMS_QDOS'					put: 12;	
		at: 'ZF_VERSION_ACORN_RISCOS'				put: 13;
	"	at: 'ZF_VERSION_VFAT'						put: 14;		-- reserved"
		at: 'ZF_VERSION_MVS'						put: 15;
		at: 'ZF_VERSION_BEOS'						put: 16;
		at: 'ZF_VERSION_TANDEM'					put: 17;

		"internal file attribute bits"
		at: 'ZF_CONTENT_IS_TEXT_MASK'				put: 1;

		"external file attribute values"
		at: 'ZF_MSDOS_READONLY_MASK'				put: 16r01;
		at: 'ZF_MSDOS_HIDDEN_MASK'					put: 16r02;
		at: 'ZF_MSDOS_SYSTEM_MASK'					put: 16r04;
	"	at: 'ZF_MSDOS_????_MASK'					put: 16r08;		-- 'lab' ?"
		at: 'ZF_MSDOS_DIRECTORY_MASK'				put: 16r10;
		at: 'ZF_MSDOS_NOTARCHIVED_MASK'				put: 16r20;
	"	at: 'ZF_MSDOS_????_MASK'					put: 16r40;		-- 'lnk' ?"
	"	at: 'ZF_MSDOS_????_MASK'					put: 16r80;		-- 'exe' ?  'normal' ?"

		shrink.
!

todo

	^ '
Decode "extra fields" (at least minimal framework).
Remove hack in ZipFileElement>>writeDataTo:
Remove stream hacks in ZipFileElement>>with{Reader/Writer}Do:
Add API similar to category "enumeration" from class-side of File ?
'!

write: aFilename

	Notification deprecated.
	^ self writingFile: aFilename.!

writeStream: aBinaryWriteStream
	"answer a new instance that will write to the given binary WriteStream"

	^ (self new)
		writeStream: aBinaryWriteStream;
		yourself.!

writingFile: aFilename
	"answer a new instance that will write the named file.
	NB: if the file already exists then it will be truncated and overwritten."

	^ (self name: aFilename)
		writeStream: (FileStream write: aFilename text: false);
		yourself.! !
!ZipFile class categoriesFor: #append:!files!instance creation!public! !
!ZipFile class categoriesFor: #appendingFile:!files!instance creation!public! !
!ZipFile class categoriesFor: #bugs!documentation!public! !
!ZipFile class categoriesFor: #exampleAppendingToAFile:!examples!public! !
!ZipFile class categoriesFor: #exampleCreatingADirectory:!examples!public! !
!ZipFile class categoriesFor: #exampleCreatingAZipStructureInMemory!examples!public! !
!ZipFile class categoriesFor: #exampleReadingAFile:!examples!public! !
!ZipFile class categoriesFor: #exampleReadingAnEntryWithAStream:!examples!public! !
!ZipFile class categoriesFor: #exampleReadingEntries:!examples!public! !
!ZipFile class categoriesFor: #exampleWritingAFile:!examples!public! !
!ZipFile class categoriesFor: #exampleWritingAnEntryWithAStream:!examples!public! !
!ZipFile class categoriesFor: #fileFormatError:with:!exceptions!public! !
!ZipFile class categoriesFor: #fromBytes:!instance creation!memory resident!public! !
!ZipFile class categoriesFor: #inMemory!instance creation!memory resident!public! !
!ZipFile class categoriesFor: #inMemory:!instance creation!memory resident!public! !
!ZipFile class categoriesFor: #name:!instance creation!public! !
!ZipFile class categoriesFor: #new!instance creation!public! !
!ZipFile class categoriesFor: #read:!files!instance creation!public! !
!ZipFile class categoriesFor: #readingFile:!files!instance creation!public! !
!ZipFile class categoriesFor: #readStream:!instance creation!public! !
!ZipFile class categoriesFor: #readWriteStream:!instance creation!public! !
!ZipFile class categoriesFor: #rebuildPoolConstants!development!private! !
!ZipFile class categoriesFor: #todo!documentation!public! !
!ZipFile class categoriesFor: #write:!files!instance creation!public! !
!ZipFile class categoriesFor: #writeStream:!instance creation!public! !
!ZipFile class categoriesFor: #writingFile:!files!instance creation!public! !

ZipFileElement guid: (GUID fromString: '{5443C3B2-FFBE-4C29-B855-E06E39D0678D}')!
ZipFileElement comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A Zip-format file is made up of a sequence of entries.

First there are 0 or more entries for the files.  Each of these starts with a block of data describing the file; this software represents such a block by a ZipFileEntry.  The block is then followed by 0 or more bytes of the data for the file''s contents.  FInally, and optionally, there may be a secondary block of data (represented here by a ZipFileDataDescriptor) that provides the size, compressed size, and crc of the file; this is uncluded *only* if the original block did not contain this data.  (This can occur if, for instance, a Zip file is created on a medium such as tape, that does not support efficient seeking).

After the file entres, there is a "table of contents" called the "central directory" which is a sequence of blocks of data describing (and duplicating with some additions) the blocks of data in the first part of the file.  Each of these is represented here by a ZipFileCentralDirectoryEntry.  Each of these entries contains a pointer to the real entry. The idea is that (if the file supports random access) then the table of contents provides an index to allow random access to the files in the archive.

After all the central directory entries, there is a final entry that summarises the contents of the file.  This represented here by a ZipFileCentralDirectoryEnd.  An "empty" Zip file consists of just the end record, and hence is never less than 22 bytes long.'!
!ZipFileElement categoriesForClass!Unclassified! !
!ZipFileElement methodsFor!

fileFormatError: aString
	"private -- trigger a standard ZipFile error with the given message"

	self class fileFormatError: aString with: self.
!

fixedRecordSize
	"answer the size of the fixed part of the record in a zip file"

	^ self class fixedRecordSize.
!

initialize
	"private -- establish a coherent initial state"
!

isCentralDirectoryElement
	"answer true if we one of the elements of the 'central directory' (table
	of contents of a zipfile)"

	^ false.!

isCentralDirectoryEnd
	"answer true if we are supposed to be the last element in a Zip file"

	^ false.!

populateFrom: aBinaryReadStream
	"private -- populate our instance data by reading from the given stream"

	self subclassResponsibility.!

positionInStream
	"answer the position in the stream where this record started.
	This is the position of our signature"

	^ positionInStream.!

positionInStream: anInteger
	"private -- set the position in the stream where this record started.
	This is the position of our signature"

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

readData
	"private -- populate our instance data by reading the next data
	from our stream"

	self populateFrom: stream.!

recordSize
	"answer the size of the record in a zip file"

	^ self fixedRecordSize + self variableRecordSize.
!

stream
	"answer the binary Stream we are reading/writing"

	^ stream.!

stream: aBinaryStream
	"private -- set the stream from which we were read or to which we will write"

	stream := aBinaryStream.!

variableRecordSize
	"answer the size of the variable part of the record in a zip file"

	^ self subclassResponsibility.
!

writeData
	"private -- write our data to our stream.
	NB: this is not symetrical with #readData in that it does not assume that the
	stream is already correctly positioned"

	self
		positionStreamAt: positionInStream;
		writeDataTo: stream.!

writeDataTo: aBinaryWriteStream
	"private -- write our data to the given stream.
	NB: this is not symetrical with #populateFrom: in that it starts
	by writing the appropriate signature"

	self subclassResponsibility.! !
!ZipFileElement categoriesFor: #fileFormatError:!exceptions!private! !
!ZipFileElement categoriesFor: #fixedRecordSize!accessing!public! !
!ZipFileElement categoriesFor: #initialize!initializing!private! !
!ZipFileElement categoriesFor: #isCentralDirectoryElement!public!testing! !
!ZipFileElement categoriesFor: #isCentralDirectoryEnd!public!testing! !
!ZipFileElement categoriesFor: #populateFrom:!private!reading! !
!ZipFileElement categoriesFor: #positionInStream!accessing!public! !
!ZipFileElement categoriesFor: #positionInStream:!initializing!private! !
!ZipFileElement categoriesFor: #positionOfNextInStream!accessing!public! !
!ZipFileElement categoriesFor: #positionStreamAt:!helpers!private! !
!ZipFileElement categoriesFor: #readData!private!reading! !
!ZipFileElement categoriesFor: #recordSize!accessing!public! !
!ZipFileElement categoriesFor: #stream!accessing!public! !
!ZipFileElement categoriesFor: #stream:!initializing!private! !
!ZipFileElement categoriesFor: #variableRecordSize!accessing!public! !
!ZipFileElement categoriesFor: #writeData!private!writing! !
!ZipFileElement categoriesFor: #writeDataTo:!private!writing! !

!ZipFileElement class methodsFor!

decodeSignatureFrom: aBinaryReadStream
	"answer the subclass identified by the signature read from aBinaryReadStream"

	| signature |

	signature := aBinaryReadStream next: 4.

	signature = ZipFileEntry signature ifTrue: [^ ZipFileEntry].
	signature = ZipFileCentralDirectoryEntry signature ifTrue: [^ ZipFileCentralDirectoryEntry].
	signature = ZipFileCentralDirectoryEnd signature ifTrue: [^ ZipFileCentralDirectoryEnd].
	signature = ZipFileDataDescriptor signature ifTrue: [^ ZipFileDataDescriptor].

	self fileFormatError: 'Unknown signature' with: signature.
!

elementsFrom: aBinaryReadStream do: a2Block
	"this will loop over all the ZipFileEntries of a read stream evaluating a2Block for each.
	The first parameter to the block is the entry itself, the second is a ReadStream (an
	InflaterReadStream if the entry is compressed) or nil if the block does not have contents.
	A similar loop could be used to read the central directory elements that follow the
	ZipFileEntries, but if you are using the central directory, then you would probably
	be better off using the ZipFile class.
	NB: this method is intended as an *example* of how to read a Zip file without using the
	ZipFile class"

	"loop over the elements until we see the first entry from the central directory"
	[| entry |
	entry := self withSignatureFrom: aBinaryReadStream.
	entry isCentralDirectoryElement ifTrue: [^ self].

	"hack to make the above comment correct, even though it's probably a wast of time"
	entry hasContents ifFalse: [a2Block value: entry value: nil].

	entry withBinaryReaderDo:
		[:stream |

		a2Block value: entry value: stream.

		"we must ensure that we have read up to the end of the data or the stream
		won't be positioned at the start of the next record"
		stream notNil ifTrue: [stream setToEnd]].

	] repeat.!

elementsFromNonPositionableStream: aBinaryReadStream do: a2Block
	"this will loop over all the ZipFileEntries of a read stream evaluating a2Block for each.
	The first parameter to the block is the entry itself, the second is a ReadStream (an
	InflaterReadStream if the entry is compressed) or nil if the block does not have contents.
	This is functionally the same as elementsFrom:do: except that takes care not
	to require the stream to be positionable.
	NB: this method is intended as an *example* of how to read a Zip file without using the
	ZipFile class"

	"loop over the elements"
	self elementsFrom: aBinaryReadStream do:
		[:entry :stream |

		"we must run unbuffered or the inflater will probably overrun the end
		of the compressed data on the input stream"
		(stream notNil and: [entry isCompressed]) ifTrue: [stream unBufferInput].

		a2Block value: entry value: stream.

		].!

fileFormatError: aString with: anObject
	"trigger a standard ZipFile error with the given message and tag"

	ZipFileError signal: aString with: anObject.!

fixedRecordSize
	"answer the size of the fixed part of our record, this includes the size of the
	signature"

	^ self signature size + self fixedRecordSizeLessSignature.!

fixedRecordSizeLessSignature
	"answer the size of the fixed part of our record, excluding the size of the
	signature"

	self subclassResponsibility.!

from: aBinaryReadStream
	"answer a new instance populated with data read from aBinaryReadStream.
	NB: we assume that the reader of the stream has already consumed the signature"

	| pos |

	pos := aBinaryReadStream position - self signature size.

	^ (self newAt: pos on: aBinaryReadStream)
		readData;
		yourself.!

maxRecordSize
	"answer the maximum size of one of our recordr"

	^ (self fixedRecordSize + self maxVariableRecordSize) min: (2 ** 16).!

maxVariableRecordSize
	"answer the maximum size of the variable part of our record"

	^ self subclassResponsibility.!

new
	"answer a new instance with default initialisation"

	^ (self basicNew)
		initialize;
		yourself.!

newAt: anInteger on: aBinaryWriteStream
	"answer a new instance that knows that it will be written at the
	given offset on the given stream"

	^ (self new)
		stream: aBinaryWriteStream;
		positionInStream: anInteger;
		yourself.!

signature
	"answer the magic number that these elements use in ZIP files"

	self subclassResponsibility.!

withSignatureFrom: aBinaryReadStream
	"answer a new instance populated with data read from aBinaryReadStream.
	We read a signature and then use that to decode the type of the element itself"

	| subclass |

	subclass := self decodeSignatureFrom: aBinaryReadStream.

	^ subclass from: aBinaryReadStream.! !
!ZipFileElement class categoriesFor: #decodeSignatureFrom:!public!reading! !
!ZipFileElement class categoriesFor: #elementsFrom:do:!public!reading! !
!ZipFileElement class categoriesFor: #elementsFromNonPositionableStream:do:!public!reading! !
!ZipFileElement class categoriesFor: #fileFormatError:with:!exceptions!public! !
!ZipFileElement class categoriesFor: #fixedRecordSize!constants!public! !
!ZipFileElement class categoriesFor: #fixedRecordSizeLessSignature!constants!public! !
!ZipFileElement class categoriesFor: #from:!instance creation!public! !
!ZipFileElement class categoriesFor: #maxRecordSize!constants!public! !
!ZipFileElement class categoriesFor: #maxVariableRecordSize!constants!public! !
!ZipFileElement class categoriesFor: #new!instance creation!public! !
!ZipFileElement class categoriesFor: #newAt:on:!instance creation!public! !
!ZipFileElement class categoriesFor: #signature!constants!public! !
!ZipFileElement class categoriesFor: #withSignatureFrom:!instance creation!public! !

ZipFileError guid: (GUID fromString: '{887655DC-A4DF-4409-AF24-177C12E07D40}')!
ZipFileError comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org
'!
!ZipFileError categoriesForClass!Unclassified! !
ZipFileCentralDirectoryEnd guid: (GUID fromString: '{DD208669-C729-43F9-A884-CB9D99E0FE5E}')!
ZipFileCentralDirectoryEnd comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these stands for the ''summary'' record at the end of each Zip file.  It is the starring point for random-access reading of a Zip file since this "knows" where to find the table-of-contents.

You won''t need to mess with these at all if you read/write Zip files using the ZipFile class.'!
!ZipFileCentralDirectoryEnd categoriesForClass!Unclassified! !
!ZipFileCentralDirectoryEnd methodsFor!

comment
	"answer the receiver's comment String, usually nil"

	^ comment.
!

comment: aString
	"set the receiver's comment to aString"

	#CUtodo.  "should limit the size of whole entry to 64K"

	comment := aString.
!

directoryDiskNumber
	"answer the 0-based number of the disk where the first directory entry is to be found"

	^ directoryDiskNumber.
!

directoryDiskNumber: anInteger
	"set the 0-based number of the disk where the first directory entry is to be found"

	directoryDiskNumber := anInteger.
!

directoryOffset
	"answer the offset (apparently 1-based for some reason) of the start of the first directory
	entry relative to the disk on which it was found"

	^ directoryOffset.
!

directoryOffset: anInteger
	"set the offset (apparently 1-based for some reason) of the start of the first directory
	entry relative to the disk on which it was found"

	directoryOffset := anInteger.
!

directorySize
	"answer the size in bytes of the entire directory"

	^ directorySize.
!

directorySize: anInteger
	"set the size in bytes of the entire directory"

	directorySize := anInteger.
!

diskNumber
	"answer the 0-based number of the disk on receiver was found"

	^ diskNumber.
!

diskNumber: anInteger
	"set the 0-based number of the disk on receiver was found"

	diskNumber := anInteger.
!

entriesInAllDirectories
	"answer the total number of entries in the entire directory"

	^ entriesInAllDirectories.
!

entriesInAllDirectories: anInteger
	"set the total number of entries in the entire directory"

	entriesInAllDirectories := anInteger.
!

entriesInDirectoryOnThisDisk
	"answer the number of entries in the directory on this disk"

	^ entriesInDirectoryOnThisDisk.
!

entriesInDirectoryOnThisDisk: anInteger
	"set the number of entries in the directory on this disk"

	entriesInDirectoryOnThisDisk := anInteger.
!

initialize
	"private -- establish a coherent initial state"

	diskNumber := directoryDiskNumber := entriesInDirectoryOnThisDisk := entriesInAllDirectories := directorySize := directoryOffset := 0.
	comment := nil.

	super initialize.!

isCentralDirectoryElement
	"answer true if we one of the elements of the 'central directory' (table
	of contents of a zipfile)"

	^ true.!

isCentralDirectoryEnd
	"answer true if we are supposed to be the last element in a Zip file"

	^ true.!

isPlausible
	"private -- do some basic sanity checking on the values we contain"

	diskNumber >= directoryDiskNumber ifFalse: [^ false].
	entriesInDirectoryOnThisDisk <= entriesInAllDirectories ifFalse: [^ false].
	directorySize >= (entriesInAllDirectories * self class fixedRecordSize) ifFalse: [^ false].

	^ true.!

populateFrom: aBinaryReadStream
	"private -- populate our instance data by reading from the given stream"

	| commentLength |

	diskNumber := aBinaryReadStream nextWORD.
	directoryDiskNumber := aBinaryReadStream nextWORD.
	entriesInDirectoryOnThisDisk := aBinaryReadStream nextWORD.
	entriesInAllDirectories := aBinaryReadStream nextWORD.
	directorySize := aBinaryReadStream nextDWORD.
	directoryOffset:= aBinaryReadStream nextDWORD.
	commentLength := aBinaryReadStream nextWORD.

	comment := commentLength > 0 ifTrue: [(aBinaryReadStream next: commentLength) asString].
!

variableRecordSize
	"answer the size of the variable part of the record in a zip file"

	^ comment ifNil: [0] ifNotNil: [:it | it size].
!

writeDataTo: aBinaryWriteStream
	"private -- write our data to the given stream.
	NB: this is not symetrical with #populateFrom: in that it starts
	by writing the appropriate signature"

	| commentLength |

	commentLength := comment ifNil: [0] ifNotNil: [:it | it size].

	aBinaryWriteStream
		nextPutAll: self class signature;
		nextWORDPut: diskNumber;
		nextWORDPut: directoryDiskNumber;
		nextWORDPut: entriesInDirectoryOnThisDisk;
		nextWORDPut: entriesInAllDirectories;
		nextDWORDPut: directorySize;
		nextDWORDPut: directoryOffset;
		nextWORDPut: commentLength.

	comment ifNotNil: [:it | aBinaryWriteStream nextPutAll: it asByteArray].
! !
!ZipFileCentralDirectoryEnd categoriesFor: #comment!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #comment:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directoryDiskNumber!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directoryDiskNumber:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directoryOffset!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directoryOffset:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directorySize!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #directorySize:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #diskNumber!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #diskNumber:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #entriesInAllDirectories!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #entriesInAllDirectories:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #entriesInDirectoryOnThisDisk!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #entriesInDirectoryOnThisDisk:!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #initialize!initializing!private! !
!ZipFileCentralDirectoryEnd categoriesFor: #isCentralDirectoryElement!public!testing! !
!ZipFileCentralDirectoryEnd categoriesFor: #isCentralDirectoryEnd!public!testing! !
!ZipFileCentralDirectoryEnd categoriesFor: #isPlausible!helpers!private!testing! !
!ZipFileCentralDirectoryEnd categoriesFor: #populateFrom:!private!reading! !
!ZipFileCentralDirectoryEnd categoriesFor: #variableRecordSize!accessing!public! !
!ZipFileCentralDirectoryEnd categoriesFor: #writeDataTo:!private!writing! !

!ZipFileCentralDirectoryEnd class methodsFor!

fixedRecordSizeLessSignature
	"answer the size of the fixed part of our record, excluding the size of the
	signature"

	^ 18.!

maxVariableRecordSize
	"answer the maximum size of the variable part of our record"

	^ 2 ** 16.!

signature
	"answer the magic number that these elements use in ZIP files"

	"#(16r06 16r05 16r4B 16r50) asByteArray reverse"
	^ #[80 75 5 6].! !
!ZipFileCentralDirectoryEnd class categoriesFor: #fixedRecordSizeLessSignature!constants!public! !
!ZipFileCentralDirectoryEnd class categoriesFor: #maxVariableRecordSize!constants!public! !
!ZipFileCentralDirectoryEnd class categoriesFor: #signature!constants!public! !

ZipFileDataDescriptor guid: (GUID fromString: '{5326B8F1-21FD-43F1-9152-28D1C088199F}')!
ZipFileDataDescriptor comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

When a Zip file is created without seeking backward in the file (as when writting a tape for instance, or using fairly dumb software such as Sun''s implemementation of Zip file handling for Java), the file sizes and checksum are not inlcuded in the ZipFileEntry that preceeds the sub-file''s contents in the Zip file.  In such cases, an additional small record follows the contents, to state what the sizes/checksum should ahve been (only usefull as a check, of course, since you can''t *find* the record without knowing where the contents end...).

These objects stand for such records.

Normal use of this package (even low-level use) should not need to worry about these records since ZipFileEntries do all the necessary housework.
'!
!ZipFileDataDescriptor categoriesForClass!Unclassified! !
!ZipFileDataDescriptor methodsFor!

compressedSize
	"answer the receiver's compressed size"

	^ compressedSize.
!

compressedSize: anInteger
	"set the receiver's compressed size to anInteger"

	compressedSize := anInteger.
!

copyDataFrom: aZipFileEntry
	"private -- copy the size and position data from the given entry"

	crc := aZipFileEntry crc.
	compressedSize := aZipFileEntry compressedSize.
	uncompressedSize := aZipFileEntry uncompressedSize.!

crc
	"answer the receiver's crc"

	^ crc.
!

crc: anInteger
	"set the receiver's crc to anInteger"

	crc := anInteger.
!

populateFrom: aBinaryReadStream
	"private -- populate our instance data by reading from the given stream"

	crc := aBinaryReadStream nextDWORD.
	compressedSize := aBinaryReadStream nextDWORD.
	uncompressedSize := aBinaryReadStream nextDWORD.
!

uncompressedSize
	"answer the receiver's uncompressed size"

	^ uncompressedSize.
!

uncompressedSize: anInteger
	"set the receiver's uncompressed size to anInteger"

	uncompressedSize := anInteger.
!

variableRecordSize
	"answer the size of the variable part of the record in a zip file"

	^ 0.
!

writeDataTo: aBinaryWriteStream
	"private -- write our data to the given stream.
	NB: this is not symetrical with #populateFrom: in that it starts
	by writing the appropriate signature"

	aBinaryWriteStream
		nextPutAll: self class signature;
		nextDWORDPut: crc;
		nextDWORDPut: compressedSize;
		nextDWORDPut: uncompressedSize.
! !
!ZipFileDataDescriptor categoriesFor: #compressedSize!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #compressedSize:!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #copyDataFrom:!helpers!private! !
!ZipFileDataDescriptor categoriesFor: #crc!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #crc:!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #populateFrom:!private!reading! !
!ZipFileDataDescriptor categoriesFor: #uncompressedSize!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #uncompressedSize:!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #variableRecordSize!accessing!public! !
!ZipFileDataDescriptor categoriesFor: #writeDataTo:!private!writing! !

!ZipFileDataDescriptor class methodsFor!

fixedRecordSizeLessSignature
	"answer the size of the fixed part of our record, excluding the size of the
	signature"

	^ 12.!

for: aZipFileEntry
	"answer a new instance that is dupluicates the size data from the given
	existing ZipFileEntry"

	^ (self newAt: aZipFileEntry positionOfNextInStream on: aZipFileEntry stream)
		copyDataFrom: aZipFileEntry;
		yourself.!

maxVariableRecordSize
	"answer the maximum size of the variable part of our record"

	^ 0.!

signature
	"answer the magic number that these elements use in ZIP files"

	"#(16r08 16r07 16r4B 16r50) asByteArray reverse"
	^  #[80 75 7 8].! !
!ZipFileDataDescriptor class categoriesFor: #fixedRecordSizeLessSignature!constants!public! !
!ZipFileDataDescriptor class categoriesFor: #for:!instance creation!public! !
!ZipFileDataDescriptor class categoriesFor: #maxVariableRecordSize!constants!public! !
!ZipFileDataDescriptor class categoriesFor: #signature!constants!public! !

ZipFileEntryBase guid: (GUID fromString: '{D14B5B1D-E598-478F-8A34-E3BA73D7F543}')!
ZipFileEntryBase comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org
'!
!ZipFileEntryBase categoriesForClass!Unclassified! !
!ZipFileEntryBase methodsFor!

bePairedWith: aZipFileEntryBase
	"private -- set up the paring relationship with the given ZipFileEntry or ZipFileCentralDirectoryEntry"

	self pairedEntry: aZipFileEntryBase.
	aZipFileEntryBase pairedEntry: self.
	self copyDataFromPairedEntry.!

binaryContents
	"answer our uncompressed contents as a (possibly empty) ByteArray or nil if we don't have any.
	Throws errors if the contents' checksum doesn't match"

	^ self contentsAsBinary: true.
!

centralDirectoryEntry
	"answer the ZipFileCentralDirectoryEntry for this file"

	self subclassResponsibility.
!

compressedSize
	"answer the receiver's compressed size"

	^ compressedSize.
!

compressedSize: anInteger
	"set the receiver's compressed size to anInteger"

	compressedSize := anInteger.
	self propagateToPair: [:it | it compressedSize: anInteger].
!

compressionLevel
	"answer the receiver's compression lvel (one of the Z_*_COMPRESSION (level) values).
	This is ignored unless we are set to use a DEFLATE compression"

	^ compressionLevel.
!

compressionLevel: anInteger
	"set the receiver's compression level to anInteger (one of the Z_*_COMPRESSION (level)values).
	This is ignored unless we are set to use a DEFLATE compression.
	Note that setting a compression level of 0 (Z_NO_COMPRESSION) is not the same as telling
	us not to use compression -- in the former case we still use the zlib library to DEFLATE the
	contents (with zero compression), in the latter case we just STORE the contents"

	| bits |

	self isDeflaterCompressed ifFalse: [^ self].

	"we'd like to keep the compression level flag bits correct"
	bits := anInteger = Z_NO_COMPRESSION ifTrue: [6]
		ifFalse: [anInteger = Z_BEST_SPEED ifTrue: [4]
		ifFalse: [anInteger = Z_BEST_COMPRESSION ifTrue: [2]
		ifFalse: [0]]].

	compressionLevel := anInteger.
	flags := (flags maskClear: ZF_DEFLATE_STRENGTH_MASK) bitOr: bits.
	self propagateToPair: [:it | it compressionLevel: anInteger].
!

compressionMethod
	"answer the receiver's compression method (one of the ZF_*_COMPRESSION values)"

	^ compressionMethod.
!

compressionMethod: anInteger
	"set the receiver's compression method to anInteger (one of the ZF_*_COMPRESSION values)"

	compressionMethod := anInteger.
	self propagateToPair: [:it | it compressionMethod: anInteger].
!

contentsAsBinary: aBool
	"answer our uncompressed contents or nil if we don't have any.
	The contents will be a ByteArray or a String according as aBool is true.
	Throws errors if the contents' checksum doesn't match"

	self subclassResponsibility.
!

copyBaseDataFrom: aZipFileEntryBase
	"private -- copy our data from the given entry"

	versionToExtractHigh := aZipFileEntryBase versionToExtractHigh.
	versionToExtractLow := aZipFileEntryBase versionToExtractLow.
	flags := aZipFileEntryBase flags.
	compressionMethod := aZipFileEntryBase compressionMethod.
	lastModifiedTime := aZipFileEntryBase lastModifiedTime.
	lastModifiedDate := aZipFileEntryBase lastModifiedDate.
	crc := aZipFileEntryBase crc.
	compressedSize := aZipFileEntryBase compressedSize.
	uncompressedSize := aZipFileEntryBase uncompressedSize.
	filename := aZipFileEntryBase filename.
	extraField := aZipFileEntryBase extraField.
!

copyDataFromPairedEntry
	"private -- copy any shared data from our paired entry"

	self copyBaseDataFrom: pairedEntry.!

crc
	"answer the receiver's crc"

	^ crc.
!

crc: anInteger
	"set the receiver's crc to anInteger"

	crc := anInteger.
	self propagateToPair: [:it | it crc: anInteger].
!

decompressionIsSelfDelimiting
	"answer whether our compression scheme is one where we can read the
	compressed data from a stream without knowing it's size in advance"

	^ self isDeflaterCompressed.!

defaultCompressionLevel
	"answer the Z_*_COMPRESSION (level) that instances will use by default"

	^ Z_DEFAULT_COMPRESSION.!

defaultCompressionMethod
	"answer the ZF_*_COMPRESSION that instances will use by default"

	^ ZF_DEFLATE_COMPRESSION.!

defaultDate
	"answer the last-modified-date to use by default"

	^ DefaultDate.!

defaultTime
	"answer the last-modified-time to use by default"

	^ DefaultTime.!

defaultVersionToExtractHigh
	"answer the version-to-extract high byte to use by default"

	^ ZF_VERSION_FAT_FS.!

defaultVersionToExtractLow
	"answer the version-to-extract low byte to use by default"

	"this is just copied from one of the widespread Zip tools"
	^ 20.
!

displayOn: aStream
	"write a user-centric representation of ourselves to aStream"

	aStream display: self filenameOrStdin.!

extraField
	"answer the receiver's extraField as a raw ByteArray or nil"

	^ extraField.
!

extraField: aByteArray
	"set the receiver's extraField to the raw data given as aByteArray"

	#CUtodo.  "should limit the size of whole entry to 64K"

	extraField := aByteArray.
	self propagateToPair: [:it | it extraField: aByteArray].
!

fileEntry
	"answer the ZipFileEntry for this file"

	self subclassResponsibility.!

filename
	"answer the receiver's filename"

	^ filename.
!

filename: aString
	"set the receiver's filename to aString.
	NB: this must not be used on entries that are held as part of a ZipFile since
	that would invalidate its table of contents"

	#CUtodo.  "should limit the size of whole entry to 64K"

	"self assert: [(aString includes: $\) not]."

	filename := aString.
	self propagateToPair: [:it | it filename: aString].
!

filenameOrStdin
	"answer the receiver's filename or the string '<stdin>' if there is no filename"

	^ filename ifNil: ['<stdin>'].
!

flags
	"answer the receiver's flags"

	^ flags.
!

flags: anInteger
	"set the receiver's flags to anInteger"

	flags := anInteger.
	self propagateToPair: [:it | it flags: anInteger].
!

getContentsSizes
	"ensure that we know our contents sizes.  If we do not then this
	will have the side-effect of reading the contents from file (since
	that's the only way to find how big the contents is, and thus to
	find the ZipFileDataDescriptor that follows the contents"

	self knowsContentsSizes ifTrue: [^ self].
	self hasContents ifFalse: [^ self].

	self withBinaryReaderDo: [:reader | reader setToEnd].!

hasCompressedContents
	"answer whether our contents are in a compressed format"

	^ self hasContents and: [self isCompressed].!

hasContents
	"answer whether this zip entry has any contents (directories do
	not, zero-length files *do*)"

	^ self isDirectory not.!

initialize
	"private -- establish a coherent initial state"

	versionToExtractHigh := self defaultVersionToExtractHigh.
	versionToExtractLow := self defaultVersionToExtractLow.
	flags := 0.
	compressionMethod := self defaultCompressionMethod.
	lastModifiedTime := self defaultTime.
	lastModifiedDate := self defaultDate.
	crc := 0.
	compressedSize := 0.
	uncompressedSize := 0.
	filename := extraField := nil.
	compressionLevel := self defaultCompressionLevel.
!

isCompressed
	"answer whether this zip entry is compressed"

	^ compressionMethod ~= ZF_NO_COMPRESSION.!

isDeflaterCompressed
	"answer whether this zip entry is compressed with the raw 'deflater' method of zlib"

	^ compressionMethod = ZF_DEFLATE_COMPRESSION.!

isDirectory
	"answer true if this looks like the entry for a directory.
	NB: this is nothing to do with 'central directory entries' -- bad naming in
	the Zip format"

	^ filename
		ifNil: [false]
		ifNotNil: [:it | it endsWith: '/'].!

isEncrypted
	"answer whether we should/do use Zipfile encryption"

	^ flags allMask: ZF_CONTENT_IS_ENCRYPTED_MASK.!

isEncrypted: aBool
	"set whether we should/do use Zipfile encryption.
	NB: encryption is not supported, so there's not a *lot* of
	point in calling this"

	"we don't do encryption"
	aBool ifTrue: [self fileFormatError: 'Encryption not supported'].

	self flags: (flags mask: ZF_CONTENT_IS_ENCRYPTED_MASK set: aBool).!

isNotCompressed
	"answer whether this zip entry is uncompressed"

	^ compressionMethod = ZF_NO_COMPRESSION.!

knowsContentsSizes
	"answer whether we know our content's sizes and crc -- we may not yet if
	we haven't seen the data"

	^ (flags allMask: ZF_NO_CONTENTS_SIZE_MASK) not.!

knowsContentsSizes: aBool
	"private -- set whether we know our content's sizes and crc -- we may not yet if
	we haven't seen the data"

	| newFlags |

	newFlags := flags
			mask: ZF_NO_CONTENTS_SIZE_MASK
			set: aBool not.

	"logically we should probably not propagate the change to the cd entry,
	however some readers complain if the flags are different, so..."
	self flags: newFlags.!

lastModifiedDate
	"answer the receiver's last modified date"

	^ lastModifiedDate.
!

lastModifiedDate: aDate
	"set the receiver's last modified date"

	lastModifiedDate := aDate.
	self propagateToPair: [:it | it lastModifiedDate: aDate].
!

lastModifiedTime
	"answer the receiver's last modified time"

	^ lastModifiedTime.
!

lastModifiedTime: aTime
	"set the receiver's last modified time"

	lastModifiedTime := aTime.
	self propagateToPair: [:it | it lastModifiedTime: aTime].
!

pairedEntry
	"private -- answer the ZipFileEntry or ZipFileCentralDirectoryEntry with which we are paired.
	May be nil if we are not yet paired"

	^ pairedEntry.!

pairedEntry: aZipFileEntryBase
	"private -- remember that we are paired with the given ZipFileEntry or ZipFileCentralDirectoryEntry"

	pairedEntry := aZipFileEntryBase.!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' filename: ';
		display: self.!

propagateToPair: a1Block
	"private -- if we have a paired entry then evaluate a1Block
	whilst temporarily breaking the linkage (so that we don't
	recurse infinitely)"

	pairedEntry isNil ifFalse:
		[pairedEntry pairedEntry: nil.
		a1Block value: pairedEntry.
		pairedEntry pairedEntry: self].!

shortName
	"answer the last segment of our filename or nil if we don't have one"

	| index |

	filename isNil ifTrue: [^ nil].

	"note that we leave the trailing '/' on the end of directory names"
	index := filename prevIndexOf: $/ from: filename size - 1 to: 1.
	^ index = 0
		ifTrue: [filename]
		ifFalse: [filename copyFrom: index + 1].!

shortNameOrStdin
	"answer the last segment of the receiver's filename or the string '<stdin>' if there is no filename"

	^ self shortName ifNil: ['<stdin'].
!

textContents
	"answer our uncompressed contents as a (possibly empty) String or nil if we don't have any.
	Throws errors if the contents' checksum doesn't match"

	^ self contentsAsBinary: false.!

timestampNow
	"set the last modified date and time to the current values taken from the
	system"

	self
		lastModifiedDate: Date today;
		lastModifiedTime: Time now.!

uncompressedSize
	"answer the receiver's uncompressed size"

	^ uncompressedSize.
!

uncompressedSize: anInteger
	"set the receiver's uncompressed size to anInteger"

	uncompressedSize := anInteger.
	self propagateToPair: [:it | it uncompressedSize: anInteger].
!

updateCompressionLevel
	"private -- update our compression level from the flags field"

	| bits |

	self isDeflaterCompressed ifFalse: [^ self].

	bits := flags bitAnd: ZF_DEFLATE_STRENGTH_MASK.
	compressionLevel := bits = 6 ifTrue: [Z_NO_COMPRESSION]
					ifFalse: [bits = 4 ifTrue: [Z_BEST_SPEED]
					ifFalse: [bits = 2 ifTrue: [Z_BEST_COMPRESSION]
					ifFalse: [Z_DEFAULT_COMPRESSION]]].
!

useCompression: aBool
	"set whether we will use compression when our contents are written out.
	It is wrong (i.e. an error that is not checked) to use this if we have already
	been written to, or were read from, file"

	| method |

	"we only support DEFLATE compression"
	method := aBool
			ifTrue: [ZF_DEFLATE_COMPRESSION]
			ifFalse: [ZF_NO_COMPRESSION].

	self compressionMethod: method.
!

usesCompression
	"answer whether we will use compression when our contents are written out.
	(Provided for symmetry with #useCompression:, although it is necessarily
	synonymous with #isCompressed)"

	^ self isCompressed.!

versionToExtractHigh
	"answer the receiver's version-to-extract high byte (one of the ZF_VERSION_* values).
	This identifies the OS/File system that is the mininum to
	extract this file"

	^ versionToExtractHigh.
!

versionToExtractHigh: aByte
	"set the receiver's version-to-extract high byte (one of the ZF_VERSION_* values).
	This identifies the OS/File system that is the mininum to
	extract this file"

	versionToExtractHigh := aByte.
	self propagateToPair: [:it | it versionToExtractHigh: aByte].
!

versionToExtractLow
	"answer the receiver's version to extract low byte"

	^ versionToExtractLow.
!

versionToExtractLow: aByte
	"set the receiver's version to extract low byte"

	versionToExtractLow := aByte.
	self propagateToPair: [:it | it versionToExtractLow: aByte].
!

versionToExtractMajor
	"answer the major number from the version-to-extract field"

	^ versionToExtractLow // 10.
!

versionToExtractMajor: anInteger1 minor: anInteger2
	"set the receiver's version-to-extract to the combination of the given major and minor version numbers"

	self versionToExtractLow: (anInteger1 * 10 + anInteger2).!

versionToExtractMinor
	"answer the minor number from the version-to-extract field"

	^ versionToExtractLow \\ 10.
!

withBinaryReaderDo: a1Block
	"evaluate the <monadicValuable> a1Block passing a binary ReadStream that can be used to read the
	uncompressed contents of this entry.  If we do not have contents (we represent a directory) then
	a1Block is not evaluated.  If we have contents, but the contents are zero-length (an empty file) then the
	ReadStream will already be #atEnd.  If the contents are compressed then the ReadStream will be an
	InflaterReadStream, otherwise it'll be a ReadStream on a ByteArray of our contents.
	NB1: the the InflaterReadStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to read more data.
	NB2: if the checksums and sizes of the contents don't match what we think they should be, then
	this will throw a ZipError *after* executing the Block (we can't check before 'cos we don't know
	what the contents will decompress to until we've decompressed them...)"

	^ self withReaderDo: a1Block binary: true.
!

withBinaryWriterDo: a1Block
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a binary WriteStream that
	can be used to write the uncompressed contents of this entry (not called if we don't have data), and
	finally clean up by ensuring that the sizes and checksums on disk are correct.  It does NOT write a central
	directory entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	NB1: If the contents are to be compressed then the ReadStream will be an DeflaterWriteStream, otherwise
	it'll be a WriteStream onto a ByteArray (which means that uncompressed entries are assembled entirely
	in memory before being written out -- I intend to fix that someday).
	NB2: the the DeflaterWriteStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to write more data"

	self withWriterDo: a1Block backPatchSizes: true binary: true.!

withReaderDo: a1Block binary: aBool
	"evaluate the <monadicValuable> a1Block passing a ReadStream that can be used to read the
	uncompressed contents of this entry.  If we do not have contents (we represent a directory) then
	a1Block is not evaluated.  If we have contents, but the contents are zero-length (an empty file) then the
	ReadStream will already be #atEnd.  If the contents are compressed then the ReadStream will be an
	InflaterReadStream, otherwise it'll be a ReadStream on a ByteArray of our contents.
	If aBool is true then the reader will be in binary mode, otherwise it'll be in text-mode.
	NB1: the the InflaterReadStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to read more data.
	NB2: if the checksums and sizes of the contents don't match what we think they should be, then
	this will throw a ZipError *after* executing the Block (we can't check before 'cos we don't know
	what the contents will decompress to until we've decompressed them...)"

	self subclassResponsibility.!

withTextReaderDo: a1Block
	"evaluate the <monadicValuable> a1Block passing a text-mode ReadStream that can be used to read the
	uncompressed contents of this entry.  If we do not have contents (we represent a directory) then
	a1Block is not evaluated.  If we have contents, but the contents are zero-length (an empty file) then the
	ReadStream will already be #atEnd.  If the contents are compressed then the ReadStream will be an
	InflaterReadStream, otherwise it'll be a ReadStream on a String of our contents.
	NB1: the the InflaterReadStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to read more data.
	NB2: if the checksums and sizes of the contents don't match what we think they should be, then
	this will throw a ZipError *after* executing the Block (we can't check before 'cos we don't know
	what the contents will decompress to until we've decompressed them...)"

	^ self withReaderDo: a1Block binary: false.	!

withTextWriterDo: a1Block
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a text-mode WriteStream that
	can be used to write the uncompressed contents of this entry (not called if we don't have data), and
	finally clean up by ensuring that the sizes and checksums on disk are correct.  It does NOT write a central
	directory entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	NB1: If the contents are to be compressed then the ReadStream will be an DeflaterWriteStream, otherwise
	it'll be a WriteStream onto a String (which means that uncompressed entries are assembled entirely
	in memory before being written out -- I intend to fix that someday).
	NB2: the the DeflaterWriteStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to write more data"

	self withWriterDo: a1Block backPatchSizes: true binary: false.!

withWriterDo: a1Block backPatchSizes: aBool binary: anotherBool
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a WriteStream that
	can be used to write the uncompressed contents of this entry (not called if we don't have data), and
	finally clean up by ensuring that the sizes and checksums on disk are correct.  It does NOT write a central
	directory entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	The write stream will expect character data or binary data according to anotherBool.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool.
	NB1: If the contents are to be compressed then the ReadStream will be an DeflaterWriteStream, otherwise
	it'll be a WriteStream onto a ByteArray or String (which means that uncompressed entries are assembled entirely
	in memory before being written out -- I intend to fix that someday).
	NB2: the the DeflaterWriteStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to write more data"

	self subclassResponsibility.
!

write
	"write a file entry followed by NO contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the
	stream until after all the file entries"

	"this is only a safe implementation because we know that
	#write: has the necessary checks for empty arrays"
	self write: #[] backPatchSizes: false binary: false.

!

write: aStringOrByteArray backPatchSizes: aBool binary: anotherBool
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool"

	self subclassResponsibility.
!

writeBinary: aByteArray
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries"

	self writeBinary: aByteArray backPatchSizes: true.!

writeBinary: aByteArray backPatchSizes: aBool
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool"

	self write: aByteArray backPatchSizes: aBool binary: true.
!

writeText: aString
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries"

	self writeText: aString backPatchSizes: true.!

writeText: aString backPatchSizes: aBool
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool"

	self write: aString backPatchSizes: aBool binary: false.
! !
!ZipFileEntryBase categoriesFor: #bePairedWith:!initializing!private! !
!ZipFileEntryBase categoriesFor: #binaryContents!accessing!public!reading! !
!ZipFileEntryBase categoriesFor: #centralDirectoryEntry!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressedSize!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressedSize:!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressionLevel!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressionLevel:!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressionMethod!accessing!public! !
!ZipFileEntryBase categoriesFor: #compressionMethod:!accessing!public! !
!ZipFileEntryBase categoriesFor: #contentsAsBinary:!accessing!public!reading! !
!ZipFileEntryBase categoriesFor: #copyBaseDataFrom:!helpers!private! !
!ZipFileEntryBase categoriesFor: #copyDataFromPairedEntry!helpers!private! !
!ZipFileEntryBase categoriesFor: #crc!accessing!public! !
!ZipFileEntryBase categoriesFor: #crc:!accessing!public! !
!ZipFileEntryBase categoriesFor: #decompressionIsSelfDelimiting!public!testing! !
!ZipFileEntryBase categoriesFor: #defaultCompressionLevel!constants!public! !
!ZipFileEntryBase categoriesFor: #defaultCompressionMethod!constants!public! !
!ZipFileEntryBase categoriesFor: #defaultDate!constants!public! !
!ZipFileEntryBase categoriesFor: #defaultTime!constants!public! !
!ZipFileEntryBase categoriesFor: #defaultVersionToExtractHigh!constants!public! !
!ZipFileEntryBase categoriesFor: #defaultVersionToExtractLow!constants!public! !
!ZipFileEntryBase categoriesFor: #displayOn:!displaying!public! !
!ZipFileEntryBase categoriesFor: #extraField!accessing!public! !
!ZipFileEntryBase categoriesFor: #extraField:!accessing!public! !
!ZipFileEntryBase categoriesFor: #fileEntry!accessing!public! !
!ZipFileEntryBase categoriesFor: #filename!accessing!public! !
!ZipFileEntryBase categoriesFor: #filename:!accessing!public! !
!ZipFileEntryBase categoriesFor: #filenameOrStdin!accessing!public! !
!ZipFileEntryBase categoriesFor: #flags!accessing!public! !
!ZipFileEntryBase categoriesFor: #flags:!accessing!public! !
!ZipFileEntryBase categoriesFor: #getContentsSizes!accessing!public! !
!ZipFileEntryBase categoriesFor: #hasCompressedContents!public!testing! !
!ZipFileEntryBase categoriesFor: #hasContents!public!testing! !
!ZipFileEntryBase categoriesFor: #initialize!initializing!private! !
!ZipFileEntryBase categoriesFor: #isCompressed!public!testing! !
!ZipFileEntryBase categoriesFor: #isDeflaterCompressed!public!testing! !
!ZipFileEntryBase categoriesFor: #isDirectory!public!testing! !
!ZipFileEntryBase categoriesFor: #isEncrypted!public!testing! !
!ZipFileEntryBase categoriesFor: #isEncrypted:!accessing!public! !
!ZipFileEntryBase categoriesFor: #isNotCompressed!public!testing! !
!ZipFileEntryBase categoriesFor: #knowsContentsSizes!public!testing! !
!ZipFileEntryBase categoriesFor: #knowsContentsSizes:!accessing!private! !
!ZipFileEntryBase categoriesFor: #lastModifiedDate!accessing!public! !
!ZipFileEntryBase categoriesFor: #lastModifiedDate:!accessing!public! !
!ZipFileEntryBase categoriesFor: #lastModifiedTime!accessing!public! !
!ZipFileEntryBase categoriesFor: #lastModifiedTime:!accessing!public! !
!ZipFileEntryBase categoriesFor: #pairedEntry!accessing!private! !
!ZipFileEntryBase categoriesFor: #pairedEntry:!initializing!private! !
!ZipFileEntryBase categoriesFor: #printOn:!printing!public! !
!ZipFileEntryBase categoriesFor: #propagateToPair:!helpers!private! !
!ZipFileEntryBase categoriesFor: #shortName!accessing!public! !
!ZipFileEntryBase categoriesFor: #shortNameOrStdin!accessing!public! !
!ZipFileEntryBase categoriesFor: #textContents!accessing!public!reading! !
!ZipFileEntryBase categoriesFor: #timestampNow!operations!public! !
!ZipFileEntryBase categoriesFor: #uncompressedSize!accessing!public! !
!ZipFileEntryBase categoriesFor: #uncompressedSize:!accessing!public! !
!ZipFileEntryBase categoriesFor: #updateCompressionLevel!helpers!private! !
!ZipFileEntryBase categoriesFor: #useCompression:!accessing!public! !
!ZipFileEntryBase categoriesFor: #usesCompression!public!testing! !
!ZipFileEntryBase categoriesFor: #versionToExtractHigh!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractHigh:!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractLow!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractLow:!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractMajor!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractMajor:minor:!accessing!public! !
!ZipFileEntryBase categoriesFor: #versionToExtractMinor!accessing!public! !
!ZipFileEntryBase categoriesFor: #withBinaryReaderDo:!public!reading! !
!ZipFileEntryBase categoriesFor: #withBinaryWriterDo:!helpers!public!writing! !
!ZipFileEntryBase categoriesFor: #withReaderDo:binary:!public!reading! !
!ZipFileEntryBase categoriesFor: #withTextReaderDo:!public!reading! !
!ZipFileEntryBase categoriesFor: #withTextWriterDo:!helpers!public!writing! !
!ZipFileEntryBase categoriesFor: #withWriterDo:backPatchSizes:binary:!helpers!public!writing! !
!ZipFileEntryBase categoriesFor: #write!helpers!public!writing! !
!ZipFileEntryBase categoriesFor: #write:backPatchSizes:binary:!public!writing! !
!ZipFileEntryBase categoriesFor: #writeBinary:!public!writing! !
!ZipFileEntryBase categoriesFor: #writeBinary:backPatchSizes:!public!writing! !
!ZipFileEntryBase categoriesFor: #writeText:!public!writing! !
!ZipFileEntryBase categoriesFor: #writeText:backPatchSizes:!public!writing! !

!ZipFileEntryBase class methodsFor!

dateAsDosWORD: aDate
	"answer a Date converted into 2-byte DOS format"

	| y m d |

	y := (aDate year - 1980 bitAnd: 16r7F) bitShift: 9.
	m := (aDate monthIndex bitAnd: 16rF) bitShift: 5.
	d := aDate dayOfMonth bitAnd: 16r1F.

	^ (y bitOr: m) bitOr: d.
!

dosDateFromWORD: anInteger
	"answer a Date converted from 2-byte DOS format"

	| y m d |

	y := ((anInteger bitShift: -9) bitAnd: 16r7F)  + 1980.
	m := (anInteger bitShift: -5) bitAnd: 16rF.
	d := anInteger bitAnd: 16r1F.

	"it seems that some JAR files are built with software that uses
	0-based indexes here.  There's not much we can do about that
	except catch the obvious case"
	#CUtodo.  "investigate further (e.g. the tagsoup.jar)"
	(m = 0 or: [d = 0]) ifTrue: [m := m + 1. d := d + 1].

	^ Date newDay: d monthIndex: m year: y.!

dosTimeFromWORD: anInteger
	"answer a Time converted from 2-byte DOS format"

	| h m s |

	h := (anInteger bitShift: -11) bitAnd: 16r1F.
	m := (anInteger bitShift: -5) bitAnd: 16r3F.
	s := (anInteger bitAnd: 16r1F) * 2.

	^ Time fromSeconds: (h * 60 + m * 60 + s).!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	DefaultTime := Time fromSeconds: 0.
	DefaultDate := Date fromSeconds: 0.
!

timeAsDosWORD: aTime
	"answer a Time converted into 2-byte DOS format"

	| h m s |

	h := (aTime hours bitAnd: 16r1F) bitShift: 11.
	m := (aTime minutes bitAnd: 16r3F) bitShift: 5.
	s := (aTime seconds // 2) bitAnd: 16r1F.

	^ (h bitOr: m) bitOr: s.! !
!ZipFileEntryBase class categoriesFor: #dateAsDosWORD:!helpers!public! !
!ZipFileEntryBase class categoriesFor: #dosDateFromWORD:!helpers!public! !
!ZipFileEntryBase class categoriesFor: #dosTimeFromWORD:!helpers!public! !
!ZipFileEntryBase class categoriesFor: #initialize!initializing!private! !
!ZipFileEntryBase class categoriesFor: #timeAsDosWORD:!helpers!public! !

ZipFileCentralDirectoryEntry guid: (GUID fromString: '{B59B3622-E525-41B5-93B5-54DC4A019B0B}')!
ZipFileCentralDirectoryEntry comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Each of these corresponds to an entry in the ''table of contents'' at the end of every Zip file.  Each one is paired with a ZipFileEntry which represents the "real" file entry in the body of the Zip file.

We contain all the data that or #fileEntry does (and we keep it in synch too), plus we have a little exrtra data such as a comment and an indication of whether we stand for a text or binary file.

For convenience, we provide a few forwarding methods in categories ''reading'' and ''writing'' to allow clients of the ZipFile class (the public API of which is entirely in terms of instances of this class, rather than ZipFileEntries) to create or read Zip files without constantly having to ask for our #fileEnty.  However these methods *are* only a convenience, and there is no reason why you should not ask for the #fileEntry if you want more control than is provided by our API here.'!
!ZipFileCentralDirectoryEntry categoriesForClass!Unclassified! !
!ZipFileCentralDirectoryEntry methodsFor!

beBinary
	"set us to think this file contains binary data"

	self beText: false.!

beBinary: aBool
	"set whether we think this file contains textual data"

	self beText: aBool not.!

beText
	"set us to think this file contains textual data"

	self beText: true.!

beText: aBool
	"set whether we think this file contains textual data"

	self internalAttributes: (internalAttributes mask: ZF_CONTENT_IS_TEXT_MASK set: aBool).!

centralDirectoryEntry
	"answer the ZipFileCentralDirectoryEntry for this file"

	"we are that entry"
	^ self.
!

comment
	"answer the receiver's comment String, usually nil"

	^ comment.
!

comment: aString
	"set the receiver's comment to aString"

	#CUtodo.  "should limit the size of whole entry to 64K"

	comment := aString.
!

contents
	"answer our uncompressed contents or nil if we don't have any.
	The contents will be a ByteArray if we are #isBinary, or a String if we are #isText.
	Throws errors if the contents' checksum doesn't match.
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	^ self contentsAsBinary: self isBinary.!

contentsAsBinary: aBool
	"answer our uncompressed contents or nil if we don't have any.
	The contents will be a ByteArray or a String according as aBool is true.
	Throws errors if the contents' checksum doesn't match"

	^ self fileEntry contentsAsBinary: aBool.!

copyDataFromPairedEntry
	"private -- copy any shared data from our paired entry"

	super copyDataFromPairedEntry.

	offsetOfLocalHeader := self pairedEntry positionInStream.

	self updateExternalAttributesFromFilename.!

copyToZipFile: aZipFile
	"add a copy of our data and contents to the given ZipFile.
	NB: obviously we should not be part of the destination file
	already"

	| new |

	#CUtodo.	"refactor this so that clients can control how the copy is made"
	#CUtodo.	"use a raw copy where possible (of the data or the entire entry)"

	"ugh!!"
	new := (aZipFile openEntry: self filename)
			versionToExtractHigh: versionToExtractHigh;
			versionToExtractLow: versionToExtractLow;
			flags: flags;
			compressionMethod: compressionMethod;
			lastModifiedTime: lastModifiedTime;
			lastModifiedDate: lastModifiedDate;
			crc: crc;
			compressedSize: compressedSize;
			uncompressedSize: uncompressedSize;
		"	filename: filename;		-- already set "
			extraField: extraField;
			versionMadeByHigh: versionMadeByHigh;
			versionMadeByLow: versionMadeByLow;
			diskNumberStart: diskNumberStart;
			internalAttributes: internalAttributes;
			externalAttributes: externalAttributes;
			lastModifiedDate: lastModifiedDate;
			comment: comment;
			yourself.

	self withBinaryReaderDo:
		[:reader | new withBinaryWriterDo:
			[:writer | [reader atEnd] whileFalse:
					[writer nextPutAll: (reader nextAvailable: 1024)]]].

	aZipFile closeEntry.!

defaultVersionMadeByHigh
	"answer the version-made-by high byte to use by default"

	^ ZF_VERSION_FAT_FS.!

defaultVersionMadeByLow
	"answer the version-made-by low byte to use by default"

	"this is just copied from one of the widespread Zip tools"
	^ 20.!

diskNumberStart
	"answer the receiver's diskNumberStart"

	^ diskNumberStart.
!

diskNumberStart: anInteger
	"set the receiver's diskNumberStart to anInteger"

	diskNumberStart := anInteger.
!

externalAttributes
	"answer the receiver's externalAttributes (a largely undefined concept)"

	^ externalAttributes.
!

externalAttributes: anInteger
	"set the receiver's externalAttributes to anInteger"

	externalAttributes := anInteger.
!

fileEntry
	"answer the ZipFileEntry for this file"

	^ self pairedEntry ifNil: [self readFileEntry].!

filename: aString
	"set the receiver's filename to aString.
	NB: this must not be used on entries that are held as part of a ZipFile since
	that would invalidate its table of contents"

	super filename: aString.

	self updateExternalAttributesFromFilename.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.

	versionMadeByHigh := self defaultVersionMadeByHigh.
	versionMadeByLow  := self defaultVersionMadeByLow.
	diskNumberStart := 0.
	internalAttributes := 0.
	externalAttributes := 0.
	offsetOfLocalHeader := nil.	"not zero to force early error"
	comment := nil.
!

internalAttributes
	"answer the receiver's internalAttributes (a bitmap of flags)"

	^ internalAttributes.
!

internalAttributes: anInteger
	"set the receiver's internalAttributes to anInteger"

	internalAttributes := anInteger.
!

isBinary
	"answer whether we think this file contains binary data.
	NB: this is *not* reliable!!"

	^ self isText not.!

isCentralDirectoryElement
	"answer true if we one of the elements of the 'central directory' (table
	of contents of a zipfile)"

	^ true.!

isText
	"answer whether we think this file contains textual data.
	NB: this is *not* reliable!!"

	^ self internalAttributes allMask: ZF_CONTENT_IS_TEXT_MASK.!

isText: aBool
	"set whether we think this file contains textual data"

	self internalAttributes: (internalAttributes mask: ZF_CONTENT_IS_TEXT_MASK set: aBool).!

msdosFileAttributes
	"answer the bitmap of MSDOS file attributes, or nil if we are not a FAT entry"

	^ versionMadeByHigh = ZF_VERSION_FAT_FS ifTrue: [self externalAttributes].
!

msdosFileAttributes: anInteger
	"set the bitmap of MSDOS file attributes"

	| new |

	"only applies if we think we're FAT"
	versionMadeByHigh = ZF_VERSION_FAT_FS ifFalse: [^ self].

	"we don't allow changes to the 'is-directory' bit 'cos that's taken from the filename"
	new := anInteger mask: ZF_MSDOS_DIRECTORY_MASK set: self isDirectory.

	self externalAttributes: new.

!

offsetOfLocalHeader
	"answer the receiver's offsetOfLocalHeader"

	^ offsetOfLocalHeader.
!

offsetOfLocalHeader: anInteger
	"set the receiver's offsetOfLocalHeader to anInteger"

	offsetOfLocalHeader := anInteger.
!

populateFrom: aBinaryReadStream
	"private -- populate our instance data by reading from the given stream"

	| filenameLength extraFieldLength commentLength |

	versionMadeByLow := aBinaryReadStream next.
	versionMadeByHigh := aBinaryReadStream next.
	versionToExtractLow := aBinaryReadStream next.
	versionToExtractHigh := aBinaryReadStream next.
	flags := aBinaryReadStream nextWORD maskClear: ZF_INTERNAL_USE_MASK_ALL.
	compressionMethod := aBinaryReadStream nextWORD.
	lastModifiedTime := self class dosTimeFromWORD: aBinaryReadStream nextWORD.
	lastModifiedDate := self class dosDateFromWORD: aBinaryReadStream nextWORD.
	crc := aBinaryReadStream nextDWORD.
	compressedSize := aBinaryReadStream nextDWORD.
	uncompressedSize := aBinaryReadStream nextDWORD.
	filenameLength := aBinaryReadStream nextWORD.
	extraFieldLength := aBinaryReadStream nextWORD.
	commentLength := aBinaryReadStream nextWORD.
	diskNumberStart := aBinaryReadStream nextWORD.
	internalAttributes := aBinaryReadStream nextWORD.
	externalAttributes := aBinaryReadStream nextDWORD.
	offsetOfLocalHeader := aBinaryReadStream nextDWORD.

	filename := filenameLength > 0 ifTrue: [(aBinaryReadStream next: filenameLength) asString].
	extraField:= extraFieldLength > 0 ifTrue: [aBinaryReadStream next: extraFieldLength].
	comment := commentLength > 0 ifTrue: [(aBinaryReadStream next: commentLength) asString].

	self updateCompressionLevel.!

readFileEntry
	"private -- read and remember the ZipFileEntry corresponding to this
	entry in the central directory"

	| signature entry |

	self positionStreamAt: offsetOfLocalHeader.
	signature := stream next: 4.
	signature = ZipFileEntry signature ifFalse: [self fileFormatError: 'Cannot read file entry data'].
	entry := ZipFileEntry from: stream.

	"remember the connection (we don't use #bePairedWith: because that
	would copy data from one to the other, and we want to be able to do
	consistancy checking)"
	entry pairedEntry: self.
	self pairedEntry: entry.

	"check that the data is consistant as far as the new entry can tell"
	filename = entry filename ifFalse: [self fileFormatError: 'Inconstant file name data'].
	entry knowsContentsSizes ifTrue:
		[uncompressedSize = entry uncompressedSize ifFalse: [self fileFormatError: 'Inconstant file size data'].
		compressedSize = entry compressedSize ifFalse: [self fileFormatError: 'Inconstant file size data'].
		crc = entry crc ifFalse: [self fileFormatError: 'Inconstant checksum data']].
	#CUtodo.  "check other data too"

	^ entry.
!

updateExternalAttributesFromFilename
	"private -- set our 'external attributes' to indicate whether we are a directory"

	"msdosFileAttributes: sets the 'dir' bit automatically"
	self msdosFileAttributes: self msdosFileAttributes.
!

variableRecordSize
	"answer the size of the variable part of the record in a zip file"

	^ (filename ifNil: [0] ifNotNil: [:it | it size])
	+ (extraField ifNil: [0] ifNotNil: [:it | it size])
	+ (comment ifNil: [0] ifNotNil: [:it | it size]).
!

versionMadeByHigh
	"answer the receiver's version-made-by high byte (one of the ZF_VERSION_* values)"

	^ versionMadeByHigh.
!

versionMadeByHigh: aByte
	"set the receiver's version-made-by high byte (one of the ZF_VERSION_* values)"

	versionMadeByHigh := aByte.
!

versionMadeByLow
	"answer the receiver's version made by low byte"

	^ versionMadeByLow.
!

versionMadeByLow: aByte
	"set the receiver's version made by low byte"

	versionMadeByLow := aByte.
!

versionMadeByMajor
	"answer the major number from the version-made-by field"

	^ versionMadeByLow // 10.
!

versionMadeByMajor: anInteger1 minor: anInteger2
	"set the receiver's version-made-by to the combination of the given major and minor version numbers"

	self versionMadeByLow: (anInteger1 * 10 + anInteger2).!

versionMadeByMinor
	"answer the minor number from the version-made-by field"

	^ versionMadeByLow \\ 10.
!

withReaderDo: a1Block
	"see ZipFileEntry>>withBinaryReaderDo: or withTextReaderDo:.
	The read stream will be binary- text- mode according as we are #isText or #isBinary.
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	^ self withReaderDo: a1Block binary: self isBinary.!

withReaderDo: a1Block binary: aBool
	"evaluate the <monadicValuable> a1Block passing a ReadStream that can be used to read the
	uncompressed contents of this entry.  If we do not have contents (we represent a directory) then
	a1Block is not evaluated.  If we have contents, but the contents are zero-length (an empty file) then the
	ReadStream will already be #atEnd.  If the contents are compressed then the ReadStream will be an
	InflaterReadStream, otherwise it'll be a ReadStream on a ByteArray of our contents.
	If aBool is true then the reader will be in binary mode, otherwise it'll be in text-mode.
	NB1: the the InflaterReadStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to read more data.
	NB2: if the checksums and sizes of the contents don't match what we think they should be, then
	this will throw a ZipError *after* executing the Block (we can't check before 'cos we don't know
	what the contents will decompress to until we've decompressed them...)"

	^ self fileEntry withReaderDo: a1Block binary: aBool.
!

withWriterDo: a1Block
	"abbreviation for the following...
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	self withWriterDo: a1Block backPatchSizes: true binary: self isBinary.
!

withWriterDo: a1Block backPatchSizes: aBool
	"abbreviation for the following...
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	self withWriterDo: a1Block backPatchSizes: aBool binary: self isBinary.
!

withWriterDo: a1Block backPatchSizes: aBool binary: anotherBool
	"write a file entry on our stream, then evaluate the <monadicValuable> a1Block passing a WriteStream that
	can be used to write the uncompressed contents of this entry (not called if we don't have data), and
	finally clean up by ensuring that the sizes and checksums on disk are correct.  It does NOT write a central
	directory entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	The write stream will expect character data or binary data according to anotherBool.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool.
	NB1: If the contents are to be compressed then the ReadStream will be an DeflaterWriteStream, otherwise
	it'll be a WriteStream onto a ByteArray (which means that uncompressed entries are assembled entirely
	in memory before being written out -- I intend to fix that someday).
	NB2: the the DeflaterWriteStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to write more data"

	self fileEntry withWriterDo: a1Block backPatchSizes: aBool binary: anotherBool.
!

write: aStringOrByteArray
	"abbreviation for the following...
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	self write: aStringOrByteArray backPatchSizes: true binary: self isBinary.
!

write: aStringOrByteArray backPatchSizes: aBool
	"abbreviation for the following...
	NB: not defined against the superclass since raw ZipFileEntries don't know
	whether they are binary or text"

	self write: aStringOrByteArray backPatchSizes: aBool binary: self isText.!

write: aStringOrByteArray backPatchSizes: aBool binary: anotherBool
	"write a file entry followed by the given contents on our stream, and finally clean up by ensuring
	that the sizes and checksums on disk are correct.  It does NOT write a central directory
	entry (even if the reciever *is* a central directory entry) since they cannot be written to the stream
	until after all the file entries.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool"

	self fileEntry write: aStringOrByteArray backPatchSizes: aBool binary: anotherBool.
!

writeDataTo: aBinaryWriteStream
	"private -- write our data to the given stream.
	NB: this is not symetrical with #populateFrom: in that it starts
	by writing the appropriate signature"

	| filenameLength extraFieldLength commentLength |

	filenameLength := filename ifNil: [0] ifNotNil: [:it | it size].
	extraFieldLength := extraField ifNil: [0] ifNotNil: [:it | it size].
	commentLength := comment ifNil: [0] ifNotNil: [:it | it size].

	aBinaryWriteStream
		nextPutAll: self class signature;
		nextPut: versionMadeByLow;
		nextPut: versionMadeByHigh;
		nextPut: versionToExtractLow;
		nextPut: versionToExtractHigh;
		nextWORDPut: (flags maskClear: ZF_INTERNAL_USE_MASK_ALL);
		nextWORDPut: compressionMethod;
		nextWORDPut: (self class timeAsDosWORD: lastModifiedTime);
		nextWORDPut: (self class dateAsDosWORD: lastModifiedDate);
		nextDWORDPut: crc;
		nextDWORDPut: compressedSize;
		nextDWORDPut: uncompressedSize;
		nextWORDPut: filenameLength;
		nextWORDPut: extraFieldLength;
		nextWORDPut: commentLength;
		nextWORDPut: diskNumberStart;
		nextWORDPut: internalAttributes;
		nextDWORDPut: externalAttributes;
		nextDWORDPut: offsetOfLocalHeader.

	filename ifNotNil: [:it | aBinaryWriteStream nextPutAll: it asByteArray].
	extraField ifNotNil: [:it | aBinaryWriteStream nextPutAll: it].
	comment ifNotNil: [:it | aBinaryWriteStream nextPutAll: it asByteArray].
! !
!ZipFileCentralDirectoryEntry categoriesFor: #beBinary!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #beBinary:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #beText!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #beText:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #centralDirectoryEntry!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #comment!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #comment:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #contents!accessing!public!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #contentsAsBinary:!accessing!public!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #copyDataFromPairedEntry!helpers!private! !
!ZipFileCentralDirectoryEntry categoriesFor: #copyToZipFile:!copying!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #defaultVersionMadeByHigh!constants!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #defaultVersionMadeByLow!constants!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #diskNumberStart!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #diskNumberStart:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #externalAttributes!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #externalAttributes:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #fileEntry!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #filename:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #initialize!initializing!private! !
!ZipFileCentralDirectoryEntry categoriesFor: #internalAttributes!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #internalAttributes:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #isBinary!public!testing! !
!ZipFileCentralDirectoryEntry categoriesFor: #isCentralDirectoryElement!public!testing! !
!ZipFileCentralDirectoryEntry categoriesFor: #isText!public!testing! !
!ZipFileCentralDirectoryEntry categoriesFor: #isText:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #msdosFileAttributes!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #msdosFileAttributes:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #offsetOfLocalHeader!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #offsetOfLocalHeader:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #populateFrom:!private!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #readFileEntry!private!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #updateExternalAttributesFromFilename!helpers!private! !
!ZipFileCentralDirectoryEntry categoriesFor: #variableRecordSize!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByHigh!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByHigh:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByLow!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByLow:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByMajor!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByMajor:minor:!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #versionMadeByMinor!accessing!public! !
!ZipFileCentralDirectoryEntry categoriesFor: #withReaderDo:!public!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #withReaderDo:binary:!public!reading! !
!ZipFileCentralDirectoryEntry categoriesFor: #withWriterDo:!helpers!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #withWriterDo:backPatchSizes:!helpers!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #withWriterDo:backPatchSizes:binary:!helpers!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #write:!helpers!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #write:backPatchSizes:!helpers!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #write:backPatchSizes:binary:!public!writing! !
!ZipFileCentralDirectoryEntry categoriesFor: #writeDataTo:!private!writing! !

!ZipFileCentralDirectoryEntry class methodsFor!

fixedRecordSizeLessSignature
	"answer the size of the fixed part of our record, excluding the size of the
	signature"

	^ 42.!

for: aZipFileEntry
	"answer a new instance that is linked to (stands for) the given
	existing ZipFileEntry (which may not yet be complete, itself)"

	^ (self new)
		stream: aZipFileEntry stream;
		bePairedWith: aZipFileEntry;
		yourself.!

maxVariableRecordSize
	"answer the maximum size of the variable part of our record"

	"NB: the ZipFile spec states that the combination of the variable
	fields should not exceed 2**16 -- I don't trust that"

	^ (2 ** 16) * 3.!

signature
	"answer the magic number that these elements use in ZIP files"

	"#(16r02 16r01 16r4B 16r50) asByteArray reverse"
	^ #[80 75 1 2].! !
!ZipFileCentralDirectoryEntry class categoriesFor: #fixedRecordSizeLessSignature!constants!public! !
!ZipFileCentralDirectoryEntry class categoriesFor: #for:!instance creation!public! !
!ZipFileCentralDirectoryEntry class categoriesFor: #maxVariableRecordSize!constants!public! !
!ZipFileCentralDirectoryEntry class categoriesFor: #signature!constants!public! !

ZipFileEntry guid: (GUID fromString: '{576C11D6-9AFF-43BD-81F1-1548880DBE2F}')!
ZipFileEntry comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Each of these represents the actual file data in the body of the Zip file.

The file sizes and checksum may or may not be valid depending on how the Zip file was created, and on whether we have yet read in the contents.  If #knowsContentsSizes is false, then we have not yet read (or otherwise defined) the relevant data.

All the data we hold, except the file contents themselves, are duplicated in the ZipFIleCentralDirectoryEntry with which we are paired.  Also that has some extra data (such as a comment) that we don''t hold.

You can ask for our contents, in which case we will read the contents from "disk" and decompress them if necesasry, or you can ask for a ReadStream on our contents.  See the methods in category ''reading''.

Similalry, if you are creating a Zip file, then you can ask the *current* entry for a WriteStream onto which you can write the intended contents, or you can just supply a String or ByteArray with the entire intended contents.  See category ''writing'' for the relevant API.   Since writing a zip file is necessarily a sequential operation, you have to proceed by creating a new entry, and writing its contents completely *before* you add the next entry.  The ZipFile class will look after most of the details of this for you unless you are using these objects "raw".'!
!ZipFileEntry categoriesForClass!Unclassified! !
!ZipFileEntry methodsFor!

basicWithWriterDo: a1Block backPatchSizes: aBool binary: anotherBool
	"private -- evaluate the <monadicValuable> a1Block passing a WriteStream that can
	be used to write the compressed contents of this entry.
	If anotherBool is true then the stream will expext to be fed binary data, ortherwise
	it will be set up to expect text.
	Note that we assume that our underlying stream is already correctly
	positioned (as it is if we've just done a #writeData)"

	| writer |

	writer := self openWriterStream: anotherBool.
	[a1Block value: writer] ensure: [self closeWriterStream: writer].
	self correctContentsSizes: aBool.
!

centralDirectoryEntry
	"answer the ZipFileCentralDirectoryEntry for this file.
	This will be nil if the central directory has not yet been read *or* if the central directory
	entry has not yet been bound to us (i.e. this method isn't a lot of use...)"

	^ self pairedEntry.!

closeReaderStream: aReadStream
	"private -- close the InflaterReadStream that should have been used to decompress
	our contents, throwing errors if the stream didn't produce the correct sizes and checksums"

	| wasAtEnd |

	wasAtEnd := aReadStream atEnd.

	"we *must* ensure that the inflater stream is closed"
	aReadStream close.

	"if the consumer hasn't read up to then end, then we'll not bother
	checking the checksums, etc, since that would only slow things
	down unecessarily" 
	wasAtEnd ifFalse: [^ self].

	"make sure that we hasn't overrun the compressed input (a nullop
	if the reader has been #unBufferInput-ed)"
	aReadStream pushBackUnconsumedInput.

	"if we are a contents-delimited entry, then we'd better get the trailer that states how
	big the contents *should* have been, etc"
	self knowsContentsSizes ifFalse: [self readDataDescriptor].

	"now we can do our sanity check"
	uncompressedSize = aReadStream uncompressedSize ifFalse: [self fileFormatError: 'Sizes don''t match'].
	compressedSize = aReadStream compressedSize ifFalse: [self fileFormatError: 'Compressed sizes don''t match'].
	crc = aReadStream crcChecksum ifFalse: [self fileFormatError: 'Checksums don''t match'].
!

closeWriterStream: aWriteStream
	"private -- close the WriteStream that has been used to encode our contents,
	ensuring that we remember the sizes and checksums"

	"we *must* ensure that the deflater stream is closed"
	aWriteStream close.

	"remember the sizes, etc"
	self
		uncompressedSize: aWriteStream uncompressedSize;
		compressedSize: aWriteStream compressedSize;
		crc: aWriteStream crcChecksum;
		knowsContentsSizes: true.
!

contentsAsBinary: aBool
	"answer our uncompressed contents or nil if we don't have any.
	The contents will be a ByteArray or a String according as aBool is true.
	Throws errors if the contents' checksum doesn't match"

	| contents contentsCrc |

	self hasContents ifFalse: [^ nil].

	"if we are compressed then use the decompression stream"
	self isCompressed ifTrue: [self withReaderDo: [:str | ^ str upToEnd] binary: aBool].

	"we don't do decryption"
	self isEncrypted ifTrue: [self fileFormatError: 'Decryption not supported'].

	"otherwise we can do it directly"
	self positionStreamAt: rawContentsPosition.
	contents := stream next: compressedSize.

	"note that we don't bother with #readDataDescriptor since uncompressed entries
	must never have one (they are not self-delimiting)"

	"however, we do still have to do the crc check (no point in checking sizes!!)"
	contentsCrc := ZLib1Library default crc32: contents.
	crc = contentsCrc ifFalse: [self fileFormatError: 'Checksums don''t match'].

	^ aBool
		ifTrue: [contents]
		ifFalse: [contents asString].
!

correctContentsSizes: aBool
	"private -- ensure that our target file's record of the contents sizes is correct.  If aBool
	is true then it does it by backpatching the original record, otherwise it appends a 'Data
	Descriptor' after the contents in the file"

	aBool
		ifTrue: [self writeData "lazy, but good enough"]
		ifFalse: [self writeDataDescriptor].!

fileEntry
	"answer the ZipFileEntry for this file"

	"we are that entry"
	^ self.!

hasBeenWritten
	"answer whether this entry has yet been written to disk"

	^ (flags allMask: ZF_INTERNAL_USE_MASK_1).
!

hasBeenWritten: aBool
	"private -- set whether this entry has yet been written to disk"

	flags := flags mask: ZF_INTERNAL_USE_MASK_1 set: aBool.
!

openReaderStream: aBool
	"private -- answer a binary or text ReadStream that will uncompress our contents"

	"the only compression format we understand is deflater"
	self isDeflaterCompressed ifFalse: [self fileFormatError: 'Compression format not supported'].

	"and we don't do decryption"
	self isEncrypted ifTrue: [self fileFormatError: 'Decryption not supported'].

	"start at the begining of our compressed contents"
	self positionStreamAt: rawContentsPosition.

	"and use an inflater stream in #Raw format"
	^ (InflaterReadStream forBinaryOn: stream)
		beBinary: aBool;
		beRaw;
		yourself.!

openWriterStream: aBool
	"private -- answer a WriteStream that will compress our contents.
	If aBool is true then answer a binary write stream, otherwise one that expects to
	be fed character data.
	Note that we assume that our underlying stream is already correctly
	positioned (as it is if we've just done a #writeData)"

	"the only compression format we understand is deflater in #Raw format"
	self isDeflaterCompressed ifFalse: [self fileFormatError: 'Compression format not supported'].

	^  (DeflaterWriteStream forBinaryOn: stream compressionLevel: compressionLevel)
		beBinary: aBool;
		beRaw;
		yourself.
!

optimiseCompressionFor: aByteArray
	"work out (by exhaustive testing) what compression type and level will result in the most
	compression of the given byte array, and set out compression levels appropriatedly"

	| bestLevel bestSize |

	"try the different compression levels"
	Z_BEST_SPEED to: Z_BEST_COMPRESSION do:
		[:level || deflater |
		deflater := (DeflaterWriteStream on: NullWriteStream new)
				beRaw;
				beBinary;	"makes no difference really"
				nextPutAll: aByteArray;
				close.
		(bestLevel isNil or: [deflater compressedSize < bestSize]) ifTrue:
			[bestLevel := level.
			bestSize := deflater compressedSize]].

	"and remember which was best"
	self
		useCompression: (bestSize < aByteArray size);
		compressionLevel: bestLevel.!

populateFrom: aBinaryReadStream
	"private -- populate our instance data by reading from the given stream"

	| filenameLength extraFieldLength |

	versionToExtractLow := aBinaryReadStream next.
	versionToExtractHigh := aBinaryReadStream next.
	flags := aBinaryReadStream nextWORD maskClear: ZF_INTERNAL_USE_MASK_ALL.
	compressionMethod := aBinaryReadStream nextWORD.
	lastModifiedTime := self class dosTimeFromWORD: aBinaryReadStream nextWORD.
	lastModifiedDate := self class dosDateFromWORD: aBinaryReadStream nextWORD.
	crc := aBinaryReadStream nextDWORD.
	compressedSize := aBinaryReadStream nextDWORD.
	uncompressedSize := aBinaryReadStream nextDWORD.
	filenameLength := aBinaryReadStream nextWORD.
	extraFieldLength := aBinaryReadStream nextWORD.

	filename := filenameLength > 0 ifTrue: [(aBinaryReadStream next: filenameLength) asString].
	extraField:= extraFieldLength > 0 ifTrue: [aBinaryReadStream next: extraFieldLength].

	rawContentsPosition := aBinaryReadStream position.

	self updateCompressionLevel.
!

positionOfNextInStream
	"answer the position in the stream where the next record after this
	should start"

	self assert: [self knowsContentsSizes].

	"overriden to take account of the contents"
	^ super positionOfNextInStream + compressedSize.!

readDataDescriptor
	"private -- read the DataDescriptor that follows this entry, so that we can
	check for consistancy.
	NB: assumes we are positioned immediately before that data (i.e. we have
	just read our own contents)"

	| signature descriptor |

	self assert: [self decompressionIsSelfDelimiting].

	signature := stream next: 4.
	signature = ZipFileDataDescriptor signature ifFalse: [self fileFormatError: 'Cannot read check data after compressed data'].
	descriptor := ZipFileDataDescriptor from: stream.

	"record the data from the descriptor"
	self knowsContentsSizes: true.
	uncompressedSize := descriptor uncompressedSize.
	compressedSize := descriptor compressedSize.
	crc := descriptor crc.!

variableRecordSize
	"answer the size of the variable part of the record in a zip file"

	^ (filename ifNil: [0] ifNotNil: [:it | it size])
	+ (extraField ifNil: [0] ifNotNil: [:it | it size]).
!

withReaderDo: a1Block binary: aBool
	"evaluate the <monadicValuable> a1Block passing a ReadStream that can be used to read the
	uncompressed contents of this entry.  If we do not have contents (we represent a directory) then
	a1Block is not evaluated.  If we have contents, but the contents are zero-length (an empty file) then the
	ReadStream will already be #atEnd.  If the contents are compressed then the ReadStream will be an
	InflaterReadStream, otherwise it'll be a ReadStream on a ByteArray of our contents.
	If aBool is true then the reader will be in binary mode, otherwise it'll be in text-mode.
	NB1: the the InflaterReadStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to read more data.
	NB2: if the checksums and sizes of the contents don't match what we think they should be, then
	this will throw a ZipError *after* executing the Block (we can't check before 'cos we don't know
	what the contents will decompress to until we've decompressed them...)"

	| reader |

	"if we don't have contents then fast-out"
	self hasContents ifFalse: [^ nil].

	"if we're not compressed then read our contents into a ByteArray.
	That's not ideal, it'd be better if we had a special ReadStream that
	could provide a limited 'view' of the underlying stream and also keep
	track of the checksums"
	#CUtodo.  "remove this hack"
	self isCompressed ifFalse:
		[| contents |
		contents := self contentsAsBinary: aBool.
		^ a1Block value: contents  readStream].

	reader := self openReaderStream: aBool.
	^ [a1Block value: reader] ensure: [self closeReaderStream: reader].!

withWriterDo: a1Block backPatchSizes: aBool binary: anotherBool
	"write our basic data, then evaluate the <monadicValuable> a1Block passing a WriteStream that
	can be used to write the uncompressed contents of this entry (not called if we don't have data), and
	finally clean up by ensuring that the sizes and checksums on disk are correct.
	The write stream will expect character data or binary data according to anotherBool.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool.
	NB1: If the contents are to be compressed then the ReadStream will be an DeflaterWriteStream, otherwise
	it'll be a WriteStream onto a ByteArray (which means that uncompressed entries are assembled entirely
	in memory before being written out -- I intend to fix that someday).
	NB2: the the DeflaterWriteStream will be closed automatically after this use.  You can keep a reference to it
	if you must, but it can't be used to write more data"

	"fast path out if we have no data"
	self hasContents ifFalse: [^ self knowsContentsSizes: true; writeData].

	"if we're not compressed then just write into a ByteArray or String.
	That's not ideal, it'd be better if we had a special WriteStream that
	could provide a 'view' of the underlying stream and keep track of
	the sizes and checksums and which understod more of the DeflaterWriteStream
	protocol"
	#CUtodo.  "remove this hack"
	self isCompressed ifFalse:
		[| writer |
		writer := anotherBool
				ifTrue: [ByteArray writeStream]
				ifFalse: [String writeStream].
		a1Block value: writer.
		^ self write: writer contents backPatchSizes: aBool binary: anotherBool].

	"write the basic data, the contents, and then correct the sizes"
	self
		knowsContentsSizes: false;
		writeData;
		basicWithWriterDo: a1Block backPatchSizes: aBool binary: anotherBool.
!

write: aStringOrByteArray backPatchSizes: aBool binary: anotherBool
	"write our basic data, followed by the given contents on our stream, and finally clean up
	by ensuring that the sizes and checksums on disk are correct.
	There are two ways to correct the on disk sized, one is to seek backwards in the file and patch the
	orginal header, the other is append a supplementary 'data descriptor' with the sizes *after* the contents
	data. Which we do (if either is neccesary) is determined by aBool"

	(self hasContents or: [aStringOrByteArray isEmpty]) ifFalse: [self fileFormatError: 'Entry should not have contents'].

	"ensure that we don't try to use compression if there's no data"
	aStringOrByteArray isEmpty ifTrue: [self useCompression: false].

	"if possible, get the sizes, etc, so that we don't have to mess around backpatching"
	self isCompressed
		ifTrue: [self knowsContentsSizes: false]
		ifFalse: [self
				uncompressedSize: aStringOrByteArray size;
				compressedSize: aStringOrByteArray size;
				crc: (ZLib1Library default crc32: aStringOrByteArray);
				knowsContentsSizes: true].

	"write out the basic data"
	self writeData.

	"if we are compressed then use the compression stream otherwise just write it direct.
	(this is a null-op if the byte array is empty and we are uncompressed)"
	self isCompressed
		ifTrue: [self basicWithWriterDo: [:str | str nextPutAll: aStringOrByteArray] backPatchSizes: aBool binary: anotherBool]
		ifFalse: [stream nextPutAll: aStringOrByteArray asByteArray].
!

writeDataDescriptor
	"private -- write a DataDescriptor following this entry.
	NB: assumes we are positioned immediately before that data (i.e. we have
	just written our contents out)"

	| descriptor |

	self assert: [self decompressionIsSelfDelimiting].

	descriptor := ZipFileDataDescriptor for: self.
	descriptor writeData.!

writeDataTo: aBinaryWriteStream
	"private -- write our data to the given stream.
	NB: this is not symetrical with #populateFrom: in that it starts
	by writing the appropriate signature"

	| filenameLength extraFieldLength |

	self hasBeenWritten: true.

	filenameLength := filename ifNil: [0] ifNotNil: [:it | it size].
	extraFieldLength := extraField ifNil: [0] ifNotNil: [:it | it size].

	aBinaryWriteStream
		nextPutAll: self class signature;
		nextPut: versionToExtractLow;
		nextPut: versionToExtractHigh;
		nextWORDPut: (flags maskClear: ZF_INTERNAL_USE_MASK_ALL);
		nextWORDPut: compressionMethod;
		nextWORDPut: (self class timeAsDosWORD: lastModifiedTime);
		nextWORDPut: (self class dateAsDosWORD: lastModifiedDate);
		nextDWORDPut: crc;
		nextDWORDPut: compressedSize;
		nextDWORDPut: uncompressedSize;
		nextWORDPut: filenameLength;
		nextWORDPut: extraFieldLength.

	filename ifNotNil: [:it | aBinaryWriteStream nextPutAll: it asByteArray].
	extraField ifNotNil: [:it | aBinaryWriteStream nextPutAll: it].

	rawContentsPosition := aBinaryWriteStream position.
! !
!ZipFileEntry categoriesFor: #basicWithWriterDo:backPatchSizes:binary:!private!writing! !
!ZipFileEntry categoriesFor: #centralDirectoryEntry!accessing!public! !
!ZipFileEntry categoriesFor: #closeReaderStream:!private!reading! !
!ZipFileEntry categoriesFor: #closeWriterStream:!private!writing! !
!ZipFileEntry categoriesFor: #contentsAsBinary:!accessing!public!reading! !
!ZipFileEntry categoriesFor: #correctContentsSizes:!private!writing! !
!ZipFileEntry categoriesFor: #fileEntry!accessing!public! !
!ZipFileEntry categoriesFor: #hasBeenWritten!public!testing!writing! !
!ZipFileEntry categoriesFor: #hasBeenWritten:!accessing!private!writing! !
!ZipFileEntry categoriesFor: #openReaderStream:!private!reading! !
!ZipFileEntry categoriesFor: #openWriterStream:!private!writing! !
!ZipFileEntry categoriesFor: #optimiseCompressionFor:!helpers!public! !
!ZipFileEntry categoriesFor: #populateFrom:!private!reading! !
!ZipFileEntry categoriesFor: #positionOfNextInStream!accessing!public! !
!ZipFileEntry categoriesFor: #readDataDescriptor!private!reading! !
!ZipFileEntry categoriesFor: #variableRecordSize!accessing!public! !
!ZipFileEntry categoriesFor: #withReaderDo:binary:!public!reading! !
!ZipFileEntry categoriesFor: #withWriterDo:backPatchSizes:binary:!helpers!public!writing! !
!ZipFileEntry categoriesFor: #write:backPatchSizes:binary:!public!writing! !
!ZipFileEntry categoriesFor: #writeDataDescriptor!private!writing! !
!ZipFileEntry categoriesFor: #writeDataTo:!private!writing! !

!ZipFileEntry class methodsFor!

fixedRecordSizeLessSignature
	"answer the size of the fixed part of our record, excluding the size of the
	signature"

	^ 26.!

maxVariableRecordSize
	"answer the maximum size of the variable part of our record"

	"NB1: the ZipFile spec states that the combination of the variable
	fields should not exceed 2**16 -- I don't trust that.
	NB2: this doesn't include the 'contents'"
	^ (2 ** 16) * 2.!

signature
	"answer the magic number that these elements use in ZIP files"

	"#(16r04 16r03 16r4B 16r50) asByteArray reverse"
	^  #[80 75 3 4].! !
!ZipFileEntry class categoriesFor: #fixedRecordSizeLessSignature!constants!public! !
!ZipFileEntry class categoriesFor: #maxVariableRecordSize!constants!public! !
!ZipFileEntry class categoriesFor: #signature!constants!public! !

"Binary Globals"!

"Resources"!

