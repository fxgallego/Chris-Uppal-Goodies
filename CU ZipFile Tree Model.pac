| package |
package := Package name: 'CU ZipFile Tree Model'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Simple utility classes to help with displaying the contents of a Zip file in a TreePresenter or similar.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #FakeZipfileEntry;
	add: #ZipFileTreeModel;
	yourself.

package methodNames
	add: #ZipFileEntryBase -> #icon;
	add: #ZipFileEntryBase -> #isFake;
	add: 'ZipFileEntryBase class' -> #fileIcon;
	add: 'ZipFileEntryBase class' -> #folderIcon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ZipFiles';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #FakeZipfileEntry
	instanceVariableNames: 'filename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeModel subclass: #ZipFileTreeModel
	instanceVariableNames: 'zipFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ZipFileEntryBase methodsFor!

icon
	"answer an icon to use for this entry"

	^ self isDirectory
		ifTrue: [self class folderIcon]
		ifFalse: [self class fileIcon].!

isFake
	"can be used to distinguish real ZipFileEntries from the 'fakes' used by ZipFileTreeModel"

	^ false.! !
!ZipFileEntryBase categoriesFor: #icon!accessing!public! !
!ZipFileEntryBase categoriesFor: #isFake!public!testing! !

!ZipFileEntryBase class methodsFor!

fileIcon
	"answer an icon to use for instances that seem to be files"

	^ Icon fromId: 2 in: ShellLibrary default.
!

folderIcon
	"answer an icon to use for instances that seem to be folders"

	^ Icon fromId: 4 in: ShellLibrary default.
! !
!ZipFileEntryBase class categoriesFor: #fileIcon!constants!public! !
!ZipFileEntryBase class categoriesFor: #folderIcon!constants!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FakeZipfileEntry guid: (GUID fromString: '{C7852726-1B9D-4637-89AA-6DC9E181E2AF}')!
FakeZipfileEntry comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Dummy used to stand for directories in ZipFileTreeModels when the Zip file itself has no entry for that directory.  (Which is the typical case)'!
!FakeZipfileEntry categoriesForClass!Unclassified! !
!FakeZipfileEntry methodsFor!

binaryContents
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

comment
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

compressedSize
	"fake to allow us to imitate a ZipFileEntry"

	^ 0.!

compressionLevel
	"fake to allow us to imitate a ZipFileEntry"

	^ 9.!

compressionMethod
	"fake to allow us to imitate a ZipFileEntry"

	^ 0.
!

contents
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

crc
	"fake to allow us to imitate a ZipFileEntry"

	^ 0.
!

displayOn: aStream
	"write a user-centric representation of ourselves to aStream"

	aStream display: self filenameOrStdin.!

externalAttributes
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

extraField
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.
!

filename
	"answer the name of the file we would stand for if we stood for anything"

	^ filename!

filename: aString
	"set the name of the file we would stand for if we stood for anything"

	filename := aString.!

filenameOrStdin
	"answer the receiver's filename or the string '<stdin>' if there is no filename"

	^ filename ifNil: ['<stdin'].
!

hasCompressedContents
	"fake to allow us to imitate a ZipFileEntry"

	^ false.!

hasContents
	"fake to allow us to imitate a ZipFileEntry"

	^ false.!

icon
	"answer an icon to use for this entry"

	^ self isDirectory
		ifTrue: [self class folderIcon]
		ifFalse: [self class fileIcon].!

internalAttributes
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

isCompressed
	"fake to allow us to imitate a ZipFileEntry"

	^ false.!

isDeflaterCompressed
	"fake to allow us to imitate a ZipFileEntry"

	^ false.
!

isDirectory
	"answer true if this looks like the entry for a directory"

	^ filename
		ifNil: [false]
		ifNotNil: [:it | it endsWith: '/'].!

isEncrypted
	"fake to allow us to imitate a ZipFileEntry"

	^ false.
!

isFake

	^ true.!

isNotCompressed
	"fake to allow us to imitate a ZipFileEntry"

	^ true.!

isText
	"fake to allow us to imitate a ZipFileEntry"

	^ false.
!

knowsContentsSizes
	"fake to allow us to imitate a ZipFileEntry"

	^ true.
!

lastModifiedDate
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.
!

lastModifiedTime
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.
!

printOn: aStream
	"write a developer-centric representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' filename: ';
		display: self.!

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
	"fake to allow us to imitate a ZipFileEntry"

	^ nil.!

uncompressedSize
	"fake to allow us to imitate a ZipFileEntry"

	^ 0.!

usesCompression
	"fake to allow us to imitate a ZipFileEntry"

	^ false.
! !
!FakeZipfileEntry categoriesFor: #binaryContents!accessing!public! !
!FakeZipfileEntry categoriesFor: #comment!accessing!public! !
!FakeZipfileEntry categoriesFor: #compressedSize!accessing!public! !
!FakeZipfileEntry categoriesFor: #compressionLevel!accessing!public! !
!FakeZipfileEntry categoriesFor: #compressionMethod!accessing!public! !
!FakeZipfileEntry categoriesFor: #contents!accessing!public! !
!FakeZipfileEntry categoriesFor: #crc!accessing!public! !
!FakeZipfileEntry categoriesFor: #displayOn:!displaying!public! !
!FakeZipfileEntry categoriesFor: #externalAttributes!accessing!public! !
!FakeZipfileEntry categoriesFor: #extraField!accessing!public! !
!FakeZipfileEntry categoriesFor: #filename!accessing!public! !
!FakeZipfileEntry categoriesFor: #filename:!accessing!public! !
!FakeZipfileEntry categoriesFor: #filenameOrStdin!accessing!public! !
!FakeZipfileEntry categoriesFor: #hasCompressedContents!public!testing! !
!FakeZipfileEntry categoriesFor: #hasContents!public!testing! !
!FakeZipfileEntry categoriesFor: #icon!accessing!public! !
!FakeZipfileEntry categoriesFor: #internalAttributes!accessing!public! !
!FakeZipfileEntry categoriesFor: #isCompressed!public!testing! !
!FakeZipfileEntry categoriesFor: #isDeflaterCompressed!public!testing! !
!FakeZipfileEntry categoriesFor: #isDirectory!public!testing! !
!FakeZipfileEntry categoriesFor: #isEncrypted!public!testing! !
!FakeZipfileEntry categoriesFor: #isFake!public!testing! !
!FakeZipfileEntry categoriesFor: #isNotCompressed!public!testing! !
!FakeZipfileEntry categoriesFor: #isText!public!testing! !
!FakeZipfileEntry categoriesFor: #knowsContentsSizes!public!testing! !
!FakeZipfileEntry categoriesFor: #lastModifiedDate!accessing!public! !
!FakeZipfileEntry categoriesFor: #lastModifiedTime!accessing!public! !
!FakeZipfileEntry categoriesFor: #printOn:!printing!public! !
!FakeZipfileEntry categoriesFor: #shortName!accessing!public! !
!FakeZipfileEntry categoriesFor: #shortNameOrStdin!accessing!public! !
!FakeZipfileEntry categoriesFor: #textContents!accessing!public! !
!FakeZipfileEntry categoriesFor: #uncompressedSize!accessing!public! !
!FakeZipfileEntry categoriesFor: #usesCompression!public!testing! !

!FakeZipfileEntry class methodsFor!

fileIcon
	^ ZipFileEntry fileIcon.!

filename: aString
	"answer a new instance with the given name"

	^ (self new)
		filename: aString;
		yourself.!

folderIcon
	^ ZipFileEntry folderIcon.! !
!FakeZipfileEntry class categoriesFor: #fileIcon!constants!public! !
!FakeZipfileEntry class categoriesFor: #filename:!instance creation!public! !
!FakeZipfileEntry class categoriesFor: #folderIcon!constants!public! !

ZipFileTreeModel guid: (GUID fromString: '{895D950D-3B0C-41C5-A150-0523B9A01629}')!
ZipFileTreeModel comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Simple utility class to help with displaying the contents of a Zip file in a TreePresenter or similar.

NB: currently this is a static view.  If I ever add change notification to ZipFile then I''ll use that to make this model update itself accordingly.

E.g:

	TreePresenter showOn:
		((ZipFileTreeModel for:
			(ZipFile readingFile:
				(FileOpenDialog showModal)))).

Or, if you have my ListTreeView, you can be more grandiose...

	model := ZipFileTreeModel for: (ZipFile readingFile: FileOpenDialog showModal).
	columns := #(#shortName #uncompressedSize #compressedSize #isText #filename) collect:
			[:each |
			(ListViewColumn new)
				text: each capitalized;
				getContentsBlock: (Message selector: each);
				yourself].
	(ListTreePresenter show: ''Multi-selection view'' on: model) view
		columnsList: columns;
		hasLinesAtRoot: true;
		hasFullRowSelect: false;
		getImageBlock: IconicListAbstract.'!
!ZipFileTreeModel categoriesForClass!Unclassified! !
!ZipFileTreeModel methodsFor!

add: aZipFileEntry using: aMap and: aSet
	"private -- add the given entry using, and if necessary updating, the supplied name->entry map"

	| name index parentName parent |

	name := aZipFileEntry filenameOrStdin.
	index := name prevIndexOf: $/ from: name size - 1 to: 1.
	parentName := index = 0 ifTrue: [''] ifFalse: [name first: index].
	parent := aMap
			at: parentName
			ifAbsentPut: [FakeZipfileEntry filename: parentName].

	(parent isNil or: [aSet includes: parent]) ifFalse: [self add: parent using: aMap and: aSet].

	self add: aZipFileEntry asChildOf: parent.
	aSet add: aZipFileEntry.
!

zipFile
	"answer the zip file of which we present a view"

	^ zipFile.!

zipFile: aZipFile
	"private -- set the ZipFile of which we present a view"

	| map done |

	zipFile := aZipFile.

	map := LookupTable new.
	map at: '' put: nil.
	zipFile do: [:each | map at: each filenameOrStdin put: each].

	done := IdentitySet new.
	zipFile do: [:each | self add: each using: map and: done].
! !
!ZipFileTreeModel categoriesFor: #add:using:and:!initializing!private! !
!ZipFileTreeModel categoriesFor: #zipFile!accessing!public! !
!ZipFileTreeModel categoriesFor: #zipFile:!initializing!private! !

!ZipFileTreeModel class methodsFor!

for: aZipFile
	"answer a new instance that presents a static
	view of the contents of aZipFIle"

	^ (self new)
		zipFile: aZipFile;
		yourself.! !
!ZipFileTreeModel class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

