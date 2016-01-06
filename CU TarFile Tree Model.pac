| package |
package := Package name: 'CU TarFile Tree Model'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Simple utility classes to help with displaying the contents of a TAR file in a TreePresenter or similar.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package classNames
	add: #FakeTarfileEntry;
	add: #TarFileTreeModel;
	yourself.

package methodNames
	add: #TarFileEntry -> #icon;
	add: #TarFileEntry -> #isFake;
	add: 'TarFileEntry class' -> #fileIcon;
	add: 'TarFileEntry class' -> #folderIcon;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU TarFiles';
	add: 'CU ZipFiles';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	yourself).

package!

"Class Definitions"!

Object subclass: #FakeTarfileEntry
	instanceVariableNames: 'filename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreeModel subclass: #TarFileTreeModel
	instanceVariableNames: 'tarFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!TarFileEntry methodsFor!

icon
	"answer an icon to use for this entry"

	^ self isDirectory
		ifTrue: [self class folderIcon]
		ifFalse: [self class fileIcon].!

isFake
	"can be used to distinguish real ZipFileEntries from the 'fakes' used by ZipFileTreeModel"

	^ false.! !
!TarFileEntry categoriesFor: #icon!accessing!public! !
!TarFileEntry categoriesFor: #isFake!public!testing! !

!TarFileEntry class methodsFor!

fileIcon
	"answer an icon to use for instances that seem to be files"

	^ Icon fromId: 2 in: ShellLibrary default.
!

folderIcon
	"answer an icon to use for instances that seem to be folders"

	^ Icon fromId: 4 in: ShellLibrary default.
! !
!TarFileEntry class categoriesFor: #fileIcon!constants!public! !
!TarFileEntry class categoriesFor: #folderIcon!constants!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FakeTarfileEntry guid: (GUID fromString: '{C9CB6710-E74A-4BB2-8870-A356A628275F}')!
FakeTarfileEntry comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Dummy used to stand for directories in TarFileTreeModels when the Tar file itself has no entry for that directory.'!
!FakeTarfileEntry categoriesForClass!Unclassified! !
!FakeTarfileEntry methodsFor!

binaryContents
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

checksum
	"fake to allow us to imitate a TarFileEntry"

	^ 0.!

contents
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

contentsSize
	"fake to allow us to imitate a TarFileEntry"

	^ 0.!

describeOn: aStream
	"append a user-centric description of ourself to aStream"

	#CUtodo. "it would probably be better to match the real tar file entries' formatting"
	^ self displayOn: filename.!

description
	"answer a user-centric description of ourself"

	| str |

	str := String writeStream: 64 + filename size.
	self describeOn: str.
	^ str contents.!

devmajor
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

devminor
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: filename.
!

filename
	"answer the name of the file we would stand for if we stood for anything"

	^ filename!

filename: aString
	"set the name of the file we would stand for if we stood for anything"

	filename := aString.!

filetime
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

gid
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

gname
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

gnameOrGidString
	"fake to allow us to imitate a TarFileEntry"

	^ ''.!

hasContents
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

icon
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

isArchaic
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isBlockSpecial
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isCharacterSpecial
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isContiguousFile
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isDevice
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isDirectory
	"answer true if this looks like the entry for a directory"

	^ filename
		ifNil: [false]
		ifNotNil: [:it | it endsWith: '/'].!

isEndMarker
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isFake
	"fake to allow us to imitate a TarFileEntry"

	^ true.!

isHardLink
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isLink
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isNamedPipe
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isRegularFile
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

isSoftLink
	"fake to allow us to imitate a TarFileEntry"

	^ false.!

linkname
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

maxFilenameSize
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

mode
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

modeString
	"fake to allow us to imitate a TarFileEntry"

	^ ''.!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' (';
		display: self;
		nextPutAll: ')'.!

shortName
	"answer the last segment of our filename or nil if we don't have one"

	| index |

	filename isNil ifTrue: [^ nil].

	"note that we leave the trailing '/' on the end of directory names"
	index := filename prevIndexOf: $/ from: filename size - 1 to: 1.
	^ index = 0
		ifTrue: [filename]
		ifFalse: [filename copyFrom: index + 1].!

sizeOrDevicesString
	"fake to allow us to imitate a TarFileEntry"

	^ ''.!

textContents
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

type
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

typeName
	"fake to allow us to imitate a TarFileEntry"

	^ #Fake.!

uid
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

uname
	"fake to allow us to imitate a TarFileEntry"

	^ nil.!

unameOrUidString
	"fake to allow us to imitate a TarFileEntry"

	^ ''.! !
!FakeTarfileEntry categoriesFor: #binaryContents!accessing!public!reading! !
!FakeTarfileEntry categoriesFor: #checksum!accessing!public! !
!FakeTarfileEntry categoriesFor: #contents!accessing!public!reading! !
!FakeTarfileEntry categoriesFor: #contentsSize!accessing!public! !
!FakeTarfileEntry categoriesFor: #describeOn:!displaying!public! !
!FakeTarfileEntry categoriesFor: #description!displaying!public! !
!FakeTarfileEntry categoriesFor: #devmajor!accessing!public! !
!FakeTarfileEntry categoriesFor: #devminor!accessing!public! !
!FakeTarfileEntry categoriesFor: #displayOn:!displaying!public! !
!FakeTarfileEntry categoriesFor: #filename!accessing!public! !
!FakeTarfileEntry categoriesFor: #filename:!accessing!public! !
!FakeTarfileEntry categoriesFor: #filetime!accessing!public! !
!FakeTarfileEntry categoriesFor: #gid!accessing!public! !
!FakeTarfileEntry categoriesFor: #gname!accessing!public! !
!FakeTarfileEntry categoriesFor: #gnameOrGidString!displaying!public! !
!FakeTarfileEntry categoriesFor: #hasContents!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #icon!accessing!public! !
!FakeTarfileEntry categoriesFor: #isArchaic!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isBlockSpecial!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isCharacterSpecial!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isContiguousFile!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isDevice!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isDirectory!public!testing! !
!FakeTarfileEntry categoriesFor: #isEndMarker!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isFake!public!testing! !
!FakeTarfileEntry categoriesFor: #isHardLink!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isLink!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isNamedPipe!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isRegularFile!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #isSoftLink!public!record types!testing! !
!FakeTarfileEntry categoriesFor: #linkname!accessing!public! !
!FakeTarfileEntry categoriesFor: #maxFilenameSize!accessing!public! !
!FakeTarfileEntry categoriesFor: #mode!accessing!public! !
!FakeTarfileEntry categoriesFor: #modeString!displaying!public! !
!FakeTarfileEntry categoriesFor: #printOn:!printing!public! !
!FakeTarfileEntry categoriesFor: #shortName!accessing!public! !
!FakeTarfileEntry categoriesFor: #sizeOrDevicesString!displaying!public! !
!FakeTarfileEntry categoriesFor: #textContents!accessing!public!reading! !
!FakeTarfileEntry categoriesFor: #type!accessing!public!record types! !
!FakeTarfileEntry categoriesFor: #typeName!accessing!public!record types! !
!FakeTarfileEntry categoriesFor: #uid!accessing!public! !
!FakeTarfileEntry categoriesFor: #uname!accessing!public! !
!FakeTarfileEntry categoriesFor: #unameOrUidString!displaying!public! !

!FakeTarfileEntry class methodsFor!

fileIcon
	^ ZipFileEntry fileIcon.!

filename: aString
	"answer a new instance with the given name"

	^ (self new)
		filename: aString;
		yourself.!

folderIcon
	^ ZipFileEntry folderIcon.! !
!FakeTarfileEntry class categoriesFor: #fileIcon!constants!public! !
!FakeTarfileEntry class categoriesFor: #filename:!instance creation!public! !
!FakeTarfileEntry class categoriesFor: #folderIcon!constants!public! !

TarFileTreeModel guid: (GUID fromString: '{8611BAE4-8D60-4872-945E-CF9208094EEB}')!
TarFileTreeModel comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Simple utility class to help with displaying the contents of a Tarfile in a TreePresenter or similar.

NB: currently this is a static view.  If I ever add change notification to TarFile then I''ll use that to make this model update itself accordingly.

E.g:

	TreePresenter showOn:
		((TarFileTreeModel for:
			(TarFile readingFile:
				(FileOpenDialog showModal)))).

Or, if you have my ListTreeView, you can be more grandiose...

	model := TarFileTreeModel for: (TarFile readingFile: FileOpenDialog showModal).
	columns := #(#shortName #modeString #filetime #contentsSize #uname #gname) collect:
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
!TarFileTreeModel categoriesForClass!Unclassified! !
!TarFileTreeModel methodsFor!

add: aTarFileEntry using: aMap and: aSet
	"private -- add the given entry using, and if necessary updating, the supplied name->entry map"

	| name index parentName parent |

	name := aTarFileEntry filename.
	index := name prevIndexOf: $/ from: name size - 1 to: 1.
	parentName := index = 0 ifTrue: [''] ifFalse: [name first: index].
	parent := aMap
			at: parentName
			ifAbsentPut: [FakeTarfileEntry filename: parentName].

	(parent isNil or: [aSet includes: parent]) ifFalse: [self add: parent using: aMap and: aSet].

	self add: aTarFileEntry asChildOf: parent.
	aSet add: aTarFileEntry.
!

tarFile
	"answer the tar file of which we present a view"

	^ tarFile.!

tarFile: aTarFile
	"private -- set the TarFile of which we present a view"

	| map done |

	tarFile := aTarFile.

	map := LookupTable new.
	map at: '' put: nil.
	tarFile do: [:each | map at: each filename put: each].

	done := IdentitySet new.
	tarFile do: [:each | self add: each using: map and: done].
! !
!TarFileTreeModel categoriesFor: #add:using:and:!initializing!private! !
!TarFileTreeModel categoriesFor: #tarFile!accessing!public! !
!TarFileTreeModel categoriesFor: #tarFile:!initializing!private! !

!TarFileTreeModel class methodsFor!

for: aTarFile
	"answer a new instance that presents a static
	view of the contents of aTarFIle"

	^ (self new)
		tarFile: aTarFile;
		yourself.! !
!TarFileTreeModel class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

