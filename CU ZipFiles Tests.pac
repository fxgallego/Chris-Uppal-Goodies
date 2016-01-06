| package |
package := Package name: 'CU ZipFiles Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

A few basic tests for the Zip file handing.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

History:

2.00
-	Small changes corresponding to v3 of ''CU ZipFiles''.

1.00
-	First release.
'.

package basicPackageVersion: '2.00'.


package classNames
	add: #ZipFileInFileTest;
	add: #ZipFileInMemoryTest;
	add: #ZipFileTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU ZipFiles';
	add: 'CU ZLib Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ZipFileTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'ZipFileConstants ZLib1Constants'
	classInstanceVariableNames: ''!
ZipFileTest subclass: #ZipFileInFileTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'runTestsInDirectory'!
ZipFileTest subclass: #ZipFileInMemoryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ZipFileTest guid: (GUID fromString: '{15235388-4EBF-49B2-AEB3-AD8B0D6295D5}')!
ZipFileTest comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Some basic tests of Zip file handing.

NB: these tests only attempt to confirm that data written by this library can be read by it.

It would be possible to use some external tool such as ''zip'' and ''zipinfo'' to automate the validation this library against other Zip tools, but unfortunately they are both part of Cygwin, and I don''t know how to run Cygwin programs from Dolphin.  So I don''t.'!
!ZipFileTest categoriesForClass!Unclassified! !
!ZipFileTest methodsFor!

add: aDictionary to: aZipFile under: aString streamed: aBool

	| name entry contents |

	name := aDictionary at: #filename ifAbsent: [''].
	contents := aDictionary at: #contents ifAbsent: [nil].
	aDictionary
		removeKey: #filename ifAbsent: [];
		removeKey: #contents ifAbsent: [].

	entry := aZipFile openEntry: (aString , name).

	aDictionary keysAndValuesDo:
		[:key :value || selector |
		selector := (key , ':') asSymbol.
		entry perform: selector with: value].

	contents notNil ifTrue:
		[aBool
			ifTrue: [entry withWriterDo: [:writer | writer nextPutAll: contents]]
			ifFalse: [entry write: contents]].

	aZipFile closeEntry.
!

addStandardDataTo: aZipFile under: aString

	self addStandardDataTo: aZipFile under: aString streamed: true.
!

addStandardDataTo: aZipFile under: aString streamed: aBool

	self allStandardData do: [:each | self add: each to: aZipFile under: aString streamed: aBool].!

allStandardData

	^ (OrderedCollection new)
		add: self emptyFileData;
		add: self emptyFileData2;
		add: self emptyDirectoryData;
		add: self uncompressibleFileData;
		add: self compressibleFileData;
		add: self miscellaneousData;
		yourself.!

closeAndReopenForAppend: aZipFile

	self subclassResponsibility.!

closeAndReopenForRead: aZipFile

	self subclassResponsibility.
!

compressibleFileData

	| words stream data |

	"from 'Psmith in the City', P. G. Woodehouse"
	words := 'Agesilaus ever afterwards had a distaste for pterodactyls.
'.

	stream := String writeStream: (words size * 1000).
	1000 timesRepeat: [stream nextPutAll: words].
	data := stream contents.

	^ (self makeFileData: 'compressible.txt')
		at: #contents put: data;
		at: #isText put: true;
		yourself.
!

createNewZipFile: aString

	self subclassResponsibility.
!

emptyDirectoryData

	"by convention, it seems that directories are not added to Zip files, however..."

	^ (self makeFileData: 'empty-directory/')
		yourself.
!

emptyFileData

	^ (self makeFileData: 'empty-file.txt')
		at: #contents put: '';
		at: #isText put: true;
		yourself.
!

emptyFileData2

	"not writing any content ~= writing 0-bytes"

	^ (self makeFileData: 'empty-file-2.txt')
		yourself.
!

makeFileData: aString

	^ (LookupTable new)
		at: #filename put: aString;
		yourself.
!

miscellaneousData

	| date time comment contents |

	date := Date fromString: '2004/01/01' format: 'yyy/mm/dd/'.
	time := Time fromString: '12:00:00'.
	comment := 'This is the comment'.

	contents := String writeStream
			display: 'Date should be '; display: date; cr;
			display: 'Time should be '; display: time; cr;
			display: 'Comment should be '; print: comment; cr;
			contents.

	^ (self makeFileData: 'miscellaneous.txt')
		at: #contents put: contents;
		at: #isText put: true;
		at: #lastModifiedDate put: date;
		at: #lastModifiedTime put: time;
		at: #comment put: 'This is the comment';
		at: #msdosFileAttributes put: ZF_MSDOS_READONLY_MASK;
		yourself.
!

test: aDictionary in: aZipFile under: aString

	| name entry |

	name := aDictionary removeKey: #filename ifAbsent: [''].

	entry := aZipFile at: (aString , name) ifAbsent: [nil].
	self should: [entry notNil].

	aDictionary keysAndValuesDo:
		[:key :shouldBe || is |
		is := entry perform: key.
		self should: [is = shouldBe]].
!

testAppend

	| zipfile |

	zipfile := self createNewZipFile: 'append'.

	self addStandardDataTo: zipfile under: 'before/'.

	zipfile := self closeAndReopenForAppend: zipfile.

	self addStandardDataTo: zipfile under: 'after/'.

	zipfile := self closeAndReopenForRead: zipfile.

	self testStandardData: zipfile under: 'before/'.
	self testStandardData: zipfile under: 'after/'.
!

testCompressionLevelsNotStreamed

	| zipfile |

	zipfile := self createNewZipFile: 'compression-levels+not-streamed'.

	zipfile compressEntries: false.
	self addStandardDataTo: zipfile under: 'uncompressed/' streamed: false.

	zipfile compressEntries: true.
	0 to: 9 do:
		[:i |
		zipfile entryCompressionLevel: i.
		self addStandardDataTo: zipfile under: ('compressed/%d/' sprintfWith: i) streamed: false].

	zipfile := self closeAndReopenForRead: zipfile.
	self testStandardData: zipfile under: 'uncompressed/'.
	0 to: 9 do: [:i | self testStandardData: zipfile under: ('compressed/%d/' sprintfWith: i)].
!

testCompressionLevelsStreamed

	| zipfile |

	zipfile := self createNewZipFile: 'compression-levels+streamed'.

	zipfile compressEntries: false.
	self addStandardDataTo: zipfile under: 'uncompressed/' streamed: true.

	zipfile compressEntries: true.
	0 to: 9 do:
		[:i |
		zipfile entryCompressionLevel: i.
		self addStandardDataTo: zipfile under: ('compressed/%d/' sprintfWith: i) streamed: true].

	zipfile := self closeAndReopenForRead: zipfile.
	self testStandardData: zipfile under: 'uncompressed/'.
	0 to: 9 do: [:i | self testStandardData: zipfile under: ('compressed/%d/' sprintfWith: i)].
!

testEmpty

	| zipfile |

	zipfile := self createNewZipFile: 'empty'.

	zipfile := self closeAndReopenForRead: zipfile.
	self testEmpty: zipfile.
!

testEmpty: aZipFile

	self should: [aZipFile size = 0].
	self should: [aZipFile isEmpty].
	self should: [aZipFile entries isEmpty].
	self shouldnt: [aZipFile includes: 'Brer Rabbit'].
	self should: [aZipFile at: 'Brer Rabbit'] raise: NotFoundError.
	aZipFile do: [:each | self should: [false]].
!

testNonEmpty

	| zipfile  comment |

	zipfile := self createNewZipFile: 'non-empty'.

	self addStandardDataTo: zipfile under: ''.
	zipfile comment: (comment := 'This is a ZipFile comment.').

	zipfile := self closeAndReopenForRead: zipfile.
	self testStandardData: zipfile under: ''.
	self should: [zipfile comment = comment].
!

testStandardData: aZipFile under: aString

	| data count |

	data := self allStandardData.

	self shouldnt: [aZipFile includes: 'Brer Rabbit'].
	self should: [aZipFile at: 'Brer Rabbit'] raise: NotFoundError.

	data do: [:each | self test: each in: aZipFile under: aString].

!

uncompressibleFileData

	| data |

	data := ByteArray withAll: (0 to: 255).

	^ (self makeFileData: 'uncompressible.dat')
		at: #contents put: data;
		at: #isText put: false;
		yourself.
! !
!ZipFileTest categoriesFor: #add:to:under:streamed:!public!test helpers! !
!ZipFileTest categoriesFor: #addStandardDataTo:under:!public!test helpers! !
!ZipFileTest categoriesFor: #addStandardDataTo:under:streamed:!public!test helpers! !
!ZipFileTest categoriesFor: #allStandardData!constants!private! !
!ZipFileTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!ZipFileTest categoriesFor: #closeAndReopenForRead:!helpers!public! !
!ZipFileTest categoriesFor: #compressibleFileData!constants!private! !
!ZipFileTest categoriesFor: #createNewZipFile:!helpers!public! !
!ZipFileTest categoriesFor: #emptyDirectoryData!constants!private! !
!ZipFileTest categoriesFor: #emptyFileData!constants!private! !
!ZipFileTest categoriesFor: #emptyFileData2!constants!private! !
!ZipFileTest categoriesFor: #makeFileData:!helpers!private! !
!ZipFileTest categoriesFor: #miscellaneousData!constants!private! !
!ZipFileTest categoriesFor: #test:in:under:!public!test helpers! !
!ZipFileTest categoriesFor: #testAppend!public!unit tests! !
!ZipFileTest categoriesFor: #testCompressionLevelsNotStreamed!public!unit tests! !
!ZipFileTest categoriesFor: #testCompressionLevelsStreamed!public!unit tests! !
!ZipFileTest categoriesFor: #testEmpty!public!unit tests! !
!ZipFileTest categoriesFor: #testEmpty:!public!test helpers! !
!ZipFileTest categoriesFor: #testNonEmpty!public!unit tests! !
!ZipFileTest categoriesFor: #testStandardData:under:!public!test helpers! !
!ZipFileTest categoriesFor: #uncompressibleFileData!constants!private! !

!ZipFileTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].!

isAbstract

	^self = ##(self).! !
!ZipFileTest class categoriesFor: #allTestSelectors!accessing!public! !
!ZipFileTest class categoriesFor: #isAbstract!public!testing! !

ZipFileInFileTest guid: (GUID fromString: '{67B960D2-9510-4112-94E6-73EB43AF2760}')!
ZipFileInFileTest comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org

Note, these tests deliberately:
	1) fail if you have not set a target directory (ZipFileInFile class>>runTestsInDirectory:).
	2) leave the generated files behind after they have run, thus allowing inspection with external tools.'!
!ZipFileInFileTest categoriesForClass!Unclassified! !
!ZipFileInFileTest methodsFor!

closeAndReopenForAppend: aZipFile

	aZipFile close.

	^ ZipFile appendingFile: aZipFile filename.
!

closeAndReopenForRead: aZipFile

	aZipFile close.

	^ ZipFile readingFile: aZipFile filename.!

createNewZipFile: aString

	^ ZipFile writingFile: (self class testFileName: aString).! !
!ZipFileInFileTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!ZipFileInFileTest categoriesFor: #closeAndReopenForRead:!helpers!public! !
!ZipFileInFileTest categoriesFor: #createNewZipFile:!helpers!public! !

!ZipFileInFileTest class methodsFor!

openTestDirectory
	"
		self openTestDirectory.
	"

	"deliberately fail if #runTestsInDirectory has not been set"
	self assert: [runTestsInDirectory notNil].

	ShellLibrary default shellOpen: '' directory: runTestsInDirectory.!

resetTestDirectory
	"ensure that the test directory exists (if set at all) and that it contains no
	zip files.

		self resetTestDirectory.
	"

	| existing |

	"deliberately fail if #runTestsInDirectory has not been set"
	self assert: [runTestsInDirectory notNil].

	(File isDirectory: runTestsInDirectory) ifFalse:
		[| ok |
		ok := MessageBox confirm: runTestsInDirectory , ' does not exist, OK to create it ?'.
		ok ifFalse: [^ self].
		File createDirectoryPath: runTestsInDirectory].

	existing := OrderedCollection new.
	File for: '*.zip' in: runTestsInDirectory do: [:each | existing add: each path].
	existing do: [:each | File delete: each].
!

runTestsInDirectory

	^ runTestsInDirectory.!

runTestsInDirectory: aStringOrNil
	"
		self runTestsInDirectory: 'C:\Temp\ZipFileTests'.

		self runTestsInDirectory: nil.
	"

	runTestsInDirectory := aStringOrNil.!

testFileName: aString

	"deliberately fail if #runTestsInDirectory has not been set"
	self assert: [runTestsInDirectory notNil].

	^ File composePath: runTestsInDirectory stem: aString extension: '.zip'.! !
!ZipFileInFileTest class categoriesFor: #openTestDirectory!helpers!public! !
!ZipFileInFileTest class categoriesFor: #resetTestDirectory!helpers!public! !
!ZipFileInFileTest class categoriesFor: #runTestsInDirectory!accessing!public! !
!ZipFileInFileTest class categoriesFor: #runTestsInDirectory:!accessing!public! !
!ZipFileInFileTest class categoriesFor: #testFileName:!helpers!private! !

ZipFileInMemoryTest guid: (GUID fromString: '{AF85708C-5D49-4FA5-88AB-AE9C390D5B57}')!
ZipFileInMemoryTest comment: 'Copyright © Chris Uppal, 2004, 2005.
chris.uppal@metagnostic.org
'!
!ZipFileInMemoryTest categoriesForClass!Unclassified! !
!ZipFileInMemoryTest methodsFor!

closeAndReopenForAppend: aZipFile

	| bytes |

	aZipFile close.

	bytes := aZipFile stream contents.

	^ (ZipFile inMemory: bytes)
		name: aZipFile name;
		yourself.
!

closeAndReopenForRead: aZipFile

	| bytes |

	aZipFile close.

	bytes := aZipFile stream contents.

	^ (ZipFile fromBytes: bytes)
		name: aZipFile name;
		yourself.

!

createNewZipFile: aString

	^ (ZipFile inMemory)
		name: aString;
		yourself.! !
!ZipFileInMemoryTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!ZipFileInMemoryTest categoriesFor: #closeAndReopenForRead:!helpers!public! !
!ZipFileInMemoryTest categoriesFor: #createNewZipFile:!helpers!public! !

"Binary Globals"!

"Resources"!

