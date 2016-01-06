| package |
package := Package name: 'CU TarFiles Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

A few basic tests for the TAR file handing.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.00'.


package classNames
	add: #TarFileInFileTest;
	add: #TarFileInMemoryTest;
	add: #TarFileTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU TarFiles';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #TarFileTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'TarFileConstants'
	classInstanceVariableNames: ''!
TarFileTest subclass: #TarFileInFileTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'runTestsInDirectory'!
TarFileTest subclass: #TarFileInMemoryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

TarFileTest guid: (GUID fromString: '{6565DE34-5A0C-4B86-A594-2C67BB930CA1}')!
TarFileTest comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Some basic tests of TAR file handing.

NB: these tests only attempt to confirm that data written by this library can be read by it.
'!
!TarFileTest categoriesForClass!Unclassified! !
!TarFileTest methodsFor!

add: aDictionary to: aTarFile under: aString streamed: aBool

	| name entry textContents binaryContents |

	name := aDictionary at: #filename.
	textContents := aDictionary at: #textContents ifAbsent: [nil].
	binaryContents := aDictionary at: #binaryContents ifAbsent: [nil].
	aDictionary
		removeKey: #filename ifAbsent: [];
		removeKey: #textContents ifAbsent: [];
		removeKey: #binaryContents ifAbsent: [].

	entry := aTarFile openEntry: (aString , name).

	aDictionary keysAndValuesDo:
		[:key :value || selector |
		selector := (key , ':') asSymbol.
		entry perform: selector with: value].

	textContents notNil ifTrue:
		[aBool
			ifTrue: [entry withTextWriterDo: [:writer | writer nextPutAll: textContents]]
			ifFalse: [entry writeText: textContents]].

	binaryContents notNil ifTrue:
		[aBool
			ifTrue: [entry withBinaryWriterDo: [:writer | writer nextPutAll: binaryContents]]
			ifFalse: [entry writeBinary: binaryContents]].

	aTarFile closeEntry.
!

addStandardDataTo: aTarFile

	self addStandardDataTo: aTarFile under: ''.
!

addStandardDataTo: aTarFile under: aString

	"unlike Zip files, the natural mode is non-streamed, so we make that the default"
	self addStandardDataTo: aTarFile under: aString streamed: false.
!

addStandardDataTo: aTarFile under: aString streamed: aBool

	self allStandardData do: [:each | self add: each to: aTarFile under: aString streamed: aBool].!

allStandardData

	^ (OrderedCollection new)
		add: self emptyFileData;
		add: self emptyFileData2;
		add: self emptyDirectoryData;
		add: self binaryFileData;
		add: self textFileData;
		add: self miscellaneousData;
		yourself.!

binaryFileData

	| data |

	data := ByteArray withAll: (0 to: 255).

	^ (self makeFileData: 'binary.dat')
		at: #binaryContents put: data;
		yourself.
!

closeAndReopenForAppend: aTarFile

	self subclassResponsibility.!

closeAndReopenForRead: aTarFile

	^ self closeAndReopenForRead: aTarFile text: true.
!

closeAndReopenForRead: aTarFile text: aBool

	self subclassResponsibility.
!

createNewTarFile: aString

	self subclassResponsibility.
!

emptyDirectoryData

	"by convention, it seems that directories are not added to Tar files, however..."

	^ (self makeFileData: 'empty-directory/')
		yourself.
!

emptyFileData

	^ (self makeFileData: 'empty-file.txt')
		at: #textContents put: '';
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

	| date time filetime mode data stream |

	date := Date fromString: '2005/01/01' format: 'yyy/mm/dd/'.
	time := Time fromString: '12:00:00'.
	filetime := (TimeStamp date: date time: time) asParameter asFILETIME.

	" -rwxr--r-x"
	mode := 0
			| TAR_MODE_UREAD | TAR_MODE_UWRITE | TAR_MODE_UEXEC
			| TAR_MODE_GREAD
			| TAR_MODE_OREAD | TAR_MODE_OEXEC.

	data := (self makeFileData: 'miscellaneous.txt')
			at: #filetime put: filetime;
			at: #uid put: 127;
			at: #uname put: 'someone';
			at: #gid put: 712;
			at: #gname put: 'people';
			at: #mode put: mode;
			yourself.

	stream := String writeStream.
	data keys asSortedCollection do:
		[:key | stream display: key capitalized; space; display: 'should be '; display: (data at: key); cr].

	data at: #textContents put: stream contents.

	^ data.!

test: aDictionary in: aTarFile under: aString

	| name entry |

	name := aDictionary removeKey: #filename ifAbsent: [''].

	entry := aTarFile at: (aString , name) ifAbsent: [nil].
	self should: [entry notNil].

	aDictionary keysAndValuesDo:
		[:key :shouldBe || is |
		is := entry perform: key.
		self should: [is = shouldBe]].
!

testAppend

	| tarfile |

	tarfile := self createNewTarFile: 'append'.

	self addStandardDataTo: tarfile under: 'before/'.

	tarfile := self closeAndReopenForAppend: tarfile.

	self addStandardDataTo: tarfile under: 'after/'.

	tarfile := self closeAndReopenForRead: tarfile.

	self testStandardData: tarfile under: 'before/'.
	self testStandardData: tarfile under: 'after/'.!

testEmpty

	| tarfile |

	tarfile := self createNewTarFile: 'empty'.

	tarfile := self closeAndReopenForRead: tarfile.
	self testEmpty: tarfile.
!

testEmpty: aTarFile

	self should: [aTarFile size = 0].
	self should: [aTarFile isEmpty].
	self should: [aTarFile entries isEmpty].
	self shouldnt: [aTarFile includes: 'Brer Rabbit'].
	self should: [aTarFile at: 'Brer Rabbit'] raise: NotFoundError.
	aTarFile do: [:each | self should: [false]].
!

testNotStreamed

	| tarfile  comment |

	tarfile := self createNewTarFile: 'not-streamed'.

	self addStandardDataTo: tarfile under: '' streamed: false.

	tarfile := self closeAndReopenForRead: tarfile.
	self testStandardData: tarfile under: ''.
!

testReadBinary

	| tarfile  comment |

	tarfile := self createNewTarFile: 'read-binary'.

	self addStandardDataTo: tarfile.

	tarfile := self closeAndReopenForRead: tarfile text: false.
	self testStandardData: tarfile under: ''.
!

testReadText

	| tarfile comment |

	tarfile := self createNewTarFile: 'read-text'.

	self addStandardDataTo: tarfile under: ''.

	tarfile := self closeAndReopenForRead: tarfile text: true.
	self testStandardData: tarfile under: ''.
!

testStandardData: aTarFile under: aString

	| data count |

	data := self allStandardData.

	self shouldnt: [aTarFile includes: 'Brer Rabbit'].
	self should: [aTarFile at: 'Brer Rabbit'] raise: NotFoundError.

	data do: [:each | self test: each in: aTarFile under: aString].

!

testWriteNotStreamed

	| tarfile  comment |

	tarfile := self createNewTarFile: 'not-streamed'.

	self addStandardDataTo: tarfile under: '' streamed: false.

	tarfile := self closeAndReopenForRead: tarfile.
	self testStandardData: tarfile under: ''.
!

testWriteStreamed

	| tarfile  comment |

	tarfile := self createNewTarFile: 'streamed'.

	self addStandardDataTo: tarfile under: '' streamed: true.

	tarfile := self closeAndReopenForRead: tarfile.
	self testStandardData: tarfile under: ''.
!

textFileData

	| words stream data |

	"from 'Psmith in the City', P. G. Woodehouse"
	words := 'Agesilaus ever afterwards had a distaste for pterodactyls.'.

	stream := String writeStream.
	10 timesRepeat: [stream nextPutAll: words; cr].
	data := stream contents.

	^ (self makeFileData: 'text.txt')
		at: #textContents put: data;
		yourself.
! !
!TarFileTest categoriesFor: #add:to:under:streamed:!public!test helpers! !
!TarFileTest categoriesFor: #addStandardDataTo:!public!test helpers! !
!TarFileTest categoriesFor: #addStandardDataTo:under:!public!test helpers! !
!TarFileTest categoriesFor: #addStandardDataTo:under:streamed:!public!test helpers! !
!TarFileTest categoriesFor: #allStandardData!constants!private! !
!TarFileTest categoriesFor: #binaryFileData!constants!private! !
!TarFileTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!TarFileTest categoriesFor: #closeAndReopenForRead:!helpers!public! !
!TarFileTest categoriesFor: #closeAndReopenForRead:text:!helpers!public! !
!TarFileTest categoriesFor: #createNewTarFile:!helpers!public! !
!TarFileTest categoriesFor: #emptyDirectoryData!constants!private! !
!TarFileTest categoriesFor: #emptyFileData!constants!private! !
!TarFileTest categoriesFor: #emptyFileData2!constants!private! !
!TarFileTest categoriesFor: #makeFileData:!helpers!private! !
!TarFileTest categoriesFor: #miscellaneousData!constants!private! !
!TarFileTest categoriesFor: #test:in:under:!public!test helpers! !
!TarFileTest categoriesFor: #testAppend!public!unit tests! !
!TarFileTest categoriesFor: #testEmpty!public!unit tests! !
!TarFileTest categoriesFor: #testEmpty:!public!test helpers! !
!TarFileTest categoriesFor: #testNotStreamed!public!unit tests! !
!TarFileTest categoriesFor: #testReadBinary!public!unit tests! !
!TarFileTest categoriesFor: #testReadText!public!unit tests! !
!TarFileTest categoriesFor: #testStandardData:under:!public!test helpers! !
!TarFileTest categoriesFor: #testWriteNotStreamed!public!unit tests! !
!TarFileTest categoriesFor: #testWriteStreamed!public!unit tests! !
!TarFileTest categoriesFor: #textFileData!constants!private! !

!TarFileTest class methodsFor!

allTestSelectors

	^ super allTestSelectors select: [:each | each argumentCount = 0].!

isAbstract

	^self = ##(self).! !
!TarFileTest class categoriesFor: #allTestSelectors!accessing!public! !
!TarFileTest class categoriesFor: #isAbstract!public!testing! !

TarFileInFileTest guid: (GUID fromString: '{B8159C0E-A74C-414F-9D0D-5DBB51EE9DE6}')!
TarFileInFileTest comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Note, these tests deliberately:
	1) fail if you have not set a target directory (TarFileInFile class>>runTestsInDirectory:).
	2) leave the generated files behind after they have run, thus allowing inspection with external tools.'!
!TarFileInFileTest categoriesForClass!Unclassified! !
!TarFileInFileTest methodsFor!

closeAndReopenForAppend: aTarFile

	aTarFile close.

	^ TarFile appendingFile: aTarFile filename.
!

closeAndReopenForRead: aTarFile text: aBool

	aTarFile close.

	^ TarFile readingFile: aTarFile filename text: aBool.
!

createNewTarFile: aString

	^ TarFile writingFile: (self class testFileName: aString).! !
!TarFileInFileTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!TarFileInFileTest categoriesFor: #closeAndReopenForRead:text:!helpers!public! !
!TarFileInFileTest categoriesFor: #createNewTarFile:!helpers!public! !

!TarFileInFileTest class methodsFor!

openTestDirectory
	"
		self openTestDirectory.
	"

	"deliberately fail if #runTestsInDirectory has not been set"
	self assert: [runTestsInDirectory notNil].

	ShellLibrary default shellOpen: '' directory: runTestsInDirectory.!

resetTestDirectory
	"ensure that the test directory exists (if set at all) and that it contains no
	tar files.

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
	File for: '*.tar' in: runTestsInDirectory do: [:each | existing add: each path].
	existing do: [:each | File delete: each].
!

runTestsInDirectory

	^ runTestsInDirectory.!

runTestsInDirectory: aStringOrNil
	"
		self runTestsInDirectory: 'C:\Temp\TarFileTests'.

		self runTestsInDirectory: nil.
	"

	runTestsInDirectory := aStringOrNil.!

testFileName: aString

	"deliberately fail if #runTestsInDirectory has not been set"
	self assert: [runTestsInDirectory notNil].

	^ File composePath: runTestsInDirectory stem: aString extension: '.tar'.! !
!TarFileInFileTest class categoriesFor: #openTestDirectory!helpers!public! !
!TarFileInFileTest class categoriesFor: #resetTestDirectory!helpers!public! !
!TarFileInFileTest class categoriesFor: #runTestsInDirectory!accessing!public! !
!TarFileInFileTest class categoriesFor: #runTestsInDirectory:!accessing!public! !
!TarFileInFileTest class categoriesFor: #testFileName:!helpers!private! !

TarFileInMemoryTest guid: (GUID fromString: '{685CA37B-541B-46B6-8919-C63F1AB9BF8B}')!
TarFileInMemoryTest comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org
'!
!TarFileInMemoryTest categoriesForClass!Unclassified! !
!TarFileInMemoryTest methodsFor!

closeAndReopenForAppend: aTarFile

	| bytes |

	aTarFile close.

	bytes := aTarFile stream contents.

	^ (TarFile inMemory: bytes)
		name: aTarFile name;
		yourself.
!

closeAndReopenForRead: aTarFile text: aBool

	| contents new |

	aTarFile close.

	contents := aTarFile stream contents.

	new := aBool
			ifTrue: [TarFile fromString: contents asString]
			ifFalse: [TarFile fromBytes: contents].
	new name: aTarFile name.

	^ new.!

createNewTarFile: aString

	^ (TarFile inMemory)
		name: aString;
		yourself.! !
!TarFileInMemoryTest categoriesFor: #closeAndReopenForAppend:!helpers!public! !
!TarFileInMemoryTest categoriesFor: #closeAndReopenForRead:text:!helpers!public! !
!TarFileInMemoryTest categoriesFor: #createNewTarFile:!helpers!public! !

"Binary Globals"!

"Resources"!

