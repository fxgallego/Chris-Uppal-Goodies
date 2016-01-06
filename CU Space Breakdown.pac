| package |
package := Package name: 'CU Space Breakdown'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

A simple tool for answering the question: ''what is taking up the space in my image ?''.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

3.00
-	Added ability to invoke the RB ReferenceFinder directly (if it''s installed).

2.00
-	Added count of finalisable instances.
-	Added ability to save data to file and do before/after comparisons.

1.00
-	First release.

'.

package basicPackageVersion: '3.01'.


package classNames
	add: #ReferenceBreakdown;
	add: #SpaceBreakdown;
	add: #SpaceBreakdownAbstractShell;
	add: #SpaceBreakdownRecord;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ReferenceBreakdown -> 'Default view';
	add: #SpaceBreakdown -> 'Default view';
	add: #SpaceBreakdown -> 'Read-only view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Hashed Pair';
	add: 'CU Package-relative File Locator';
	add: 'CU Sortblocks';
	add: 'CU Storage Size';
	add: 'CU Tools Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package setManualPrerequisites: #(
	'CU Storage Size').

package!

"Class Definitions"!

Object subclass: #SpaceBreakdownRecord
	instanceVariableNames: 'subject instances space finalizableInstances'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CUToolShell subclass: #SpaceBreakdownAbstractShell
	instanceVariableNames: 'threshold scanner timestamp'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpaceBreakdownAbstractShell subclass: #ReferenceBreakdown
	instanceVariableNames: 'targets'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpaceBreakdownAbstractShell subclass: #SpaceBreakdown
	instanceVariableNames: 'showMetaclasses scannedObjects scannedSize scannedFinalizable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SpaceBreakdownRecord guid: (GUID fromString: '{FA1A88ED-BC30-424B-A75C-F5BFFA347E99}')!
SpaceBreakdownRecord comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

Nasty little data holder class that is used by the Space Breakdown tool since Arrays became too much of a pest.'!
!SpaceBreakdownRecord categoriesForClass!Unclassified! !
!SpaceBreakdownRecord methodsFor!

classFromString: aString
	"private -- decode a class name into a Class.
	NB: has to cope with metaclasses with names like
		aClass class
	"

	^ (aString endsWith: ' class')
		ifTrue: [(self classFromString: (aString allButLast: 6)) class]
		ifFalse: [Smalltalk at: aString asSymbol].!

className
	"answer the String name of the class we hold data about"

	^ subject name.!

decrementAllBy: anotherRecord
	"private -- decrement all our counts by subtracting the values found in anotherRecord"

	instances := instances - anotherRecord instances.
	space := space - anotherRecord space.
	finalizableInstances := finalizableInstances - anotherRecord finalizableInstances.
!

finalizableInstances
	"answer how many finalisable instances of our class were found"

	^ finalizableInstances.!

incrementFinalizableInstances
	"private -- increment our count of how many finalisable instances our class has"

	finalizableInstances := finalizableInstances + 1.!

incrementInstances
	"private -- increment our count of how many instances our class has"

	instances := instances + 1.!

incrementSpaceBy: anInteger
	"private -- add anInteger to our record of how much space the instances of our class take up"

	space := space + anInteger.!

initialize
	"private -- establish a coherent intiial state"

	subject:= nil.
	instances := space := finalizableInstances := 0.!

instances
	"answer how many instances of our class were found"

	^ instances.!

populateFromString: aString
	"private -- initialise ourself by reading one line of CSV data from aStream"

	| stream |

	#CUtodo.  "this is pretty fragile logic..."

	stream := aString readStream.

	subject := self classFromString: (stream upTo: $,).
	instances := Integer fromString: (stream upTo: $,).
	space := Integer fromString: (stream upTo: $,).
	finalizableInstances := Integer fromString: (stream upTo: $,).

	"the stream should now be empty"!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' (';
		display: subject;
		space;
		display: instances;
		nextPutAll: ')'.!

space
	"answer how much space was taken up by instances"

	^ space.!

subject
	"answer the class we hold data about"

	^ subject.!

subject: aClass
	"private -- set the class about which we hold data"

	subject := aClass.!

writeRecordTo: aStream
	"private -- write ourself to aStream as tab-separated CSV format"

	aStream
		nextPutAll: subject name;
		nextPut: $,;
		display: instances;
		nextPut: $,;
		display: space;
		nextPut: $,;
		display: finalizableInstances;
		cr.! !
!SpaceBreakdownRecord categoriesFor: #classFromString:!helpers!private! !
!SpaceBreakdownRecord categoriesFor: #className!accessing!public! !
!SpaceBreakdownRecord categoriesFor: #decrementAllBy:!accessing!private! !
!SpaceBreakdownRecord categoriesFor: #finalizableInstances!accessing!public! !
!SpaceBreakdownRecord categoriesFor: #incrementFinalizableInstances!accessing!private! !
!SpaceBreakdownRecord categoriesFor: #incrementInstances!accessing!private! !
!SpaceBreakdownRecord categoriesFor: #incrementSpaceBy:!accessing!private! !
!SpaceBreakdownRecord categoriesFor: #initialize!initializing!private! !
!SpaceBreakdownRecord categoriesFor: #instances!accessing!public! !
!SpaceBreakdownRecord categoriesFor: #populateFromString:!initializing!private! !
!SpaceBreakdownRecord categoriesFor: #printOn:!printing!public! !
!SpaceBreakdownRecord categoriesFor: #space!accessing!public! !
!SpaceBreakdownRecord categoriesFor: #subject!accessing!public! !
!SpaceBreakdownRecord categoriesFor: #subject:!initializing!private! !
!SpaceBreakdownRecord categoriesFor: #writeRecordTo:!commands!operations!private! !

!SpaceBreakdownRecord class methodsFor!

for: aClass
	"answer a new instance that holds data about instances of aClass"

	^ (self new)
		initialize;
		subject: aClass;
		yourself.!

fromString: aString
	"private -- answer a new instance populated by reading CSV data from aString"

	^ (self new)
		populateFromString: aString;
		yourself.!

readRecordsFrom: aStream
	"answer a collection of isntances read from aStream"

	| all |

	#CUtodo.  "this skips any records for classes that are not present in the current image"

	all := OrderedCollection new.
	[aStream atEnd] whileFalse:
		[| line |
		line := aStream nextLine.
		line isEmpty ifFalse:
			[[all addLast: (self fromString: line)]
				on: NotFoundError
				do: [:err | "nothing"]]].

	^ all.!

writeColumnHeadersTo: aStream
	"private -- write our colunm headers to aStream (we use tab-separated CSV format)"

	aStream
		nextPutAll: 'Class name';
		nextPut: $,;
		nextPutAll: 'Instances';
		nextPut: $,;
		nextPutAll: 'Space';
		nextPut: $,;
		nextPutAll: 'Finalisable';
		cr.!

writeRecords: aCollection to: aStream
	"write a list of instances to aStream in tab-separated CSV format"

	aCollection do: [:each | each writeRecordTo: aStream].! !
!SpaceBreakdownRecord class categoriesFor: #for:!instance creation!public! !
!SpaceBreakdownRecord class categoriesFor: #fromString:!instance creation!private! !
!SpaceBreakdownRecord class categoriesFor: #readRecordsFrom:!operations!public! !
!SpaceBreakdownRecord class categoriesFor: #writeColumnHeadersTo:!operations!public! !
!SpaceBreakdownRecord class categoriesFor: #writeRecords:to:!operations!public! !

SpaceBreakdownAbstractShell guid: (GUID fromString: '{F7A75554-A4B5-4141-BEF1-B57F69F5EF2B}')!
SpaceBreakdownAbstractShell comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!SpaceBreakdownAbstractShell categoriesForClass!Unclassified! !
!SpaceBreakdownAbstractShell methodsFor!

buildFilterMenu: aMenu
	"private -- this is invoked when the dynamic filter menu is about to be displayed;
	update it appropriately"

	self subclassResponsibility.!

canCancel
	"private -- answer true iff we may perform the #cancel command ?"

	^ self isScanning.
!

cancel
	"command -- stop any ongoing scan"

	self canCancel ifFalse: [^ self].

	scanner := nil.
!

canRefresh
	"private -- answer true iff we may perform the #refresh command ?"

	^ self isScanning not.
!

controlsPresenter
	"private -- answer the presenter named 'Controls"

	^ self presenterNamed: 'Controls'.
!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	self add: (Presenter new) name: 'Top'.
	self add: (ListPresenter new) name: 'MainList'.
	self add: (Presenter new) name: 'Controls'.
	self add: (NumberPresenter new) name: 'Progress'.
!

defaultSortBlock
	"private -- answer the sort block to use for the main list before the user has clicked
	any column header"

	self subclassResponsibility.!

defaultThreshold
	"answer the threshold setting to use by default"

	^ 1.!

defer: a0Block
	"private -- evaluate a0Block in the UI thread"

	SessionManager inputState queueDeferredAction: a0Block.
!

deferNoFlicker: a0Block
	"private -- evaluate a0Block in the UI thread with flicker reduced"

	self defer: [self withoutFlickerDo: a0Block].
!

deferProgress: anInteger
	"private -- display how much progress we've made, but do it in the main GUI thread"

	self defer: [self progress: anInteger].
!

filteredList
	"answer a subsequence from our list of records with 'hidden' records removed"

	^ self model select: [:each | self isRecordShown: each].!

hasData
	"answer whether we have performed a scan yet"

	^ self model notEmpty and: [timestamp notNil].!

help
	"command -- display the help for this tool"

	| locator file |

	locator := PackageRelativeFileLocator package: self class owningPackage.
	file := locator localFileSpecFor: ('Docs\SpaceBreakdown.html').
	[ShellLibrary default shellOpen: file]
		on: Error
		do: [:err | MessageBox
			notify: ('Sorry.  No help for the ' , self class toolName , ' is available')
			caption: 'Space Breakdown Help'].

!

initialize
	"private -- establish a coherent inital state"

	threshold := self defaultThreshold.

	^ super initialize.
!

isRecordShown: anArray
	"private -- is the record one that we wish to show ?"

	self subclassResponsibility.!

isScanning
	"answer whether a scan is underway"

	^ scanner notNil.!

mainListPresenter
	"private -- answer the presenter named 'MainList'"

	^ self presenterNamed: 'MainList'.
!

model: anOrderedCollection
	"private -- set our model"

	super model: anOrderedCollection.
	self updateMainList.


!

onAboutToDisplayMenu: aMenu
	"private -- this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	| name |

	name := aMenu name.
	name == #dynamicFilterMenu ifTrue: [^ self buildFilterMenu: aMenu].

!

onViewClosed
	"our view has been closed -- cancel any on-going scan"

	self cancel.
	super onViewClosed.!

onViewOpened
	"called by the framework as the view is opened"

	super onViewOpened.
	self mainListPresenter beSorted: self defaultSortBlock.!

progress: anInteger
	"private -- display how much progress we've made"

	self progressPresenter value: anInteger.
!

progressPresenter
	"private -- answer the presenter named 'Progress'"

	^ self presenterNamed: 'Progress'.
!

queryCommand: aCommandQuery
	"private -- set the enabledness etc of aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	cmd isNil ifTrue: [^ self].

	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd = #cancel ifTrue: [enabled := self canCancel].
	cmd = #refresh ifTrue: [enabled := self canRefresh].

	cmd = #dynamicFilterMenu ifTrue: [enabled := self canRefresh].

	"spot the dynamically generated commands which change the threshold"
	(cmd asSymbol = #threshold:) ifTrue:
		[checked := cmd arguments first = threshold].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

refresh
	"command -- update the main list"

	self canRefresh ifFalse: [^ self].

	self scan.!

resetProgressBar
	"private -- ensure that the progress bar is at 0"

	self progress: 0.
!

resetProgressBar: size
	"private -- ensure that the progress bar is at 0 and make it range from 0 to size"

	| bar |

	bar := self progressPresenter.

	bar view range: (0 to: size).
	bar value: 0.
!

scan
	"private -- build an an OrderedCollection of records and make that be our new model.
	NB: runs the scan in a background thread"

	| scan |

	scan := 
		[| list progress |
		progress := -1.
		list := self scanNotifyingProgress:
			[:done |
			self deferProgress: (done * 16r1000) asInteger.
			scanner notNil].
		scanner := nil.
		self deferNoFlicker:
			[self resetProgressBar.
			self model: list.
			self controlsPresenter view invalidateUserInterface]].

	self model: self class defaultModel.
	self resetProgressBar: 16r1000.
	scanner := scan newProcess.
	scanner priority: Processor userBackgroundPriority.
	scanner resume.
!

scanNotifyingProgress: a1Block
	"private -- answer an an OrderedCollection of records.
	a1Block is evaluated periodically with our progress to date as a fraction; if
	it evaluates to false, then the scan is aborted and an incomplete list is returned"

	self subclassResponsibility.!

threshold: aNumber
	"command -- set the threshold above which we consider records to be worth displaying"

	threshold := aNumber.
	self updateMainList.!

timestamp
	"answer the TimeStamp of when our data was collected.
	May be nil if we haven't (finished) collected it yet"

	^ timestamp.!

timestamp: aTimeStamp
	"private -- set the TimeStamp of when our data was collected"

	| cap |

	timestamp := aTimeStamp.

	cap := (timestamp date = Date today)
			ifTrue: [timestamp time displayString]
			ifFalse: [timestamp displayString].
		
	self captionExtension: ('At ' , cap).!

timestampNow
	"private -- set the TimeStamp of when our data was collected to be now"

	"normally called from the scanning thread, so defer it"
	self defer: [self timestamp: TimeStamp new].!

topPresenter
	"private -- answer the presenter named 'Top"

	^ self presenterNamed: 'Top'.
!

updateMainList
	"private -- rebuild the main list from our underlying data"

	| presenter selection list |

	presenter := self mainListPresenter.
	list := self filteredList.
	selection := presenter selection select: [:each | list identityIncludes: each].
	
	presenter list: list; selection: selection.
	presenter view autoResizeColumns.

	self updateTotals.
!

updateTotals
	"private -- rebuild the total fields from our underlying data"

	self subclassResponsibility.!

withoutFlickerDo: a0Block
	"private -- evaluate a0Block with graphics updates temporarily suspended"

	self topPresenter view disableRedraw.
	a0Block ensure: [self topPresenter view enableRedraw; invalidate].
! !
!SpaceBreakdownAbstractShell categoriesFor: #buildFilterMenu:!menus!private! !
!SpaceBreakdownAbstractShell categoriesFor: #canCancel!commands!private! !
!SpaceBreakdownAbstractShell categoriesFor: #cancel!commands!public! !
!SpaceBreakdownAbstractShell categoriesFor: #canRefresh!commands!private! !
!SpaceBreakdownAbstractShell categoriesFor: #controlsPresenter!private!subpresenters! !
!SpaceBreakdownAbstractShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!SpaceBreakdownAbstractShell categoriesFor: #defaultSortBlock!constants!private! !
!SpaceBreakdownAbstractShell categoriesFor: #defaultThreshold!constants!public!scanning! !
!SpaceBreakdownAbstractShell categoriesFor: #defer:!helpers!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #deferNoFlicker:!helpers!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #deferProgress:!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #filteredList!accessing!private! !
!SpaceBreakdownAbstractShell categoriesFor: #hasData!accessing!public!testing! !
!SpaceBreakdownAbstractShell categoriesFor: #help!commands!public! !
!SpaceBreakdownAbstractShell categoriesFor: #initialize!initializing!private! !
!SpaceBreakdownAbstractShell categoriesFor: #isRecordShown:!private!testing! !
!SpaceBreakdownAbstractShell categoriesFor: #isScanning!accessing!public!testing! !
!SpaceBreakdownAbstractShell categoriesFor: #mainListPresenter!private!subpresenters! !
!SpaceBreakdownAbstractShell categoriesFor: #model:!initializing!models!private! !
!SpaceBreakdownAbstractShell categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!SpaceBreakdownAbstractShell categoriesFor: #onViewClosed!event handling!public! !
!SpaceBreakdownAbstractShell categoriesFor: #onViewOpened!event handling!public! !
!SpaceBreakdownAbstractShell categoriesFor: #progress:!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #progressPresenter!private!subpresenters! !
!SpaceBreakdownAbstractShell categoriesFor: #queryCommand:!commands!menus!public! !
!SpaceBreakdownAbstractShell categoriesFor: #refresh!commands!public! !
!SpaceBreakdownAbstractShell categoriesFor: #resetProgressBar!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #resetProgressBar:!private!progress! !
!SpaceBreakdownAbstractShell categoriesFor: #scan!private!progress!scanning! !
!SpaceBreakdownAbstractShell categoriesFor: #scanNotifyingProgress:!private!scanning! !
!SpaceBreakdownAbstractShell categoriesFor: #threshold:!commands!public! !
!SpaceBreakdownAbstractShell categoriesFor: #timestamp!accessing!public! !
!SpaceBreakdownAbstractShell categoriesFor: #timestamp:!accessing!private! !
!SpaceBreakdownAbstractShell categoriesFor: #timestampNow!accessing!private! !
!SpaceBreakdownAbstractShell categoriesFor: #topPresenter!private!subpresenters! !
!SpaceBreakdownAbstractShell categoriesFor: #updateMainList!private!updating! !
!SpaceBreakdownAbstractShell categoriesFor: #updateTotals!private!updating! !
!SpaceBreakdownAbstractShell categoriesFor: #withoutFlickerDo:!helpers!private!progress! !

!SpaceBreakdownAbstractShell class methodsFor!

defaultModel

	^ OrderedCollection new.! !
!SpaceBreakdownAbstractShell class categoriesFor: #defaultModel!constants!public! !

ReferenceBreakdown guid: (GUID fromString: '{5EA3C1B0-3B3F-497B-A297-AA6420C186B0}')!
ReferenceBreakdown comment: 'Copyright © Chris Uppal, 2002-2004.
chris.uppal@metagnostic.org

Shell that displays a list of the variables/slots that hold references to instances of a given set classes.

Can be launched from the Space Breakdown tool, or programmatically as in:

	ReferenceBreakdown showOn: aListOfClasses.

'!
!ReferenceBreakdown categoriesForClass!Unclassified! !
!ReferenceBreakdown methodsFor!

buildFilterMenu: aMenu
	"private -- this is invoked when the dynamic filter menu is about to be displayed;
	update it appropriately"

	| command item |

	aMenu clear.		"we build it fresh each time"

	command := Message selector: #threshold: argument: 1.
	item := CommandMenuItem command: command description: 'At least &1 reference'.
	item isRadioButtonStyle: true.
	item isDefault: true.
	aMenu addItem: item.

	((2 to: 9) , #(10 20 40 100 200 400 1000)) do:
		[:each |
		command := Message selector: #threshold: argument: each.
		item := CommandMenuItem command: command description: ('At least &' , each displayString , ' references').
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	^ aMenu.!

defaultSortBlock
	"private -- answer the sort block to use for the main list before the user has clicked
	any column header"

	^ SortDescending by: #second.!

initialize
	"private -- establish a coherent inital state"

	targets := #().

	^ super initialize.
!

isRecordShown: a2Array
	"private -- is the record one that we wish to show ?"

	^ a2Array second >= threshold.!

scanNotifyingProgress: a1Block
	"private -- answer an an OrderedCollection of
		<variable name, reference count>
	Arrays, representing all the references in the system to instances of our target classes.
	a1Block is evaluated periodically with our progress to date as a fraction; if
	it evaluates to false, then the scan is aborted and an incomplete list is returned"
	| objects uses list todo done |

	#CUtodo.  "note that we don't yet consider objects which are not subclasses of Object"

	(a1Block value: 0) ifFalse: [^ #()].

	"get the list of objects before we build the new map"
	objects := Object allSubinstances.
	objects := objects reject: [:each | each == objects or: [each isImmediate or: [each class isPointers not]]].

	"record when that list was valid"
	self timestampNow.

	"look for references to instances of target classes held in instvars"
	todo := objects size * 3.	"initial estimate -- will change"
	done := 0.
	uses := LookupTable new.
	objects do: [:each || class |

		(((done := done+1) allMask: 16rFFF) and: [(a1Block value: (done / todo)) not])
			ifTrue: [^ #()].

		class := each basicClass.
		1 to: class instSize do: [:i || ref |
			ref := each instVarAt: i.
			(targets identityIncludes: ref basicClass) ifTrue: [| hp refs |
				hp := HashedPair first: class second: i.
				refs := uses at: hp ifAbsentPut: [0].
				uses at: hp put: refs + 1]]].
	todo := todo - objects size.

	"look for strong references to instances of target classes held in indexed slots"
	objects := objects select: [:each | each isWeak not].
	todo := todo + objects size.
	objects do:
		[:each || refs |

		(((done := done+1) allMask: 16rFF) and: [(a1Block value: (done / todo)) not])
			ifTrue: [^ #()].

		refs := 0.
		1 to: each basicSize do: [:i | (targets identityIncludes: (each basicAt: i) basicClass) ifTrue: [refs := refs + 1]].
		refs > 0 ifTrue:
			[| hp |
			hp := HashedPair first: each basicClass second: 0.
			refs := refs + (uses at: hp ifAbsent: [0]).
			uses at: hp put: refs]].
	todo := todo - objects size.

	"convert our map into an OrderedCollection of Arrays"
	todo := todo + uses size.
	list := OrderedCollection new: uses size.
	uses keysAndValuesDo:
		[:key :value || class index name |

		(((done := done+1) allMask: 16rFF) and: [(a1Block value: (done / todo)) not])
			ifTrue: [^ list].

		class := key first.
		index := key second.
		name := index > 0
			ifTrue: [class allInstVarNames at: index]
			ifFalse: ['<indexed>'].
		name := class name , '.' , name.
		list add: (Array with: name with: value)].

	^ list.!

targets: aCollection
	"private -- set the collection of Classes that we are interested in instances of"

	| stream |

	targets := aCollection size > 3
			ifTrue: [IdentitySet withAll: aCollection]
			ifFalse: [Array withAll: aCollection].

	stream := String writeStream.
	aCollection do: [:each | each displayOn: stream] separatedBy: [stream space].
	self captionExtension: stream contents.
!

updateTotals
	"private -- rebuild the total fields from our underlying data"

	"nothing to do"! !
!ReferenceBreakdown categoriesFor: #buildFilterMenu:!menus!private! !
!ReferenceBreakdown categoriesFor: #defaultSortBlock!constants!private! !
!ReferenceBreakdown categoriesFor: #initialize!initializing!private! !
!ReferenceBreakdown categoriesFor: #isRecordShown:!private!testing! !
!ReferenceBreakdown categoriesFor: #scanNotifyingProgress:!private!scanning! !
!ReferenceBreakdown categoriesFor: #targets:!initializing!private! !
!ReferenceBreakdown categoriesFor: #updateTotals!private!updating! !

!ReferenceBreakdown class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Reference Breakdown.  Version 1.
Copyright © Chris Uppal, 2002-2004.
chris.uppal@metagnostic.org'.!

bugs
	"answer a String describing the less than outstanding work"

	^ '
Objects which are not instances of Object should be included.
'.!

classes: aCollection
	"show and answer a new instance which will give a breakdown of the
	references to instances of the classes and metaclasses in aCollection"

	^ (self show)
		targets: aCollection;
		yourself.!

help
	"answer a string describing ourselves"

	^ '
A simple breakdown of which class''s instance variables point to instances of the target class(es).

	-- chris
'.
!

todo
	"answer a String describing the outstanding work"

	^ '
Add browsing operations.
'.! !
!ReferenceBreakdown class categoriesFor: #about!documentation!public! !
!ReferenceBreakdown class categoriesFor: #bugs!documentation!public! !
!ReferenceBreakdown class categoriesFor: #classes:!instance creation!public! !
!ReferenceBreakdown class categoriesFor: #help!documentation!public! !
!ReferenceBreakdown class categoriesFor: #todo!documentation!public! !

SpaceBreakdown guid: (GUID fromString: '{D16FA4C3-7143-11D5-8727-A78243202A3E}')!
SpaceBreakdown comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

Shell that displays a list of the classes in the system with instance counts and RAM consumption estimates for each.'!
!SpaceBreakdown categoriesForClass!Unclassified! !
!SpaceBreakdown methodsFor!

add: anObject toScanMap: aMap
	"private -- add anObject into the Dictionary we are using to hold scanning data"
	| record class size |

	size := anObject storageSize.

	class := anObject basicClass.
	record := aMap at: class ifAbsentPut: [SpaceBreakdownRecord for: class].

	record
		incrementInstances;
		incrementSpaceBy: size.

	scannedObjects := scannedObjects + 1.
	scannedSize := scannedSize + size.

	anObject isFinalizable ifTrue:
		[record incrementFinalizableInstances.
		scannedFinalizable := scannedFinalizable + 1].

!

breakdownClassReferences
	"command -- open a class breakdown of the currently selected classes' references"

	self canBreakdownClassReferences ifFalse: [^ self].

	(ReferenceBreakdown classes: self selectedClasses)
		refresh.!

browseClass
	"command -- open a browser on the currently selected class"

	self canBrowseClass ifFalse: [^ self].

	self selectedClass browse.
!

buildFilterMenu: aMenu
	"private -- this is invoked when the dynamic filter menu is about to be displayed;
	update it appropriately"

	| command item |

	aMenu clear.		"we build it fresh each time"

	command := Message selector: #threshold: argument: 1.
	item := CommandMenuItem command: command description: 'At least &1 instance'.
	item isRadioButtonStyle: true.
	item isDefault: true.
	aMenu addItem: item.

	((2 to: 9) , #(10 20 40 100 200 400 1000)) do:
		[:each |
		command := Message selector: #threshold: argument: each.
		item := CommandMenuItem command: command description: ('At least &' , each displayString , ' instances').
		item isRadioButtonStyle: true.
		aMenu addItem: item].

	aMenu addItem: DividerMenuItem separator.

	command := Message selector: #threshold: argument: 0.
	item := CommandMenuItem command: command description: 'Show &all classes'.
	item isRadioButtonStyle: true.
	aMenu addItem: item.

	^ aMenu.!

canBreakdownClassReferences
	"private -- answer true iff we may perform the #breakdownClassReferences command ?"

	| classes |

	classes := self selectedClasses.
	^ classes notEmpty and: [classes allSatisfy: [:each | each inheritsFrom: Object]].
!

canBrowseClass
	"private -- answer true iff we may perform the #browseClass command ?"

	^ self selectedClass notNil.
!

canCompareWithFile
	"private -- answer true iff we may perform the #compareWithFile command ?"

	^ self hasData and: [self isScanning not].
!

canInspectInstances
	"private -- answer true iff we may perform the #inspectInstances command ?"

	^ self selectedClass notNil.
!

canLaunchReferenceFinder
	"private -- answer whether we may perform the #launchReferenceFinder command"

	^ (Smalltalk includesKey: #ReferenceFinder) and: [self selectedClass notNil].!

canLoadFromFile
	"private -- answer true iff we may perform the #loadFromFile command ?"

	^ self isScanning not.
!

canSaveToFile
	"private -- answer true iff we may perform the #saveToFile command ?"

	^ self hasData and: [self isScanning not].
!

compareWith: anOrderedCollection recordedAt: aTimeStamp
	"command-- launch a new instance on the differences between our current data and the old data"

	| map list shell cap |

	"we need to be able to find the classes quickly"
	map := IdentityDictionary new.
	self model do: [:each | map at: each subject put: each copy].

	"now subtract the old data"
	anOrderedCollection do:
		[:each || current |
		current := map at: each subject ifAbsentPut: [SpaceBreakdownRecord for: each subject].
		current decrementAllBy: each].

	"launch the new viewer"
	list := map asSortedCollection: self defaultSortBlock.
	shell := self class show: 'Read-only view' on: (ListModel on: list).

	"give it a nice caption too..."
	cap := (aTimeStamp date = timestamp date)
			ifTrue: [aTimeStamp time displayString , ' to ' , timestamp time displayString]
			ifFalse: [aTimeStamp displayString , ' to ' , timestamp displayString].
	shell caption: ('Space Increase - From ' , cap).
!

compareWithFile
	"command -- prompt for the name of a file containing previously recorded data,
	load the data from it, and display the differnces with our current state"

	| filename |

	self canCompareWithFile ifFalse: [^ self].

	filename := (FileOpenDialog new)
			caption: 'Compare with data from...';
			fileTypes: self class fileTypes;
			showModal.
	(filename isNil or: [filename isEmpty]) ifTrue: [^ self].

	self compareWithFile: filename.!

compareWithFile: aFilename
	"private -- load data from aFilename and then display the differences with our current
	data"

	| stream time data |

	stream := FileStream read: aFilename.

	time := self readFormatAndTimeStampFrom: stream.	"will throw error if not in OK format"

	data := self readRecordsFrom: stream.

	stream close.

	self compareWith: data recordedAt: time.!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	self add: (NumberPresenter new) name: 'TotalObjects'.
	self add: (NumberPresenter new) name: 'TotalSize'.
	self add: (NumberPresenter new) name: 'TotalFinalizable'.
!

defaultSortBlock
	"private -- answer the sort block to use for the main list before the user has clicked
	any column header"

	^ SortDescending by: #instances.!

deferProgress: anInteger
	"private -- display how much progress we've made, but do it in the main GUI thread"

	self defer: [self progress: anInteger objects: scannedObjects size: scannedSize finalizable: scannedFinalizable].
!

getAllObjects
	"private -- answer an Array of all the objects in the image"
	| objects |

	#CUtodo.  "note that we don't yet consider objects which are not subclasses of Object"
	objects := Object allSubinstances.
	^ objects reject: [:each | each == objects].!

initialize
	"private -- establish a coherent inital state"

	showMetaclasses := false.

	^ super initialize.
!

inspectInstances
	"command -- open an Inspector on the currently selected class's instances"

	self canInspectInstances ifFalse: [^ self].

	self selectedClass allInstances inspect.!

isRecordShown: aRecord
	"private -- is the record one that we wish to show ?"

	(showMetaclasses not and: [aRecord subject isMeta]) ifTrue: [^ false].

	"we may have negative instance couints if we're in difference mode"
	aRecord instances abs < threshold ifTrue: [^ false].

	^ true.!

launchReferenceFinder
	"command -- inspedt the results of using the RB's RreferenceFinder on the currently selected class"

	self canLaunchReferenceFinder ifFalse: [^ self].

	Smalltalk at: #ReferenceFinder ifPresent: [:it | (it findAllPathsToInstanceOf: self selectedClass) inspect].!

loadFromFile
	"command -- prompt for the name of a file containing previously recorded data,
	load the data from it, and display it in our list"

	| filename |

	self canLoadFromFile ifFalse: [^ self].

	filename := (FileOpenDialog new)
			caption: 'Load data from...';
			fileTypes: self class fileTypes;
			showModal.
	(filename isNil or: [filename isEmpty]) ifTrue: [^ self].

	self loadFromFile: filename.!

loadFromFile: aFilename
	"private -- load data from aFilename and then display it"

	| stream time data |

	stream := FileStream read: aFilename.

	time := self readFormatAndTimeStampFrom: stream.	"will throw error if not in OK format"

	data := self readRecordsFrom: stream.

	stream close.

	self
		model: data;
		timestamp: time.!

progress: anInteger objects: aCount size: aSize finalizable: anotherCount
	"private -- display how much progress we've made"

	self
		progress: anInteger;
		totalObjects: aCount;
		totalSize: aSize;
		totalFinalizable: anotherCount.
!

queryCommand: aCommandQuery
	"private -- set the enabledness etc of aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	cmd isNil ifTrue: [^ self].

	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd = #breakdownClassReferences ifTrue: [enabled := self canBreakdownClassReferences].
	cmd = #browseClass ifTrue: [enabled := self canBrowseClass].
	cmd = #inspectInstances ifTrue: [enabled := self canInspectInstances].

	cmd = #saveToFile ifTrue: [enabled := self canSaveToFile].
	cmd = #compareWithFile ifTrue: [enabled := self canCompareWithFile].

	cmd = #toggleShowMetaclasses ifTrue: [checked := showMetaclasses].

	cmd == #launchReferenceFinder ifTrue: [enabled := self canLaunchReferenceFinder].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

readFormatAndTimeStampFrom: aStream
	"private -- write our header from aStream, will simply throw errors if the format isn't OK
	Answers the discovered timestamp of the data"

	| line words |

	line := aStream nextLine.
	words := line subStrings.

	"using #assert is well lazy"
	self assert: [words size = 3].
	self assert: [(words first) = (self class fileFormatIdentifier)].
	self assert: [(words second) = (self class fileFormatVersion)].
	self assert: [words third notEmpty].

	^ TimeStamp fromSeconds: (Integer fromString: words third).!

readRecordsFrom: aStream
	"private -- answer a collection of records read from aStream"

	"skip the column headers"
	aStream nextLine.

	^ SpaceBreakdownRecord readRecordsFrom: aStream.!

saveToFile
	"command -- prompt for a file and save our data out to it"

	| filename |

	self canSaveToFile ifFalse: [^ self].

	filename := (FileSaveDialog new)
			caption: 'Save data to...';
			fileTypes: self class fileTypes;
			defaultExtension: 'csv';
			value: 'SpaceBreakdown';
			showModal.
	(filename isNil or: [filename isEmpty]) ifTrue: [^ self].

	((File exists: filename) not or: [self confirm: ('OK to  overwrite ''' ,  filename , ''' ?')])
		ifTrue: [self saveToFile: filename].!

saveToFile: aFilename
	"private -- and save our data out to to the named file,
	We write the format and timestamp on one line, then
	the records in CSV format"

	| stream |

	stream := FileStream write: aFilename.

	self
		writeFormatAndTimeStampTo: stream;
		writeRecordsTo: stream.

	stream close.

!

scanNotifyingProgress: a1Block
	"private -- answer an an OrderedCollection of
		<class name, instance count, space used, class>
	Arrays, representing all the classes in the system.
	a1Block is evaluated periodically with our progress to date as a fraction; if
	it evaluates to false, then the scan is aborted and an incomplete list is returned"
	| map objects size |

	#CUtodo.  "note that we don't yet consider objects which are not subclasses of Object"

	(a1Block value: 0) ifFalse: [^ #()].

	"get the list of objects before we build the new map"
	objects := self getAllObjects.

	"record when that list was valid"
	self timestampNow.

	"intialize the map (it helps if we put empty elements in now, even though we'd add them later if necessary)"
	map := IdentityDictionary new: 3000.
	Class allBehaviorsDo: [:each | map at: each put: (SpaceBreakdownRecord for: each)].
	scannedObjects := scannedSize := scannedFinalizable := 0.

	"and populate it"
	size := objects size.
	objects do:
		[:each |
		each == objects ifFalse: [self add: each toScanMap: map].
		((scannedObjects allMask: 16rFFF) and: [(a1Block value: (scannedObjects / size)) not])
			ifTrue: [^ map asOrderedCollection]].

	^ map asOrderedCollection.
!

selectedClass
	"private -- answer the currently selected class or nil if there isn't
	a single selection"

	| classes |

	classes := self selectedClasses.
	^ classes size = 1
		ifTrue: [classes first]
		ifFalse: [nil].	!

selectedClasses
	"private -- answer the currently selected classes, a possibly
	empty collection"

	^ self mainListPresenter selection collect: [:each | each subject].
	!

toggleShowMetaclasses
	"command -- toggle whether metaclasses are shown/hidden"

	showMetaclasses := showMetaclasses not.
	self updateMainList.!

totalFinalizable: anInteger
	"private -- display how many finalizable objects we've scanned"

	self totalFinalizablePresenter value: anInteger.
!

totalFinalizablePresenter
	"private -- answer the presenter named 'TotalFinalizable'"

	^ self presenterNamed: 'TotalFinalizable'.
!

totalObjects: anInteger
	"private -- display how many objects we've scanned"

	self totalObjectsPresenter value: anInteger.
!

totalObjectsPresenter
	"private -- answer the presenter named 'TotalObjects'"

	^ self presenterNamed: 'TotalObjects'.
!

totalSize: anInteger
	"private -- display how much space is taken by the objects we've scanned"

	self totalSizePresenter value: anInteger.
!

totalSizePresenter
	"private -- answer the presenter named 'TotalSize'"

	^ self presenterNamed: 'TotalSize'.
!

updateTotals
	"private -- rebuild the total fields from our underlying data"

	scannedObjects := scannedSize := scannedFinalizable := 0.

	self model do:
		[:each |
		scannedObjects := scannedObjects + each instances.
		scannedSize := scannedSize + each space.
		scannedFinalizable := scannedFinalizable + each finalizableInstances].

	self 
		totalObjects: scannedObjects;
		totalSize: scannedSize;
		totalFinalizable: scannedFinalizable.
!

writeFormatAndTimeStampTo: aStream
	"private -- write our data to aStream (we use a CSV format)"

	aStream
		nextPutAll: self class fileFormatIdentifier;
		space;
		nextPutAll: self class fileFormatVersion;
		space;
		display: timestamp asSeconds;
		cr.!

writeRecordsTo: aStream
	"private -- write our records to aStream (we use a CSV format)"

	SpaceBreakdownRecord
		writeColumnHeadersTo: aStream;
		writeRecords: self model to: aStream.! !
!SpaceBreakdown categoriesFor: #add:toScanMap:!private!scanning! !
!SpaceBreakdown categoriesFor: #breakdownClassReferences!commands!public! !
!SpaceBreakdown categoriesFor: #browseClass!commands!public! !
!SpaceBreakdown categoriesFor: #buildFilterMenu:!menus!private! !
!SpaceBreakdown categoriesFor: #canBreakdownClassReferences!commands!private! !
!SpaceBreakdown categoriesFor: #canBrowseClass!commands!private! !
!SpaceBreakdown categoriesFor: #canCompareWithFile!commands!private! !
!SpaceBreakdown categoriesFor: #canInspectInstances!commands!private! !
!SpaceBreakdown categoriesFor: #canLaunchReferenceFinder!commands!private! !
!SpaceBreakdown categoriesFor: #canLoadFromFile!commands!private! !
!SpaceBreakdown categoriesFor: #canSaveToFile!commands!private! !
!SpaceBreakdown categoriesFor: #compareWith:recordedAt:!commands!public! !
!SpaceBreakdown categoriesFor: #compareWithFile!commands!public! !
!SpaceBreakdown categoriesFor: #compareWithFile:!file operations!private! !
!SpaceBreakdown categoriesFor: #createComponents!initializing!private!subpresenters! !
!SpaceBreakdown categoriesFor: #defaultSortBlock!constants!private! !
!SpaceBreakdown categoriesFor: #deferProgress:!private!progress! !
!SpaceBreakdown categoriesFor: #getAllObjects!private!scanning! !
!SpaceBreakdown categoriesFor: #initialize!initializing!private! !
!SpaceBreakdown categoriesFor: #inspectInstances!commands!public! !
!SpaceBreakdown categoriesFor: #isRecordShown:!private!testing! !
!SpaceBreakdown categoriesFor: #launchReferenceFinder!commands!public! !
!SpaceBreakdown categoriesFor: #loadFromFile!commands!public! !
!SpaceBreakdown categoriesFor: #loadFromFile:!file operations!private! !
!SpaceBreakdown categoriesFor: #progress:objects:size:finalizable:!private!progress! !
!SpaceBreakdown categoriesFor: #queryCommand:!commands!public! !
!SpaceBreakdown categoriesFor: #readFormatAndTimeStampFrom:!file operations!private! !
!SpaceBreakdown categoriesFor: #readRecordsFrom:!file operations!private! !
!SpaceBreakdown categoriesFor: #saveToFile!commands!public! !
!SpaceBreakdown categoriesFor: #saveToFile:!file operations!private! !
!SpaceBreakdown categoriesFor: #scanNotifyingProgress:!private!scanning! !
!SpaceBreakdown categoriesFor: #selectedClass!accessing!private! !
!SpaceBreakdown categoriesFor: #selectedClasses!accessing!private! !
!SpaceBreakdown categoriesFor: #toggleShowMetaclasses!commands!public! !
!SpaceBreakdown categoriesFor: #totalFinalizable:!private!progress! !
!SpaceBreakdown categoriesFor: #totalFinalizablePresenter!private!subpresenters! !
!SpaceBreakdown categoriesFor: #totalObjects:!private!progress! !
!SpaceBreakdown categoriesFor: #totalObjectsPresenter!private!subpresenters! !
!SpaceBreakdown categoriesFor: #totalSize:!private!progress! !
!SpaceBreakdown categoriesFor: #totalSizePresenter!private!subpresenters! !
!SpaceBreakdown categoriesFor: #updateTotals!private!updating! !
!SpaceBreakdown categoriesFor: #writeFormatAndTimeStampTo:!file operations!private! !
!SpaceBreakdown categoriesFor: #writeRecordsTo:!file operations!private! !

!SpaceBreakdown class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Space Breakdown.  Version 3.
Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org'.!

bugs
	"answer a String describing the less than outstanding work"

	^ '
Objects which are not instances of Object should be included.
Load-from-file and compare-with-file both skip any records for unknown classes.
'.!

fileFormatIdentifier
	"answer the String identifier of our file format (must not contain spaces)"

	^ 'SpaceBreakdown'.!

fileFormatVersion
	"answer the String version of our file format (must not contain spaces)"

	^ 'v1'.!

fileTypes
	"answer an array of file types suitble for reading/writing our data"

	^ #(
		('CSV Files (*.csv)' '*.csv')
		('Text Files (*.txt)' '*.txt')
		('All Files' '*.*')
	).
!

help
	"answer a string describing ourselves"

	^ '
A simple display of how many instances each class has,
and how much memory they are using.

	-- chris
'.
!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	self registerAsTool.
!

todo
	"answer a String describing the outstanding work"

	^ '
Who knows....
'.!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	self unRegisterAsTool.! !
!SpaceBreakdown class categoriesFor: #about!documentation!public! !
!SpaceBreakdown class categoriesFor: #bugs!documentation!public! !
!SpaceBreakdown class categoriesFor: #fileFormatIdentifier!constants!public! !
!SpaceBreakdown class categoriesFor: #fileFormatVersion!constants!public! !
!SpaceBreakdown class categoriesFor: #fileTypes!constants!public! !
!SpaceBreakdown class categoriesFor: #help!documentation!public! !
!SpaceBreakdown class categoriesFor: #initialize!development!initializing!private! !
!SpaceBreakdown class categoriesFor: #todo!documentation!public! !
!SpaceBreakdown class categoriesFor: #uninitialize!development!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ReferenceBreakdown name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AwAAAFIEAAAAAAAAcAQAAGIAAAACAAAAAgIAAAAAAAALAAAACwAAAAICAAAAAAAA6QMAAOkDAACg
AQAAUgQAAAAAAAAQBQAAYgAAAAEAAABSAAAAEwAAAFJlZmVyZW5jZSBicmVha2Rvd26gAQAAUgQA
AAAAAAC6AAAAAAAAAFIAAAAIAAAAbWVudUJhcjpiAAAAAQAAAMAQAACgAQAAUgUAAAAAAAByAAAA
LAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAPkBAAD5AQAAygAAAAAAAADQ
AAAAYgAAAAEAAABAAgAAoAUAAAAAAAAVAAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNU
QlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJl
bGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAANAAAAU2hlbGxWaWV3
Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhp
bmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: SpaceBreakdown name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAIgrAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAAAAAABgIFAFBvaW50AAAAACEDAABNBAAA
BwIAAAAAAAAAAAAAAAAAAKABAAAGBwwAQm9yZGVyTGF5b3V0AAAAAAEAAAABAAAAAAAAAAAAAAAA
AAAAAAAAAJoBAAAAAAAAmgAAAAAAAADAAQAAUgAAAA0AAABDb250YWluZXJWaWV3YgAAAA8AAAAA
AAAAoAEAAGIAAAACAAAAggAAAAQAAAAAAABEAQACAEACAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAA
AAAAAAAAQAIAACICAAAAAAAAAQAAAAEAAAAAAAAAmgEAAAAAAABQAgAAYgAAAA8AAAAAAAAAQAIA
AGIAAAACAAAAggAAAAQAAAAAAABEAQACALACAAAAAAAABgELAFN5c3RlbUNvbG9yAAAAAB8AAAAA
AAAABwAAAAAAAAAAAAAAAAAAALACAAAGAQ0ARnJhbWluZ0xheW91dAAAAADqAAAAAAAAAPAAAABi
AAAACAAAAJoBAAAAAAAAUAIAAGIAAAAPAAAAAAAAALACAABiAAAAAgAAAIIAAAAEAAAAAAAARAEA
AgBQAwAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAFADAAAGBAoAR3JpZExheW91dAAAAAAD
AAAADQAAAAEAAAABAAAA6gAAAAAAAAAAAQAAYgAAAAAAAAAAAAAABgEPAE1lc3NhZ2VTZXF1ZW5j
ZQAAAADKAAAAAAAAANAAAABiAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAAAAAAAFIAAAAQ
AAAAY3JlYXRlQXQ6ZXh0ZW50OmIAAAACAAAAAgIAAAAAAAALAAAACwAAAAICAAAAAAAA/QIAACkA
AABQAwAABgEPAFdJTkRPV1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////
//////////8FAAAABQAAAIMBAAAZAAAAygAAAAAAAADQAAAAYgAAAAMAAACaAQAAAAAAAFACAABi
AAAADwAAAAAAAABQAwAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIA0AQAAAAAAAAAAAAAAAAAAAcA
AAAAAAAAAAAAAAAAAADQBAAAIgIAAAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAJoBAAAAAAAAmgAA
AAAAAADAAQAAUgAAAAoAAABTdGF0aWNUZXh0YgAAABAAAAAAAAAA0AQAAGIAAAACAAAAggAAAAQA
AAACAQBEAQAAACAFAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAIAUAAAAAAACCAAAACAAA
AFEI//8AAAAABgINAE51bGxDb252ZXJ0ZXIAAAAAAAAAAAAAAAAAAAAA0gMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAgAAABIEAAAAAAAAMAQAAGIAAAACAAAAAgIAAAAAAAABAAAAAQAAAAICAAAAAAAA
ZQAAACkAAAAgBQAAEgQAAAAAAAC6AAAAAAAAAFIAAAAFAAAAdGV4dDpiAAAAAQAAAFIAAAAJAAAA
T2JqZWN0czogIAUAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////
AAAAAAAAAAAyAAAAFAAAAMoAAAAAAAAA0AAAAMADAAACAgAAAAAAAMEAAADBAAAAAAAAABMAAACa
AQAAAAAAAJoAAAAAAAAAwAEAAFIAAAAIAAAAVGV4dEVkaXRiAAAAEAAAAAAAAADQBAAAYgAAAAIA
AACCAAAABAAAAAIgAUQBAAQAsAYAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACwBgAAAAAA
AIIAAAAIAAAA6wT//wAAAAAGAw0ASW50ZWdlclRvVGV4dAAAAAAAAAAAUgAAAAAAAAAAAAAAAwAA
ANIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAMAAAASBAAAAAAAADAEAABiAAAAAgAAAAICAAAAAAAA
ZQAAAAEAAAACAgAAAAAAAJsAAAApAAAAsAYAABIEAAAAAAAAugAAAAAAAABSAAAADwAAAHNlbGVj
dGlvblJhbmdlOmIAAAABAAAABgMIAEludGVydmFsAAAAAAMAAAABAAAAAwAAALAGAAASBAAAAAAA
ALoAAAAAAAAAUgAAAA8AAABpc1RleHRNb2RpZmllZDpiAAAAAQAAACAAAACwBgAAggQAAAAAAABy
AAAALAAAACwAAAAAAAAAAQAAAP////////////////////8yAAAAAAAAAH8AAAAUAAAAygAAAAAA
AADQAAAAwAMAAKAGAAAAAAAAEwAAAOoAAAAAAAAAAAEAAGIAAAACAAAAsAYAAFIAAAAMAAAAVG90
YWxPYmplY3RzAAAAANIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAASBAAAAAAAADAEAABiAAAA
AgAAAAICAAAAAAAAAQAAAAEAAAACAgAAAAAAAP8AAAApAAAA0AQAAIIEAAAAAAAAcgAAACwAAAAs
AAAAAAAAAAEAAAD/////////////////////AAAAAAAAAAB/AAAAFAAAAMoAAAAAAAAA0AAAAGIA
AAACAAAAIAUAALAGAACgBgAAAAAAABMAAACaAQAAAAAAAFACAABiAAAADwAAAAAAAABQAwAAYgAA
AAIAAACCAAAABAAAAAAAAEQBAAIAcAkAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAABwCQAA
IgIAAAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAJoBAAAAAAAAMAUAAGIAAAAQAAAAAAAAAHAJAABi
AAAAAgAAAIIAAAAEAAAAAgEARAEAAADACQAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAMAJ
AAAAAAAAggAAAAgAAABRCP//AAAAAJIFAAAAAAAAAAAAAAAAAAAAAAAA0gMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAgAAABIEAAAAAAAAMAQAAGIAAAACAAAAAgIAAAAAAAABAAAAAQAAAAICAAAAAAAA
UQAAACkAAADACQAAEgQAAAAAAAAwBgAAYgAAAAEAAABSAAAABgAAAFNpemU6IMAJAACCBAAAAAAA
AHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAAAAAAKAAAABQAAADKAAAA
AAAAANAAAADAAwAAoAYAAAAAAAATAAAAmgEAAAAAAADABgAAYgAAABAAAAAAAAAAcAkAAGIAAAAC
AAAAggAAAAQAAAACIAFEAQAEAPAKAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA8AoAAAAA
AACCAAAACAAAAOsE//8AAAAAIgcAAAAAAAAAAAAAUgAAAAAAAAAAAAAAAwAAANIDAAAAAAAAygAA
AAAAAADQAAAAYgAAAAMAAAASBAAAAAAAADAEAABiAAAAAgAAAAICAAAAAAAAUQAAAAEAAAACAgAA
AAAAAK8AAAApAAAA8AoAABIEAAAAAAAA0AcAAGIAAAABAAAAAggAAAAAAAADAAAAAQAAAAMAAADw
CgAAEgQAAAAAAAAwCAAAYgAAAAEAAAAgAAAA8AoAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEA
AAD/////////////////////KAAAAAAAAAB/AAAAFAAAAMoAAAAAAAAA0AAAAMADAACgBgAAAAAA
ABMAAADqAAAAAAAAAAABAABiAAAAAgAAAPAKAABSAAAACQAAAFRvdGFsU2l6ZQAAAADSAwAAAAAA
AMoAAAAAAAAA0AAAAGIAAAABAAAAEgQAAAAAAAAwBAAAYgAAAAIAAAACAgAAAAAAAP8AAAABAAAA
AgIAAAAAAAD/AAAAKQAAAHAJAACCBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////
/////////38AAAAAAAAA/gAAABQAAADKAAAAAAAAANAAAABiAAAAAgAAAMAJAADwCgAAoAYAAAAA
AAATAAAAmgEAAAAAAABQAgAAYgAAAA8AAAAAAAAAUAMAAGIAAAACAAAAggAAAAQAAAAAAABEAQAC
ADANAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAMA0AACICAAAAAAAAAQAAAAEAAAAAAAAA
AAAAAAAAAACaAQAAAAAAADAFAABiAAAAEAAAAAAAAAAwDQAAYgAAAAIAAACCAAAABAAAAAIBAEQB
AAAAgA0AAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACADQAAAAAAAIIAAAAIAAAAUQj//wAA
AACSBQAAAAAAAAAAAAAAAAAAAAAAANIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAIAAAASBAAAAAAA
ADAEAABiAAAAAgAAAAICAAAAAAAAAQAAAAEAAAACAgAAAAAAAHkAAAApAAAAgA0AABIEAAAAAAAA
MAYAAGIAAAABAAAAUgAAAA0AAABGaW5hbGlzYWJsZToggA0AAIIEAAAAAAAAcgAAACwAAAAsAAAA
AAAAAAEAAAD/////////////////////AAAAAAAAAAA8AAAAFAAAAMoAAAAAAAAA0AAAAMADAACg
BgAAAAAAABMAAACaAQAAAAAAAMAGAABiAAAAEAAAAAAAAAAwDQAAYgAAAAIAAACCAAAABAAAAAIg
AUQBAAQAsA4AAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACwDgAAAAAAAIIAAAAIAAAA6wT/
/wAAAAAiBwAAAAAAAAAAAABSAAAAAAAAAAAAAAADAAAA0gMAAAAAAADKAAAAAAAAANAAAABiAAAA
AwAAABIEAAAAAAAAMAQAAGIAAAACAAAAAgIAAAAAAAB5AAAAAQAAAAICAAAAAAAAiQAAACkAAACw
DgAAEgQAAAAAAADQBwAAYgAAAAEAAAACCAAAAAAAAAMAAAABAAAAAwAAALAOAAASBAAAAAAAADAI
AABiAAAAAQAAACAAAACwDgAAggQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////////
//////88AAAAAAAAAIAAAAAUAAAAygAAAAAAAADQAAAAwAMAAKAGAAAAAAAAEwAAAOoAAAAAAAAA
AAEAAGIAAAACAAAAsA4AAFIAAAAQAAAAVG90YWxGaW5hbGl6YWJsZQAAAADSAwAAAAAAAMoAAAAA
AAAA0AAAAGIAAAABAAAAEgQAAAAAAAAwBAAAYgAAAAIAAAACAgAAAAAAAP0BAAABAAAAAgIAAAAA
AAABAQAAKQAAADANAACCBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////////////
//4AAAAAAAAAfgEAABQAAADKAAAAAAAAANAAAABiAAAAAgAAAIANAACwDgAAoAYAAAAAAAATAAAA
oAYAAAAAAAATAAAARggSAAEAAABGcmFtaW5nQ29uc3RyYWludHMAAAAAugAAAAAAAABSAAAADwAA
AGZpeGVkUGFyZW50TGVmdAsAAAC6AAAAAAAAAFIAAAAQAAAAZml4ZWRQYXJlbnRSaWdodPf///+6
AAAAAAAAAFIAAAAOAAAAZml4ZWRQYXJlbnRUb3ALAAAAugAAAAAAAABSAAAADAAAAGZpeGVkVmll
d1RvcCkAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAAKAAAAUHVzaEJ1dHRvbmIAAAARAAAAAAAA
ALACAABiAAAAAgAAAIIAAAAEAAAAACABRAEAAACQEQAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAA
AAAAAJARAAAAAAAAggAAAAgAAABjCP//AAAAAEYFEgAEAAAAQ29tbWFuZERlc2NyaXB0aW9uAAAA
ALoAAAAAAAAAUgAAAAYAAABjYW5jZWxSAAAABgAAAENhbmNlbAEAAAABAAAAAAAAAAAAAAABAAAA
0gMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAABIEAAAAAAAAMAQAAGIAAAACAAAAAgIAAAAAAABd
AgAARwAAAAICAAAAAAAAoQAAADMAAACQEQAAEgQAAAAAAAC6AAAAAAAAAFIAAAAKAAAAaXNFbmFi
bGVkOmIAAAABAAAAIAAAAJARAAASBAAAAAAAADAGAABiAAAAAQAAAFIAAAAGAAAAQ2FuY2VskBEA
AIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////LgEAACMAAAB+AQAA
PAAAAMoAAAAAAAAA0AAAAMADAACgBgAAAAAAABMAAADyEAAAAAAAALoAAAAAAAAAUgAAABIAAABm
aXhlZFByZXZpb3VzUmlnaHQLAAAAugAAAAAAAABSAAAADQAAAGZpeGVkVmlld0xlZnShAAAAugAA
AAAAAABSAAAADwAAAGZpeGVkVmlld0JvdHRvbc////+6AAAAAAAAAFIAAAARAAAAZml4ZWRQYXJl
bnRCb3R0b233////mgEAAAAAAACgEQAAYgAAABEAAAAAAAAAsAIAAGIAAAACAAAAggAAAAQAAAAA
IAFEAQAAAPATAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA8BMAAAAAAACCAAAACAAAAGMI
//8AAAAAAhIAAAAAAAC6AAAAAAAAAFIAAAAHAAAAcmVmcmVzaFIAAAAHAAAAUmVmcmVzaAEAAAAB
AAAAAAAAAAAAAAADAAAA0gMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAABIEAAAAAAAAMAQAAGIA
AAACAAAAAgIAAAAAAACzAQAARwAAAAICAAAAAAAAoQAAADMAAADwEwAAEgQAAAAAAADQEgAAYgAA
AAEAAAAgAAAA8BMAABIEAAAAAAAAMAYAAGIAAAABAAAAUgAAAAcAAABSZWZyZXNo8BMAAIIEAAAA
AAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////2QAAACMAAAApAQAAPAAAAMoA
AAAAAAAA0AAAAMADAACgBgAAAAAAABMAAADyEAAAAAAAAHATAAAVAAAAkBMAAKEAAACwEwAAz///
/9ATAAD3////mgEAAAAAAACaAAAAAAAAAMABAABSAAAACwAAAFByb2dyZXNzQmFyYgAAAA8AAAAA
AAAAsAIAAGIAAAACAAAAggAAAAQAAAAAAABEAQAAAAYACgBEZWFmT2JqZWN0AAAAAEYECwACAAAA
VmFsdWVIb2xkZXIAAAAAAAAAAAAAAAAOAhEAU1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAABS
AAAABwAAAERvbHBoaW5SAAAADAAAAFNlYXJjaFBvbGljeboAAAAAAAAAUgAAAAUAAABuZXZlcgEA
AAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACAFQAAAAAAAIIAAAAIAAAA0QT//wAAAACSBQAAAAAA
AAAAAAAAAAAA0gMAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAABIEAAAAAAAAMAQAAGIAAAACAAAA
AgIAAAAAAAALAAAAUQAAAAICAAAAAAAAlQEAAB8AAACAFQAAEgQAAAAAAAC6AAAAAAAAAFIAAAAG
AAAAcmFuZ2U6YgAAAAEAAAACCAAAAAAAAAEAAADJAAAAAwAAAIAVAACCBAAAAAAAAHIAAAAsAAAA
LAAAAAAAAAABAAAA/////////////////////wUAAAAoAAAAzwAAADcAAADKAAAAAAAAANAAAADA
AwAAoAYAAAAAAAATAAAA8hAAAAAAAAAQEQAACwAAADARAACP/v//sBMAAOP////QEwAA7f///+oA
AAAAAAAAAAEAAGIAAAACAAAAgBUAAFIAAAAIAAAAUHJvZ3Jlc3MGAgkAUmVjdGFuZ2xlAAAAAAIC
AAAAAAAAAQAAAAEAAAACAgAAAAAAAAEAAAABAAAA0gMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAA
ABIEAAAAAAAAMAQAAGIAAAACAAAAAgIAAAAAAAABAAAAkwMAAAICAAAAAAAAEQMAAIMAAACwAgAA
ggQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAyQEAAIgBAAAK
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
AAAAAAAAAADSAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAADAAAAEgQAAAAAAAAwBAAAYgAAAAIAAAAC
AgAAAAAAAAsAAAALAAAAAgIAAAAAAAAhAwAATQQAAKABAAASBAAAAAAAADAGAABiAAAAAQAAAFIA
AAAPAAAAU3BhY2UgQnJlYWtkb3duoAEAABIEAAAAAAAAugAAAAAAAABSAAAACAAAAG1lbnVCYXI6
YgAAAAEAAABwIQAAoAEAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAAAAAD/////////////////
////BQAAAAUAAACVAQAAKwIAAMoAAAAAAAAA0AAAAGIAAAABAAAAQAIAAKAGAAAAAAAAFQAAAEYF
BAADAAAASWNvbgAAAAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIA
AAAHAAAARG9scGhpblIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAA
BwAAAGN1cnJlbnRSAAAADQAAAFNoZWxsVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxp
YnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

(ResourceIdentifier class: SpaceBreakdown name: 'Read-only view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
YgAAAAEAAACCAwAAAAAAAKADAABiAAAAAgAAAAICAAAAAAAACwAAAAsAAAACAgAAAAAAAP8AAAAp
AAAAQAQAAPIDAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////BQAAAAUA
AACEAAAAGQAAAMoAAAAAAAAA0AAAAGIAAAACAAAAkAQAACAGAAAQBgAAAAAAABMAAACaAQAAAAAA
AFACAABiAAAADwAAAAAAAABAAgAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIA4AgAAAAAAAAAAAAA
AAAAAAcAAAAAAAAAAAAAAAAAAADgCAAAIgIAAAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAJoBAAAA
AAAAoAQAAGIAAAAQAAAAAAAAAOAIAABiAAAAAgAAAIIAAAAEAAAAAgEARAEAAAAwCQAAAAAAAAAA
AAAAAAAABwAAAAAAAAAAAAAAAAAAADAJAAAAAAAAggAAAAgAAABRCP//AAAAAAIFAAAAAAAAAAAA
AAAAAAAAAAAAQgMAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAAIIDAAAAAAAAoAMAAGIAAAACAAAA
AgIAAAAAAAABAAAAAQAAAAICAAAAAAAAUQAAACkAAAAwCQAAggMAAAAAAACgBQAAYgAAAAEAAABS
AAAABgAAAFNpemU6IDAJAADyAwAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////////
/////wAAAAAAAAAAKAAAABQAAADKAAAAAAAAANAAAADwAgAAEAYAAAAAAAATAAAAmgEAAAAAAAAw
BgAAYgAAABAAAAAAAAAA4AgAAGIAAAACAAAAggAAAAQAAAACIAFEAQAEAGAKAAAAAAAAAAAAAAAA
AAAHAAAAAAAAAAAAAAAAAAAAYAoAAAAAAACCAAAACAAAAOsE//8AAAAAkgYAAAAAAAAAAAAAUgAA
AAAAAAAAAAAAAwAAAEIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAMAAACCAwAAAAAAAKADAABiAAAA
AgAAAAICAAAAAAAAUQAAAAEAAAACAgAAAAAAAK8AAAApAAAAYAoAAIIDAAAAAAAAQAcAAGIAAAAB
AAAAcgcAAAAAAAADAAAAAQAAAAMAAABgCgAAggMAAAAAAACgBwAAYgAAAAEAAAAgAAAAYAoAAPID
AAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////KAAAAAAAAAB/AAAAFAAA
AMoAAAAAAAAA0AAAAPACAAAQBgAAAAAAABMAAADqAAAAAAAAAAABAABiAAAAAgAAAGAKAABSAAAA
CQAAAFRvdGFsU2l6ZQAAAABCAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAAggMAAAAAAACgAwAA
YgAAAAIAAAACAgAAAAAAAAkBAAALAAAAAgIAAAAAAAD/AAAAKQAAAOAIAADyAwAAAAAAAHIAAAAs
AAAALAAAAAAAAAABAAAA/////////////////////4QAAAAFAAAAAwEAABkAAADKAAAAAAAAANAA
AABiAAAAAgAAADAJAABgCgAAEAYAAAAAAAATAAAAmgEAAAAAAABQAgAAYgAAAA8AAAAAAAAAQAIA
AGIAAAACAAAAggAAAAQAAAAAAABEAQACAKAMAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA
oAwAACICAAAAAAAAAQAAAAEAAAAAAAAAAAAAAAAAAACaAQAAAAAAAKAEAABiAAAAEAAAAAAAAACg
DAAAYgAAAAIAAACCAAAABAAAAAIBAEQBAAAA8AwAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAA
AADwDAAAAAAAAIIAAAAIAAAAUQj//wAAAAACBQAAAAAAAAAAAAAAAAAAAAAAAEIDAAAAAAAAygAA
AAAAAADQAAAAYgAAAAIAAACCAwAAAAAAAKADAABiAAAAAgAAAAICAAAAAAAAAQAAAAEAAAACAgAA
AAAAAHkAAAApAAAA8AwAAIIDAAAAAAAAoAUAAGIAAAABAAAAUgAAAA0AAABGaW5hbGlzYWJsZTog
8AwAAPIDAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAAAAAAAAA8
AAAAFAAAAMoAAAAAAAAA0AAAAPACAAAQBgAAAAAAABMAAACaAQAAAAAAADAGAABiAAAAEAAAAAAA
AACgDAAAYgAAAAIAAACCAAAABAAAAAIgAUQBAAQAIA4AAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAA
AAAAAAAgDgAAAAAAAIIAAAAIAAAA6wT//wAAAACSBgAAAAAAAAAAAABSAAAAAAAAAAAAAAADAAAA
QgMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAAIIDAAAAAAAAoAMAAGIAAAACAAAAAgIAAAAAAAB5
AAAAAQAAAAICAAAAAAAAiQAAACkAAAAgDgAAggMAAAAAAABABwAAYgAAAAEAAAByBwAAAAAAAAMA
AAABAAAAAwAAACAOAACCAwAAAAAAAKAHAABiAAAAAQAAACAAAAAgDgAA8gMAAAAAAAByAAAALAAA
ACwAAAAAAAAAAQAAAP////////////////////88AAAAAAAAAIAAAAAUAAAAygAAAAAAAADQAAAA
8AIAABAGAAAAAAAAEwAAAOoAAAAAAAAAAAEAAGIAAAACAAAAIA4AAFIAAAAQAAAAVG90YWxGaW5h
bGl6YWJsZQAAAABCAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAAggMAAAAAAACgAwAAYgAAAAIA
AAACAgAAAAAAAAcCAAALAAAAAgIAAAAAAAABAQAAKQAAAKAMAADyAwAAAAAAAHIAAAAsAAAALAAA
AAAAAAABAAAA/////////////////////wMBAAAFAAAAgwEAABkAAADKAAAAAAAAANAAAABiAAAA
AgAAAPAMAAAgDgAAEAYAAAAAAAATAAAAEAYAAAAAAAATAAAAAAAAAAAAAACaAQAAAAAAAFACAABi
AAAADwAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAYBAAAAAAAAAAAAAAAAAAAAcA
AAAAAAAAAAAAAAAAAABgEAAAIgIAAAAAAAABAAAAAQAAAAAAAAAAAAAAAAAAAAAAAACaAQAAAAAA
AJoAAAAAAAAAUgAAABcAAABEb2xwaGluIENvbW1vbiBDb250cm9sc1IAAAAZAAAATXVsdGlwbGVT
ZWxlY3Rpb25MaXN0Vmlld2IAAAAeAAAAAAAAAGAQAABiAAAAAgAAAIIAAAAEAAAASRABRAEEAACw
EAAARgMJAAIAAABMaXN0TW9kZWwAAAAAygAAAAAAAADQAAAA8AIAAAAAAAAOAhEAU1RCU2luZ2xl
dG9uUHJveHkAAAAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5SAAAADAAAAFNlYXJjaFBvbGljeboA
AAAAAAAAUgAAAAgAAABpZGVudGl0eQAAAAAAAAAABwAAAEYFBAACAAAATWVudQAAAAAAAAAAEAAA
AGIAAAAFAAAARgQPAAIAAABDb21tYW5kTWVudUl0ZW0AAAAAAQAAAEYFEgAEAAAAQ29tbWFuZERl
c2NyaXB0aW9uAAAAALoAAAAAAAAAUgAAABgAAABicmVha2Rvd25DbGFzc1JlZmVyZW5jZXNSAAAA
FQAAACZCcmVha2Rvd24gcmVmZXJlbmNlc6UgAAABAAAAAAAAAAAAAAAAAAAA8hEAAAAAAAABAAAA
EhIAAAAAAAC6AAAAAAAAAFIAAAALAAAAYnJvd3NlQ2xhc3NSAAAADQAAAEJyb3dzZSAmY2xhc3OF
IAAAAQAAAAAAAAAAAAAAAAAAAPIRAAAAAAAAAQAAABISAAAAAAAAugAAAAAAAABSAAAAEAAAAGlu
c3BlY3RJbnN0YW5jZXNSAAAAEgAAACZJbnNwZWN0IGluc3RhbmNlc5MgAAABAAAAAAAAAAAAAAAA
AAAARgEPAAEAAABEaXZpZGVyTWVudUl0ZW0AAAAAARAAAPIRAAAAAAAAAQAAABISAAAAAAAAugAA
AAAAAABSAAAAFQAAAGxhdW5jaFJlZmVyZW5jZUZpbmRlclIAAAAZAAAAU3RhcnQgJ1JlZmVyZW5j
ZSAmZmluZGVyJ40kAAABAAAAAAAAAAAAAAAAAAAAUgAAAAAAAAAAAAAAAAAAAAAAAACwEAAAAAAA
AIIAAAAIAAAAcQL//wAAAACaAAAAAAAAAMABAABSAAAAEQAAAEJhc2ljTGlzdEFic3RyYWN0AAAA
AFoRAAAAAAAAmgAAAAAAAADAAQAAUgAAABAAAABJY29uSW1hZ2VNYW5hZ2VyugAAAAAAAABSAAAA
BwAAAGN1cnJlbnQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADKAAAAAAAAANAAAABiAAAABAAAAEYM
DgAFAAAATGlzdFZpZXdDb2x1bW4AAAAAUgAAAAoAAABDbGFzcyBuYW1lbwEAALoAAAAAAAAAUgAA
AAQAAABsZWZ0kBMAAJoAAAAAAAAAgBEAAFIAAAAQAAAAU29ydGVkQ29sbGVjdGlvbgYCBwBNZXNz
YWdlAAAAALoAAAAAAAAAUgAAAAkAAABjbGFzc05hbWViAAAAAAAAAAAAAACwEAAAAAAAAAMAAAAA
AAAAAAAAACIUAAAAAAAAUgAAAAkAAABJbnN0YW5jZXONAAAAugAAAAAAAABSAAAABQAAAHJpZ2h0
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
50QAAOAZAAAyHwAAAAAAAKUgAABgGgAAMh8AAAAAAACFIAAAkBoAADIfAAAAAAAAkyAAAMAaAAAy
HwAAAAAAAI0kAAAAGwAAMh8AAAAAAACbIAAAYBsAADIfAAAAAAAAqSAAAFAcAAAyHwAAAAAAAOEA
AACQHQAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAQgMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAwAAAIIDAAAAAAAAoAMAAGIAAAACAAAAAgIAAAAAAAALAAAACwAAAAICAAAAAAAA
IQMAAE0EAACgAQAAggMAAAAAAACgBQAAYgAAAAEAAABSAAAADwAAAFNwYWNlIEJyZWFrZG93bqAB
AACCAwAAAAAAALoAAAAAAAAAUgAAAAgAAABtZW51QmFyOmIAAAABAAAAkBkAAKABAADyAwAAAAAA
AHIAAAAsAAAALAAAAAAAAAAAAAAA/////////////////////wUAAAAFAAAAlQEAACsCAADKAAAA
AAAAANAAAABiAAAAAgAAAGAQAABAAgAAEAYAAAAAAAAVAAAARgUEAAMAAABJY29uAAAAAAAAAAAQ
AAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgA
AABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAANAAAA
U2hlbGxWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQ
AAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

