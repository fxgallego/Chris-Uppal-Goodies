| package |
package := Package name: 'CU Quick Deploy'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A simple GUI tool to provide nearly-one-click-deployment.

This tool is intended to help make deployment from a clean image (which is the only sensible way of doing deployment, in my opinion) simple and convenient.  The easiest way to describe what it does is to say how to use it to best effect.

First, set up a clean image that you will use for deployment.  Don''t load any of your own packages into it, just have the basic Dolphin stuff (I remove all the sample packages, and as much of the RB as I can) since none of that will be needed in the clean image).  You may find it convenint to add any third party libraries that you include in your deployed applicatins.  *Don''t* add any IDE extensions to the image !!  Turn off the option to save the image automatically before deployment.  That is your clean image to use for deployment.

Now load this package, which will install ''Quick Deploy'' as an ''Additional Tool''.  Show the tool and close/minimise all the other Dolphin windows.  Exit Dolphin by selecting ''close'' from the taksbar, allow it to save the image.  Make a copy of the image so that you don''t have to go through the above palaver again when you next want a clean image.  Set up a shortcut of some kind that allows you to start Dolphin on this deployment image (I add it to the system menu, you may prefer to set up a desktop shortcut, or something else).  The result of all this is that you can now launch the deployment image with (more or less) one click, and that it will come up straight into the Quick Deploy tool.  Don''t try that yet, though, because we haven''t told QuickDeploy what packages we may want to deploy from.

Now install Quick Deploy into your normal working image too.  From here you can launch the tool, it will display a list of package (initially empty) that can be deployed.  Use the ''Add'' button to add whatever packages you are likely to be wanting to deploy over the next few months (for me that''s all my deployable packages -- around ten of them).  You *can* use the tool to deploy the packages directly from your working image, but I don''t recommend doing that (just because I recomend you never deploy from a "dirty" image).  What''s important here is that the package list has been written to the registry where other invocations of the same tool will find it.

Now to attempt a nearly-one-click-deployment.

Still in your working image, ensure that the package you are interested in has been saved, and that the deployment settings (as set by the Lagoon Wizard) are all as you wish them to be and have been saved with it.  (Having to remember to *save* packages before deploying them is the only downside of deploying from a clean image)  For me, the normal state of any package is that it has been saved, so that doesn''t count as a click ;-)

Now start your deployment image.  (Don''t bother leaving your working image, there''s no need to disrupt your work by doing so.)  That''s "one" click. That will bring up the deployment instance of Quick Deploy, and it will now have its package list populated (although the packages themselves won''t be loaded yet).  Select the package you are interested in (if it isn''t already) and either double-click it, or press the ''Deploy'' button.  That''s two clicks.   Quick Deploy will then load the selected package plus any prerequisites in the normal Dolphin way  (there should be better feedback at this point -- it should say that its doing something, but I haven''t bothered to implement a progress bar yet).  Then it will ask Dolphin to generate a .EXE for the package(s), which will start the normal deployment sequence.  It takes a bit of cleverness to ensure that Quick Deploy itself is not in the image as its deployed -- take a look at the code if you are interested.  Finally Dolphin will (one hopes) complete the deployment and present its ''Finished'' notification message; OK that and we''re done.  Makeing three clicks in all.

That''s *nearly* one click ;-)

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris

'.

package basicPackageVersion: '4.04'.


package classNames
	add: #QuickDeployRecord;
	add: #QuickDeployShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #QuickDeployShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	yourself).

package!

"Class Definitions"!

Object subclass: #QuickDeployRecord
	instanceVariableNames: 'name filename'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #QuickDeployShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

QuickDeployRecord guid: (GUID fromString: '{701DAEFC-EBFA-4E2A-9635-D1FE66FF7891}')!
QuickDeployRecord comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

One of these represent one deployable package in the QuickDeployShell tool.  The important thing here is that a package is defined (for these purposes) by the *full* (not relative) name of the .PAC file.'!
!QuickDeployRecord categoriesForClass!Unclassified! !
!QuickDeployRecord methodsFor!

canDeploy
	"answer whether our target package can be deployed"

	^ self isInstalled or: [self canInstall].!

canInstall
	"answer whether our target package can be installed"

	^ self package isNil and: [File exists: filename].!

deploy
	"deploy our target package, installing it first if necessary"

	self canDeploy ifFalse: [^ self].

	self install.

	"we queue a deferred action so that the Quick Deploy package can be unloaded
	without problems"
	SessionManager inputState queueDeferredAction: self makeDeployAction.!

filename
	"answer the (full) name of our package"

	^ filename.!

filename: aFilename
	"private -- set the filename that we will attempt to load packages from"

	filename := aFilename.
	name := File splitStemFrom: filename.!

icon

	^ (self packageIfNone: [^ False icon]) icon.!

imageStripper
	"answer the image stripper that will be used to deploy our target package"

	^ (self packageIfNone: [^ nil]) imageStripper.
!

initialize
	"private -- establish a coherent initial state"

	name := filename := '<undefined>'.!

install
	"install our target package"

	self canInstall ifFalse: [^ self].

	Package manager install: filename.!

isInstalled
	"answer whether our target package is installed"

	^ self package notNil.!

makeDeployAction
	"private -- answer a <niladicValuable> which when evaluated will cause
	our taget package to be deployed.
	NB: we must be careful to answer an object that will not prevent this
	package from being uninstalled, and which will work correctly without
	any code from this package"

	^ MessageSend receiver: self imageStripper selector: #stripAndSaveWithProgress.
!

name
	"answer the (short) name of our package"

	^ name.!

package
	"answer our target package or nil if it isn't installed"

	^ self packageIfNone: [].!

packageIfNone: a0Block
	"answer our target package or the result of evaluating a0Block if it isn't installed"

	^ Package manager packageNamed: name ifNone: a0Block.!

packageVersion

	^ (self packageIfNone: [^ '']) packageVersion.!

resourceVersion

	^ self imageStripper
		ifNil: ['']
		ifNotNil: [:it | it versionResource productVersion].
! !
!QuickDeployRecord categoriesFor: #canDeploy!accessing!initializing!public!testing! !
!QuickDeployRecord categoriesFor: #canInstall!accessing!initializing!public!testing! !
!QuickDeployRecord categoriesFor: #deploy!operations!public! !
!QuickDeployRecord categoriesFor: #filename!accessing!public! !
!QuickDeployRecord categoriesFor: #filename:!initializing!private! !
!QuickDeployRecord categoriesFor: #icon!accessing!initializing!public! !
!QuickDeployRecord categoriesFor: #imageStripper!accessing!initializing!public! !
!QuickDeployRecord categoriesFor: #initialize!initializing!private! !
!QuickDeployRecord categoriesFor: #install!operations!public! !
!QuickDeployRecord categoriesFor: #isInstalled!accessing!initializing!public!testing! !
!QuickDeployRecord categoriesFor: #makeDeployAction!helpers!private! !
!QuickDeployRecord categoriesFor: #name!accessing!public! !
!QuickDeployRecord categoriesFor: #package!accessing!initializing!public! !
!QuickDeployRecord categoriesFor: #packageIfNone:!accessing!initializing!public! !
!QuickDeployRecord categoriesFor: #packageVersion!accessing!initializing!public! !
!QuickDeployRecord categoriesFor: #resourceVersion!accessing!initializing!public! !

!QuickDeployRecord class methodsFor!

filename: aString
	"answer a new instance corresponding to the given package file"

	^ (self new)
		filename: aString;
		yourself.!

forPackage: aPackage
	"answer a new instance corresponding to the given package"

	^ self filename: aPackage packageFileName.!

new
	"private -- use #filename:: or #forPackage"

	^ (self basicNew)
		initialize;
		yourself.! !
!QuickDeployRecord class categoriesFor: #filename:!instance creation!public! !
!QuickDeployRecord class categoriesFor: #forPackage:!instance creation!public! !
!QuickDeployRecord class categoriesFor: #new!instance creation!private! !

QuickDeployShell guid: (GUID fromString: '{D58820CF-B5B2-4CB5-875A-C23AE91FC98F}')!
QuickDeployShell comment: 'Copyright © Chris Uppal, 2004.
chris.uppal@metagnostic.org

A simple GUI tool to provide nearly-one-click-deployment.  See the package comment for details of how to use it.
'!
!QuickDeployShell categoriesForClass!Unclassified! !
!QuickDeployShell methodsFor!

add
	"command -- prompt for the filename of a package to add to the list"

	| filename |

	self canAdd ifFalse: [^ self].

	filename := FileOpenDialog new
			fileTypes: self packageFileDialogTypes;
			defaultExtension: self packageExtension;
			caption: 'Choose package file';
			showModal.

	filename isNil ifTrue: [^ self].

	self addRecord: (QuickDeployRecord filename: filename).

	self updateRegistry.

!

addPackages: aList
	"add (record corresponding to) all the package in aList to our list"

	self addRecords: (aList collect: [:each | QuickDeployRecord forPackage: each]).!

addRecord: aRecord
	"add a QuickDeployRecord (for some package) to our list"

	self model add: aRecord.!

addRecords: aList

	self model addAll: aList.!

canAdd
	"private -- answer true iff we may perform the #add command ?"

	^ true.
!

canDeploy
	"private -- answer true iff we may perform the #deploy command ?"

	^ self selectedRecord
		ifNil: [false]
		ifNotNil: [:it | it canDeploy].!

canInstall
	"private -- answer true iff we may perform the #install command ?"

	^ self selectedRecord
		ifNil: [false]
		ifNotNil: [:it | it canInstall].
!

canMoveDown
	"private -- answer true iff we may perform the #moveDown command ?"

	^ self selectedRecordIndex
		between: 1
		and: self model size - 1.
!

canMoveFirst
	"private -- answer true iff we may perform the #moveFirst command ?"

	^ self canMoveUp.
!

canMoveLast
	"private -- answer true iff we may perform the #moveLast command ?"

	^ self canMoveDown.!

canMoveUp
	"private -- answer true iff we may perform the #moveUp command ?"

	^ self selectedRecordIndex
		between: 2
		and: self model size.
!

canRemove
	"private -- answer true iff we may perform the #remove command ?"

	^ self hasSelectedRecord.
!

contentsOfDragSession: aSession
	"private -- answer the interesting Objects from aSession, if any"

	^ (((aSession dragObjects select: [:each | each isFormatAvailable: #Object])
		collect: [:each | each object]))
			select: [:each | each isKindOf: Package].
!

createComponents
	"create sub-presenters that they may be linked into MVP triads"

	self add: ListPresenter new name: 'PackageList'.

	^ super createComponents.
!

createSchematicWiring
	"set up triggers between our components"

	self packageListPresenter
		when: #selectionChanged send: #updateStatus to: self;
		when: #actionPerformed send: #deploy to: self;
		when: #dragOver: send: #onDragOverPackages: to: self;
		when: #drop: send: #onDropOverPackages: to: self.

	^ super createSchematicWiring.

!

deploy
	"command -- deploy the currently selected package"

	| item |

	self canDeploy ifFalse: [^ self].

	item := self selectedRecord.

	item install.

	self updateRegistryWith: item.

	self view close.

	item deploy.!

hasSelectedRecord
	"answer wether we have a selected record (package)"

	^ self packageListPresenter hasSelection.!

install
	"command -- install the selected package"

	self canInstall ifFalse: [^ self].

	self selectedRecord install.

	self packageListPresenter view invalidate.!

model: aListModel
	"private -- set the model for this Presenter"

	super model: aListModel.

	self packageListPresenter model: aListModel.

	self statusTextView model: nil asValue.
!

moveDown
	"command -- move the selected item down one in the list"

	| index |

	self canMoveDown ifFalse: [^ self].

	index := self selectedRecordIndex.
	self model swap: index with: index + 1.

	self selectedRecordIndex: index + 1.

	self updateRegistry.!

moveFirst
	"command -- move the selected item first in the list"

	| item |

	self canMoveFirst ifFalse: [^ self].

	item := self selectedRecord.
	(self model)
		remove: item;
		addFirst: item.

	self selectedRecord: item.

	self updateRegistry.!

moveLast
	"command -- move the current item last in the list"

	| item |

	self canMoveLast ifFalse: [^ self].

	item := self selectedRecord.
	(self model)
		remove: item;
		addLast: item.

	self selectedRecord: item.

	self updateRegistry.
!

moveUp
	"command -- move the selected item up in the list"

	| index |

	self canMoveUp ifFalse: [^ self].

	index := self selectedRecordIndex.
	self model swap: index with: index - 1.

	self selectedRecordIndex: index - 1.

	self updateRegistry.!

onDragOverPackages: aSession
	"private -- a d&d session is passing over the filters pane.
	Update the session accordingly"

	| op |

	(self contentsOfDragSession: aSession) isEmpty ifTrue: [^ self].

	op := aSession intendedOperation.
	op == #move ifTrue: [op := #copy].	"interpret move as copy"
	aSession operation: op.


!

onDropOverPackages: aSession
	"private -- a d&d session is attempting to drop on the package pane.
	Update the session accordingly"

	| op |

	op := aSession operation.

	"we never do anything that should cause the source to 'cut' what was dropped"
	aSession operation: nil.

	"could possably accept #link too -- interpreting it as #remove"
	op == #copy ifFalse: [^ self].

	"note that we add objects in their capacities as #Objects.  We don't care what
	they are as long as they know how to supply a filter block"
	self addPackages: (self contentsOfDragSession: aSession).
!

onViewClosed

	"just to make sure"
	SessionManager current removeEventsTriggeredFor: self.

	super onViewClosed.
!

onViewOpened

	super onViewOpened.

	self updateFromRegistry.

	SessionManager current when: #sessionStarted send: #updateFromRegistry to: self.!

packageExtension
	"private -- answer the default file extension for packages"

	^ 'pac'.!

packageFileDialogTypes
	"private -- answer a file types array for use with a FileDialog to save/load packages"

	^ #(
		#('Package (*.pac)' '*.pac')
		#('Multi-file Package (*.pax)' '*.pax')
		#('All Files (*.*)' '*.*')
	).!

packageListPresenter
	"private -- answer the presenter named 'PackageList'"

	^ self presenterNamed: 'PackageList'.
!

queryCommand: aCommandQuery
	"the usual..."

	super queryCommand: aCommandQuery.

	aCommandQuery command == #add ifTrue: [^ aCommandQuery isEnabled: self canAdd].
	aCommandQuery command == #remove ifTrue: [^ aCommandQuery isEnabled: self canRemove].

	aCommandQuery command == #moveFirst ifTrue: [^ aCommandQuery isEnabled: self canMoveFirst].
	aCommandQuery command == #moveLast ifTrue: [^ aCommandQuery isEnabled: self canMoveLast].
	aCommandQuery command == #moveUp ifTrue: [^ aCommandQuery isEnabled: self canMoveUp].
	aCommandQuery command == #moveDown ifTrue: [^ aCommandQuery isEnabled: self canMoveDown].

	aCommandQuery command == #install ifTrue: [^ aCommandQuery isEnabled: self canInstall].
	aCommandQuery command == #deploy ifTrue: [^ aCommandQuery isEnabled: self canDeploy].
!

registryEntryRoot
	"private -- answer a RegKey representing the root of our registry entry"

	^ RegKey userRoot createKey: self registryRootName.
!

registryRootName
	"private -- answer the name of the root of our registy entry"

	^ 'Software\Metagnostic\QuickDeploy'.!

remove
	"command -- remove the selected package record"

	self canRemove ifFalse: [^ self].

	self removeRecord: self selectedRecord.

	self updateRegistry.!

removeRecord: aRecord
	"ensure that the given record (for a package) is not on our list"

	self model remove: aRecord ifAbsent: [].!

selectedRecord
	"answer the currently selected record (for a package) or nil if there is
	no selection"

	^ self packageListPresenter selectionOrNil!

selectedRecord: aRecord
	"set the currently selected record (for a package)"

	^ self packageListPresenter selectionOrNil: aRecord.!

selectedRecordIndex

	^ self packageListPresenter selectionByIndexIfNone: [0].!

selectedRecordIndex: anInteger

	self packageListPresenter selectionByIndex: anInteger.!

statusTextView

	^ self view viewNamed: 'StatusText'.!

updateFromRegistry
	"read the package list (and selection) stored in the registry, and update
	our state from it"

	| root count deployed select |

	self model removeAll.

	root := self registryEntryRoot.

	count := root valueAt: 'count' ifAbsent: [0].
	deployed := root valueAt: 'deployed' ifAbsent: [].

	1 to: count do:
		[:i || filename record |
		filename := root valueAt: ('package%03d' sprintfWith: i) ifAbsent: [^ self].
		record := QuickDeployRecord filename: filename.
		self addRecord: record.
		filename = deployed ifTrue: [select := record]].

	[self selectedRecord: select] postToInputQueue.

!

updateRegistry
	"save our current package list into the registry"

	| root |

	root := self registryEntryRoot.

	root
		valueAt: 'count'
		put: self model size.

	self model keysAndValuesDo:
		[:i :each |
		root
			valueAt: ('package%03d' sprintfWith: i)
			put: each filename].!

updateRegistryWith: aRecord
	"save the most prefered (default) deployable package into the registry"

	| root |

	root := self registryEntryRoot.

	root
		valueAt: 'deployed'
		put: aRecord filename.
!

updateStatus

	self statusTextView model: self selectedRecord asValue.! !
!QuickDeployShell categoriesFor: #add!commands!public! !
!QuickDeployShell categoriesFor: #addPackages:!helpers!public! !
!QuickDeployShell categoriesFor: #addRecord:!helpers!public! !
!QuickDeployShell categoriesFor: #addRecords:!helpers!public! !
!QuickDeployShell categoriesFor: #canAdd!commands!private! !
!QuickDeployShell categoriesFor: #canDeploy!commands!private! !
!QuickDeployShell categoriesFor: #canInstall!commands!private! !
!QuickDeployShell categoriesFor: #canMoveDown!commands!private! !
!QuickDeployShell categoriesFor: #canMoveFirst!commands!private! !
!QuickDeployShell categoriesFor: #canMoveLast!commands!private! !
!QuickDeployShell categoriesFor: #canMoveUp!commands!private! !
!QuickDeployShell categoriesFor: #canRemove!commands!private! !
!QuickDeployShell categoriesFor: #contentsOfDragSession:!helpers!private! !
!QuickDeployShell categoriesFor: #createComponents!initializing!presenters!public! !
!QuickDeployShell categoriesFor: #createSchematicWiring!event handling!initializing!public! !
!QuickDeployShell categoriesFor: #deploy!commands!public! !
!QuickDeployShell categoriesFor: #hasSelectedRecord!accessing!public! !
!QuickDeployShell categoriesFor: #install!commands!public! !
!QuickDeployShell categoriesFor: #model:!initializing!models!private! !
!QuickDeployShell categoriesFor: #moveDown!commands!public! !
!QuickDeployShell categoriesFor: #moveFirst!commands!public! !
!QuickDeployShell categoriesFor: #moveLast!commands!public! !
!QuickDeployShell categoriesFor: #moveUp!commands!public! !
!QuickDeployShell categoriesFor: #onDragOverPackages:!event handling!private! !
!QuickDeployShell categoriesFor: #onDropOverPackages:!event handling!private! !
!QuickDeployShell categoriesFor: #onViewClosed!event handling!public! !
!QuickDeployShell categoriesFor: #onViewOpened!event handling!public! !
!QuickDeployShell categoriesFor: #packageExtension!constants!private! !
!QuickDeployShell categoriesFor: #packageFileDialogTypes!constants!private! !
!QuickDeployShell categoriesFor: #packageListPresenter!private!subpresenters! !
!QuickDeployShell categoriesFor: #queryCommand:!commands!public! !
!QuickDeployShell categoriesFor: #registryEntryRoot!private!registry! !
!QuickDeployShell categoriesFor: #registryRootName!constants!private!registry! !
!QuickDeployShell categoriesFor: #remove!commands!public! !
!QuickDeployShell categoriesFor: #removeRecord:!helpers!public! !
!QuickDeployShell categoriesFor: #selectedRecord!accessing!public! !
!QuickDeployShell categoriesFor: #selectedRecord:!accessing!public! !
!QuickDeployShell categoriesFor: #selectedRecordIndex!accessing!public! !
!QuickDeployShell categoriesFor: #selectedRecordIndex:!accessing!public! !
!QuickDeployShell categoriesFor: #statusTextView!private!subpresenters! !
!QuickDeployShell categoriesFor: #updateFromRegistry!public!registry! !
!QuickDeployShell categoriesFor: #updateRegistry!public!registry! !
!QuickDeployShell categoriesFor: #updateRegistryWith:!public!registry! !
!QuickDeployShell categoriesFor: #updateStatus!helpers!private! !

!QuickDeployShell class methodsFor!

defaultModel
	"answer the initial model our instances will use"

	^ ListModel new.!

initialize
	"private -- class-side initialisation.

		self initialize
	"

	self registerAsTool.
!

registerAsTool
	"add ourself to the 'system tools'"

	| icon |

	icon := (Smalltalk at: #SmalltalkSystemIcon) show: self description: self toolName.
	(Smalltalk at: #SmalltalkSystem) current addAdditionalToolsFolderIcon: icon.
!

toolName
	"answer a name to use for this tool"

	^ 'Quick Deploy'.!

uninitialize
	"private -- class-side tear down.

		self uninitialize
	"

	self unRegisterAsTool.
!

unRegisterAsTool
	"remove ourself from the 'system tools'"

	| icon |

	icon := (Smalltalk at: #SmalltalkSystemIcon ifAbsent: [^ self]) show: self description: self toolName.
	(Smalltalk at: #SmalltalkSystem ifAbsent: [^ self]) current removeSystemFolderIcon: icon.! !
!QuickDeployShell class categoriesFor: #defaultModel!models!public! !
!QuickDeployShell class categoriesFor: #initialize!development!initializing!private! !
!QuickDeployShell class categoriesFor: #registerAsTool!development!initializing!public! !
!QuickDeployShell class categoriesFor: #toolName!constants!displaying!public! !
!QuickDeployShell class categoriesFor: #uninitialize!development!initializing!private! !
!QuickDeployShell class categoriesFor: #unRegisterAsTool!development!initializing!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: QuickDeployShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAPgdAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29sb3IAAAAAHwAAAAYC
BQBQb2ludAAAAABNBAAAWQIAAAcCAAAAAAAAAAAAAAAAAACgAQAABgcMAEJvcmRlckxheW91dAAA
AAABAAAAAQAAAAAAAACaAQAAAAAAAJoAAAAAAAAAUgAAABQAAABEb2xwaGluIENvbnRyb2wgQmFy
c1IAAAAJAAAAU3RhdHVzQmFyYgAAABIAAAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAAAECQBEAQAA
AGACAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAYEBABGb250AAAAAAAAAAAQAAAABgEHAExPR0ZPTlQA
AAAAcgAAADwAAADz////AAAAAAAAAAAAAAAAkAEAAAAAAAADAgEiQXJpYWwAnwQAhj8BAADMNT8B
AgAUOwAAAAD3AAVWbwEiAgAAAAAAAMEAAADBAAAAAAAAAGACAAAAAAAAggAAAAgAAADXAf//AAAA
AOoAAAAAAAAAAAEAAGIAAAACAAAABgcNAFN0YXR1c0Jhckl0ZW0AAAAAAQAAAP////9gAgAAAAAA
AAYCBwBNZXNzYWdlAAAAALoAAAAAAAAAUgAAAAgAAABmaWxlbmFtZWIAAAAAAAAAYAMAAA4CEQBT
VEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAMABAABSAAAAEAAAAEljb25JbWFnZU1hbmFnZXK6
AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAAKAAAAU3RhdHVzVGV4dGIAAAABAAAAcAMAAAYEEQBT
dGF0dXNCYXJOdWxsSXRlbQAAAAABAgAAAQAAAGACAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVu
Y2UAAAAAygAAAAAAAADQAAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAA
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
BAAA9QEAAIAFAAAiBQAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAA
AAAAAAAAHgIAAPoAAADKAAAAAAAAANAAAABiAAAAAwAAAJAXAAAQDwAA8AUAAHAFAAAAAAAAEwAA
AOoAAAAAAAAAAAEAAGAFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAWzAAAAAAAAAAAAAAAAAAAAAAAA
AQAAAAAAAAAAAAAAcgQAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAALIEAAAAAAAA0AQAAGIAAAAC
AAAAIgIAAAAAAAALAAAACwAAACICAAAAAAAATQQAAFkCAACgAQAAsgQAAAAAAAAACAAAYgAAAAEA
AABSAAAADAAAAFF1aWNrIERlcGxveaABAACyBAAAAAAAALoAAAAAAAAAUgAAAAgAAABtZW51QmFy
OmIAAAABAAAAAAAAAKABAAAiBQAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////
/////wUAAAAFAAAAKwIAADEBAADKAAAAAAAAANAAAABiAAAAAgAAAIAFAABgAgAAcAUAAAAAAAAV
AAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAA
AAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAA
AFIAAAAHAAAAY3VycmVudFIAAAANAAAAU2hlbGxWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291
cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

