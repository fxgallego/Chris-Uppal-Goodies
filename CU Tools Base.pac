| package |
package := Package name: 'CU Tools Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

A very small framework that I use to give my applications a common look.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '0.0009 (unpublished)'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Tools Base'')
	propertyAt: #ExternalResourceFileNames
	put: #(''Resources\Metagnostic.ico'').
!!'.

package classNames
	add: #CUToolShell;
	add: #CUWizardShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #CUToolShell -> 'Default view';
	add: #CUWizardShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Package-relative File Locator';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package!

"Class Definitions"!

Shell subclass: #CUToolShell
	instanceVariableNames: 'isTopMost'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CUToolShell subclass: #CUWizardShell
	instanceVariableNames: 'pageNumber pages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

CUToolShell guid: (GUID fromString: '{39327820-5EFE-11D4-8725-98CF9948BA36}')!
CUToolShell comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Simple base class to reduce, very slightly, the quantity of boilerplate code which is duplicated from application to application.
'!
!CUToolShell categoriesForClass!Unclassified! !
!CUToolShell methodsFor!

bugs
	"command -- display the bugs-box"

	MessageBox
		warning: self class bugs
		caption: ('Known bugs in ' , self class toolName).!

canForgetWindowSize
	"command -- can we forget preferred size for windows"

	^ self class defaultExtent notNil.!

captionExtension: aString
	"set the caption to our tool name followed (if aString is non-empty)
	by ' - ' and then aString"

	| cap |

	cap := self class toolName.

	(aString isNil or: [aString isEmpty]) ifFalse:
		[cap := cap , ' - ' , aString].

	self caption: cap.!

confirm: aString
	"popup a message box to confirm the given question"

	^ MessageBox
		confirm: aString
		caption: self class toolName.!

forgetWindowSize
	"command -- forget preferred size for windows"

	self class defaultExtent: nil.!

help
	"command -- display the help-box"

	MessageBox
		notify: self class help
		caption: ('Help for ' , self class toolName).!

helpAbout
	"command -- display the about-box"

	MessageBox
		notify: self class about
		caption: ('About ' , self class toolName).!

initialize
	"private -- establish coherent inital state"

	isTopMost := false.
	self captionExtension: nil.
	^ super initialize.!

queryCommand: aCommandQuery
	"private -- set the enabledness etc of aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd == #toggleTopMost ifTrue: [checked := isTopMost].
	cmd = #forgetWindowSize ifTrue: [enabled := self canForgetWindowSize].

	aCommandQuery
		isEnabled: enabled;
		isChecked: checked.
!

rememberWindowSize
	"command -- make the current window size be the preferred size"

	self class defaultExtent: self view extent.!

show
	"shows the view of the receiver using the default extent, if any"

	| extent |

	extent := self class defaultExtent.
	extent isNil ifFalse: [self view extent: extent].

	^ super show.!

todo
	"command -- display the todo-box"

	MessageBox
		notify: self class todo
		caption: ('Known deficiencies in ' , self class toolName).
!

toggleTopMost
	"command -- toggle whether this is a 'topmost' window"

	(isTopMost := isTopMost not)
		ifTrue: [self view beTopMost]
		ifFalse: [self view beNotTopMost].! !
!CUToolShell categoriesFor: #bugs!commands!public! !
!CUToolShell categoriesFor: #canForgetWindowSize!commands!public! !
!CUToolShell categoriesFor: #captionExtension:!accessing!public! !
!CUToolShell categoriesFor: #confirm:!helpers!public! !
!CUToolShell categoriesFor: #forgetWindowSize!commands!public! !
!CUToolShell categoriesFor: #help!commands!public! !
!CUToolShell categoriesFor: #helpAbout!commands!public! !
!CUToolShell categoriesFor: #initialize!initializing!private! !
!CUToolShell categoriesFor: #queryCommand:!commands!private! !
!CUToolShell categoriesFor: #rememberWindowSize!commands!public! !
!CUToolShell categoriesFor: #show!operations!public! !
!CUToolShell categoriesFor: #todo!commands!public! !
!CUToolShell categoriesFor: #toggleTopMost!commands!private! !

!CUToolShell class methodsFor!

about
	"answer a string very briefly describing ourself"

#subclassResponsibility.

	^ 'CU ToolShell.  Version 0.
Copyright © Chris Uppal, 2000-2003.
chris.uppal@metagnostic.org'.
!

bugs
	"answer a String describing the less than outstanding work"

	#subclassResponsibility.
	^ 'implement %s class>>#bugs !!' sprintfWith: self name.
!

defaultExtent
	"Answer a <Point> which is the user's chosen default extent for new instances
	of the receiver, or nil if left up to Windows."

	| entry |

	entry := self registryEntry at: #defaultExtent ifAbsent: [^ nil].
	^ (entry valueAt: 'x' ifAbsent: [400]) @ (entry valueAt: 'y' ifAbsent: [500]).!

defaultExtent: aPointOrNil
	"set the receiver's defaultExtent to aPointOrNil"

	aPointOrNil isNil
		ifTrue:
			[self removeRegistryAt: #defaultExtent]
		ifFalse:
			[(self registryAt: #defaultExtent)
				valueAt: 'x' put: aPointOrNil x;
				valueAt: 'y' put: aPointOrNil y].

	self trigger: #defaultExtentChanged.
!

defaultIconName
	"answers the name of the Icon that can be used to represent this class"

	^ 'Metagnostic.ico'.!

description
	"answer a string briefly describing ourself"

	#subclassResponsibility.
	^ self comment.	"handy hack"!

help
	"answer our help text"

	"hacky, very hacky..."
	^ self description.!

icon
	"answers an Icon that can be used to represent this class"

	"this will have to be recompiled if the package we live in changes its name"
	^ (Smalltalk at: #Icon)
		fromFile: self defaultIconName
		usingLocator: (PackageResourceLocator packageNamed: ##(self owningPackage name)).
!

initialize
	"private -- class-side initialisation
		self initialize
	"

	self reuseIfOpen: true.
!

publishedAspects
	"answer our aspects as a Set"

	^ super publishedAspects
		add: (Aspect boolean: #reuseIfOpen);
		add: (Aspect name: #defaultExtent);
		yourself.
!

publishedEvents
	"answer our published events"

	^ super publishedEvents
		add: #reuseIfOpenChanged;
		add: #defaultExtentChanged;
		yourself.
!

registerAsTool
	"add ourself to the 'system tools'"

	| icon |

	"add ourself to the 'Extra tools' folder"
	icon := (Smalltalk at: #SmalltalkSystemIcon) show: self description: self toolName.
	(Smalltalk at: #SmalltalkSystem) current addAdditionalToolsFolderIcon: icon.

	"add ourself to the 'Dolphin Options' set"
	(Smalltalk at: #SmalltalkSystem) registerTool: self.
!

registryAt: aString
	"private -- answer the registry entry with the given name, creating it if necesary"

	^ self registryEntry createKey: aString.
!

registryAt: aString put: aValue
	"private -- set the value associated with aString in our registry entry.
	answers the new entry (not its value)"

	^ (self registryAt: aString)
		value: aValue;
		yourself.
!

registryEntry
	"private -- answer a RegKey representing the root of our registry entry"

	^ self registryEntryRoot createKey: self registryKeyName.
!

registryEntryRoot
	"private -- answer a RegKey representing the root of our registry entry"

	^ RegKey userRoot createKey: self registryRootName.
!

registryKeyName
	"private -- answer the name of our registy entry within the root"

	^ self toolName.!

registryRootName
	"private -- answer the name of the root of our registy entry"

	^ 'Software\Metagnostic'.!

registryValue: aString default: aValue
	"private -- answer the registry value associated with aString, setting it to aValue if it is
	not already present"

	^ (self registryEntry at: aString ifAbsent: [self registryAt: aString put: aValue]) value.
!

removeRegistryAt: aString
	"private -- remove the registry entry with the given name"

	^ self registryEntry removeKey: aString ifAbsent: [].
!

reuseIfOpen
	"answer the receiver's reuseIfOpen aspect"

	^ (self registryValue: #reuseIfOpen default: 0) = 1.
!

reuseIfOpen: aBool
	"set the receiver's reuseIfOpen aspect"

	self registryAt: #reuseIfOpen put: (aBool ifTrue: [1] ifFalse: [0]).
	self trigger: #reuseIfOpenChangedChanged.
!

show
	"shows an instance of the receiver, overridden to implement the reuse if open stuff"

	| it |

	self reuseIfOpen ifFalse: [^ super show].
	it := self allInstances detect: [:each | each isOpen] ifNone: [^ super show].
	it view zOrderTop; show.

	^ it.!

show: aViewName on: anObject asToolboxFor: aView
	"shows an instance of the receiver on the given Object as its model, and displayed
	as a toolbox view attached to aView"

	| new newView |

	new := self on: anObject.

	newView := self loadViewResource: aViewName inContext: View desktop.
	newView bePopupFor: aView.
	newView largeIcon: (aView topShell view largeIcon).
	new view: newView.

	^ new show.!

showOn: aModel
	"show and answer a new instance dislaying aModel.
	NB: ignores the reuseIfOpen stuff"

	^ super showOn: aModel.!

showOn: anObject asToolboxFor: aView
	"shows an instance of the receiver on the given Object as its model, and displayed
	as a toolbox view attached to aView"

	^ self show: self defaultView on: anObject asToolboxFor: aView.!

todo
	"answer a String describing the outstanding work"

	#subclassResponsibility.
	^ 'implement %s class>>#todo !!' sprintfWith: self name.
!

toolName
	"answer the Sting name to use for the name of the tool we define"

	| base stream lastWasUpper |

	base := self name.
	(base endsWith: 'Shell') ifTrue: [base := base allButLast: 5].

	"useful default implementation"
	stream := String writeStream.
	lastWasUpper := true.
	base do:
		[:each || isUpper |
		isUpper := each isUppercase.
		(isUpper and: [lastWasUpper not]) ifTrue: [stream space].
		lastWasUpper := isUpper.
		stream nextPut: each].

	^ stream contents.!

unRegisterAsTool
	"remove ourself from the 'system tools'"

	| icon |

	"remove ourself from the 'Extra tools' folder"
	icon := (Smalltalk at: #SmalltalkSystemIcon) show: self description: self toolName.
	(Smalltalk at: #SmalltalkSystem) current removeSystemFolderIcon: icon.

	"remove ourself from the 'Dolphin Options' set"
	(Smalltalk at: #SmalltalkSystem) unregisterTool: self.
! !
!CUToolShell class categoriesFor: #about!documentation!public! !
!CUToolShell class categoriesFor: #bugs!documentation!public! !
!CUToolShell class categoriesFor: #defaultExtent!accessing!public! !
!CUToolShell class categoriesFor: #defaultExtent:!accessing!public! !
!CUToolShell class categoriesFor: #defaultIconName!constants!public! !
!CUToolShell class categoriesFor: #description!documentation!public! !
!CUToolShell class categoriesFor: #help!documentation!public! !
!CUToolShell class categoriesFor: #icon!constants!public! !
!CUToolShell class categoriesFor: #initialize!development!initializing!private! !
!CUToolShell class categoriesFor: #publishedAspects!constants!must strip!public! !
!CUToolShell class categoriesFor: #publishedEvents!constants!public! !
!CUToolShell class categoriesFor: #registerAsTool!development!initializing!public! !
!CUToolShell class categoriesFor: #registryAt:!accessing!private!registry! !
!CUToolShell class categoriesFor: #registryAt:put:!accessing!private!registry! !
!CUToolShell class categoriesFor: #registryEntry!accessing!private!registry! !
!CUToolShell class categoriesFor: #registryEntryRoot!accessing!private!registry! !
!CUToolShell class categoriesFor: #registryKeyName!constants!private!registry! !
!CUToolShell class categoriesFor: #registryRootName!constants!private!registry! !
!CUToolShell class categoriesFor: #registryValue:default:!accessing!private!registry! !
!CUToolShell class categoriesFor: #removeRegistryAt:!accessing!private!registry! !
!CUToolShell class categoriesFor: #reuseIfOpen!accessing!public! !
!CUToolShell class categoriesFor: #reuseIfOpen:!accessing!public! !
!CUToolShell class categoriesFor: #show!instance creation!public! !
!CUToolShell class categoriesFor: #show:on:asToolboxFor:!instance creation!public! !
!CUToolShell class categoriesFor: #showOn:!instance creation!public! !
!CUToolShell class categoriesFor: #showOn:asToolboxFor:!instance creation!public! !
!CUToolShell class categoriesFor: #todo!documentation!public! !
!CUToolShell class categoriesFor: #toolName!constants!displaying!public! !
!CUToolShell class categoriesFor: #unRegisterAsTool!development!initializing!public! !

CUWizardShell guid: (GUID fromString: '{9BA61CB1-C56A-4BDD-8C8A-EFE5EAE78267}')!
CUWizardShell comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org'!
!CUWizardShell categoriesForClass!Unclassified! !
!CUWizardShell methodsFor!

additionalAccelerators
	"overriden to add F1 help (since we don't pick it up from the menu bar, us not
	having one)"

	^ super additionalAccelerators , #(#(#help 'F1'))!

canCancel
	"private -- answer true iff we may perform the #cancel command ?"

	"override if necessary"
	^ true.
!

cancel
	"command -- cancel this wizard"

	self canCancel ifFalse: [^ self].

	self view exit.
!

canFinish
	"private -- answer true iff we may perform the #finish command ?"

	^ self isOnFinalPage.
!

canNextPage
	"private -- answer true iff we may perform the #nextPage command ?"

	^ self isOnFinalPage not.
!

canPreviousPage
	"private -- answer true iff we may perform the #previousPage command ?"

	^ self isOnFirstPage not.
!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	self add: Presenter new name: 'Pages'.

	1 to: self maxPages do:
		[:i || page |
		page := Presenter new.
		pages addLast: page.
		self add: page name: ('Page%d' sprintfWith: i)].

!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self pagesPresenter view
		when: #currentCardChanged
		send: #onPageChanged
		to: self.
!

currentPage
	"answer the presenter of the current page"

	^ pages at: pageNumber.!

finish
	"command -- finish the wizard's operation"

	self canFinish ifFalse: [^ self].

	Error notYetImplemented.
!

finishButton
	"private -- answer the view named 'finishButton'"

	^ self view viewNamed: 'finishButton' .
!

initialize
	"private -- establish coherent inital state"

	pageNumber := 0.
	pages := OrderedCollection new.
	^ super initialize.!

isOnFinalPage
	"answer whether we are currently displaying the final page"

	^ pageNumber = self maxPages.!

isOnFirstPage
	"answer whether we are currently displaying the first page"

	^ pageNumber = 1.!

maxPages
	"answer how many pages we have"

	"override as necessary"
	^ 5.!

nextPage
	"command -- switch to the next page"

	self canNextPage ifFalse: [^ self].

	pageNumber := pageNumber + 1.
	self pagesPresenter view nextCard.!

nextPageButton
	"private -- answer the view named 'nextPageButton'"

	^ self view viewNamed: 'nextPageButton'.
!

onPageChanged
	"private -- invoked when the displayed page has changed"

	self isOnFirstPage
		ifTrue: [self previousPageButton hide]
		ifFalse: [self previousPageButton show].

	self isOnFinalPage
		ifTrue: [self nextPageButton hide. self finishButton show; isDefault: true]
		ifFalse: [self finishButton hide. self nextPageButton show; isDefault: true].
!

onViewOpened
	"private -- called when the view has been connected and we are ready to roll"

	super onViewOpened.
	pageNumber := 1.
	self onPageChanged.!

page: anInteger
	"answer the presenter of the indexed page"

	^ pages at: anInteger.!

pageNumber
	"answer the number of the current page"

	^ pageNumber.!

pagesPresenter
	"answer the presenter named 'Pages'"

	^ self presenterNamed: 'Pages'.
!

previousPage
	"command -- switch to the previous"

	self canPreviousPage ifFalse: [^ self].

	pageNumber := pageNumber - 1.
	self pagesPresenter view previousCard.!

previousPageButton
	"private -- answer the view named 'previousPageButton'"

	^ self view viewNamed: 'previousPageButton'.
!

queryCommand: aCommandQuery
	"private -- set the enabledness etc of aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	enabled := aCommandQuery isEnabled.

	cmd = #cancel ifTrue: [enabled := self canCancel].
	cmd = #finish ifTrue: [enabled := self canFinish].
	cmd = #nextPage ifTrue: [enabled := self canNextPage].
	cmd = #previousPage ifTrue: [enabled := self canPreviousPage].

	aCommandQuery isEnabled: enabled.
! !
!CUWizardShell categoriesFor: #additionalAccelerators!constants!public! !
!CUWizardShell categoriesFor: #canCancel!commands!private! !
!CUWizardShell categoriesFor: #cancel!commands!public! !
!CUWizardShell categoriesFor: #canFinish!commands!private! !
!CUWizardShell categoriesFor: #canNextPage!commands!private! !
!CUWizardShell categoriesFor: #canPreviousPage!commands!private! !
!CUWizardShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!CUWizardShell categoriesFor: #createSchematicWiring!event handling!initializing!private! !
!CUWizardShell categoriesFor: #currentPage!pages!public! !
!CUWizardShell categoriesFor: #finish!commands!public! !
!CUWizardShell categoriesFor: #finishButton!private!subpresenters! !
!CUWizardShell categoriesFor: #initialize!initializing!private! !
!CUWizardShell categoriesFor: #isOnFinalPage!pages!public! !
!CUWizardShell categoriesFor: #isOnFirstPage!pages!public! !
!CUWizardShell categoriesFor: #maxPages!constants!pages!public! !
!CUWizardShell categoriesFor: #nextPage!commands!pages!public! !
!CUWizardShell categoriesFor: #nextPageButton!private!subpresenters! !
!CUWizardShell categoriesFor: #onPageChanged!event handling!pages!private! !
!CUWizardShell categoriesFor: #onViewOpened!event handling!public! !
!CUWizardShell categoriesFor: #page:!pages!public! !
!CUWizardShell categoriesFor: #pageNumber!pages!public! !
!CUWizardShell categoriesFor: #pagesPresenter!public!subpresenters! !
!CUWizardShell categoriesFor: #previousPage!commands!pages!public! !
!CUWizardShell categoriesFor: #previousPageButton!private!subpresenters! !
!CUWizardShell categoriesFor: #queryCommand:!commands!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: CUToolShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAFIGAAAhU1RCIDAgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
TgINAAEAAABTVEJDbGFzc1Byb3h5AAAAADYABgBTdHJpbmcHAAAARG9scGhpbpIAAAAJAAAAU2hl
bGxWaWV3JgAFAEFycmF5GwAAAAAAAAAAAAAAwgAAAAIAAAABAJ4BAQACAGAAAAAAAAAAAAAAAAAA
AAAHAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOAhoAU1RCSWRlbnRpdHlEaWN0aW9uYXJ5UHJveHkA
AAAAegAAAAAAAACgAAAAkgAAABIAAABJZGVudGl0eURpY3Rpb25hcnnCAAAAAAAAAAAAAABGBQcA
AgAAAE1lbnVCYXIAAAAAAAAAABAAAADCAAAAAgAAAEYFBAACAAAATWVudQAAAAAAAAAAEAAAAMIA
AAABAAAAcgEAAAAAAAAAAAAAEAAAAMIAAAAEAAAARgIPAAEAAABDb21tYW5kTWVudUl0ZW0AAAAA
AQAAAEYEEgACAAAAQ29tbWFuZERlc2NyaXB0aW9uAAAAAA4BDgBTVEJTeW1ib2xQcm94eQAAAACS
AAAADQAAAHRvZ2dsZVRvcE1vc3SSAAAADgAAAEFsd2F5cyBvbiAmVG9wqSAAAAAAAABGAQ8AAQAA
AERpdmlkZXJNZW51SXRlbQAAAAABEAAAwgEAAAAAAAABAAAA4gEAAAAAAAAKAgAAAAAAAJIAAAAS
AAAAcmVtZW1iZXJXaW5kb3dTaXplkgAAABMAAAAmUmVtZW1iZXIgdGhpcyBzaXplAQAAAAAAAADC
AQAAAAAAAAEAAADiAQAAAAAAAAoCAAAAAAAAkgAAABAAAABmb3JnZXRXaW5kb3dTaXplkgAAAAwA
AAAmRm9yZ2V0IHNpemUBAAAAAAAAAJIAAAAIAAAAJk9wdGlvbnMAAAAAkgAAAAYAAAAmVG9vbHMA
AAAAcgEAAAAAAAAAAAAAEAAAAMIAAAAGAAAAwgEAAAAAAAABAAAA4gEAAAAAAAAKAgAAAAAAAJIA
AAAEAAAAaGVscJIAAAASAAAAJkhlbHAgb24gdGhpcyB0b29s4QAAAAAAAABCAgAAAAAAAAEQAADC
AQAAAAAAAAEAAADiAQAAAAAAAAoCAAAAAAAAkgAAAAkAAABoZWxwQWJvdXSSAAAAEAAAACZBYm91
dCB0aGlzIHRvb2wBAAAAAAAAAEICAAAAAAAAARAAAMIBAAAAAAAAAQAAAOIBAAAAAAAACgIAAAAA
AACSAAAABAAAAGJ1Z3OSAAAABQAAACZCdWdzAQAAAAAAAADCAQAAAAAAAAEAAADiAQAAAAAAAAoC
AAAAAAAAkgAAAAQAAAB0b2RvkgAAAAUAAAAmVG9kbwEAAAAAAAAAkgAAAAUAAAAmSGVscAAAAACS
AAAAAAAAAAAAAAAAAAAABgMQAEFjY2VsZXJhdG9yVGFibGUAAAAAAAAAABAAAADCAAAAAgAAAAYC
CwBBc3NvY2lhdGlvbgAAAACpIAAA8AEAAPIEAAAAAAAA4QAAAFADAAAAAAAAAQAAAAAAAAAAAAAA
AAAAAAAAAAABAAAAAAAAAAAAAAAGAQ8ATWVzc2FnZVNlcXVlbmNlAAAAAA4CEgBTVEJDb2xsZWN0
aW9uUHJveHkAAAAAegAAAAAAAACgAAAAkgAAABEAAABPcmRlcmVkQ29sbGVjdGlvbsIAAAADAAAA
BgMLAE1lc3NhZ2VTZW5kAAAAAAoCAAAAAAAAkgAAABAAAABjcmVhdGVBdDpleHRlbnQ6wgAAAAIA
AAAGAgUAUG9pbnQAAAAACwAAAAsAAADiBQAAAAAAAKUEAADHAQAAYAAAAJIFAAAAAAAACgIAAAAA
AACSAAAABQAAAHRleHQ6wgAAAAEAAACSAAAADAAAAENVIFRvb2xTaGVsbGAAAACSBQAAAAAAAAoC
AAAAAAAAkgAAAAgAAABtZW51QmFyOsIAAAABAAAAUAEAAGAAAAAGAQ8AV0lORE9XUExBQ0VNRU5U
AAAAADYACQBCeXRlQXJyYXksAAAALAAAAAAAAAAAAAAA/////////////////////wUAAAAFAAAA
VwIAAOgAAABKBQAAAAAAAGAFAAAwAQAA4gUAAAAAAADBAAAAwQAAAAAAAAAVAAAARgUEAAMAAABJ
Y29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABE
b2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3Vy
cmVudFIAAAANAAAAU2hlbGxWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVBy
b3h5AAAAAFIAAAAQAAAAZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: CUWizardShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
IgIAAAAAAAAlAwAAEwMAANAHAADCBAAAAAAAAEAFAABiAAAAAQAAAFIAAAAGAAAAUGFnZSAy0AcA
AIIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAAAAAAAACSAQAA
iQEAAMoAAAAAAAAA0AAAAMAFAADQBQAAAAAAABMAAADiBQAAAAAAAAAGAAABAAAAIAYAAAEAAABA
BgAAAQAAAGAGAAABAAAA6gAAAAAAAAAAAQAAwAUAAAAAAACCBAAAAAAAAMoAAAAAAAAA0AAAAGIA
AAABAAAAwgQAAAAAAADgBAAAYgAAAAIAAAAiAgAAAAAAAAEAAAABAAAAIgIAAAAAAAAlAwAAEwMA
AFAHAACCBQAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA/////////////////////wAAAAAAAAAA
kgEAAIkBAADKAAAAAAAAANAAAABiAAAAAQAAANAHAADQBQAAAAAAABMAAAAyAwAAAAAAAAcAAACa
AQAAAAAAAGADAABiAAAADwAAAAAAAACAAgAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIA4AkAAAAA
AAACAgAAAAAAAB8AAAAAAAAABwAAAAAAAAAAAAAAAAAAAOAJAABCAgAAAAAAAOoAAAAAAAAA8AAA
AGIAAAACAAAAmgEAAAAAAAAABAAAYgAAABAAAAAAAAAA4AkAAGIAAAACAAAAggAAAAQAAAABAQBE
AQAAAGAKAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAYAoAAAAAAACCAAAABAAAALcl43di
BAAAAAAAAAAAAAAAAAAAAAAAAIIEAAAAAAAAygAAAAAAAADQAAAAYgAAAAIAAADCBAAAAAAAAOAE
AABiAAAAAgAAACICAAAAAAAAAQAAAAEAAAAiAgAAAAAAACUDAAATAwAAYAoAAMIEAAAAAAAAQAUA
AGIAAAABAAAAUgAAAAYAAABQYWdlIDNgCgAAggUAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//
//////////////////8AAAAAAAAAAJIBAACJAQAAygAAAAAAAADQAAAAwAUAANAFAAAAAAAAEwAA
AOIFAAAAAAAAAAYAAAEAAAAgBgAAAQAAAEAGAAABAAAAYAYAAAEAAADqAAAAAAAAAAABAADABQAA
AAAAAIIEAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAADCBAAAAAAAAOAEAABiAAAAAgAAACICAAAA
AAAAAQAAAAEAAAAiAgAAAAAAACUDAAATAwAA4AkAAIIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAAA
AAD/////////////////////AAAAAAAAAACSAQAAiQEAAMoAAAAAAAAA0AAAAGIAAAABAAAAYAoA
ANAFAAAAAAAAEwAAADIDAAAAAAAACQAAAJoBAAAAAAAAYAMAAGIAAAAPAAAAAAAAAIACAABiAAAA
AgAAAIIAAAAEAAAAAAAARAEAAgBwDAAAAAAAAAICAAAAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAA
AAAAcAwAAEICAAAAAAAA6gAAAAAAAADwAAAAYgAAAAIAAACaAQAAAAAAAAAEAABiAAAAEAAAAAAA
AABwDAAAYgAAAAIAAACCAAAABAAAAAEBAEQBAAAA8AwAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAA
AAAAAADwDAAAAAAAAIIAAAAEAAAAtyXjd2IEAAAAAAAAAAAAAAAAAAAAAAAAggQAAAAAAADKAAAA
AAAAANAAAABiAAAAAgAAAMIEAAAAAAAA4AQAAGIAAAACAAAAIgIAAAAAAAABAAAAAQAAACICAAAA
AAAAJQMAABMDAADwDAAAwgQAAAAAAABABQAAYgAAAAEAAABSAAAABgAAAFBhZ2UgNPAMAACCBQAA
AAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAAAAAAkgEAAIkBAADK
AAAAAAAAANAAAADABQAA0AUAAAAAAAATAAAA4gUAAAAAAAAABgAAAQAAACAGAAABAAAAQAYAAAEA
AABgBgAAAQAAAOoAAAAAAAAAAAEAAMAFAAAAAAAAggQAAAAAAADKAAAAAAAAANAAAABiAAAAAQAA
AMIEAAAAAAAA4AQAAGIAAAACAAAAIgIAAAAAAAABAAAAAQAAACICAAAAAAAAJQMAABMDAABwDAAA
ggUAAAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8AAAAAAAAAAJIBAACJ
AQAAygAAAAAAAADQAAAAYgAAAAEAAADwDAAA0AUAAAAAAAATAAAAMgMAAAAAAAALAAAAmgEAAAAA
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
AAAAIgIAAAAAAAB5AAAALwAAANAXAADCBAAAAAAAAEAFAABiAAAAAQAAAFIAAAAGAAAAQ2FuY2Vs
0BcAAIIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////VgEAAAAAAACS
AQAAFwAAAMoAAAAAAAAA0AAAAMAFAADQBQAAAAAAABMAAADiBQAAAAAAANAUAACJ////IAYAAAEA
AABABgAAAQAAAGAGAAABAAAAmgEAAAAAAABAEwAAYgAAABEAAAAAAAAAsBIAAGIAAAACAAAAggAA
AAQAAAAAIAFEAQAAAEAZAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAQBkAAAAAAACCAAAA
BAAAAD/f4neiEwAAAAAAALoAAAAAAAAAUgAAAAQAAABoZWxwUgAAAAUAAAAmSGVscAEAAAABAAAA
AAAAAAAAAAABAAAAggQAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAAMIEAAAAAAAA4AQAAGIAAAAC
AAAAIgIAAAAAAAABAAAAAQAAACICAAAAAAAAeQAAAC8AAABAGQAAwgQAAAAAAABABQAAYgAAAAEA
AABSAAAABQAAACZIZWxwQBkAAIIFAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////
////////AAAAAAAAAAA8AAAAFwAAAMoAAAAAAAAA0AAAAMAFAADQBQAAAAAAABMAAADiBQAAAAAA
AAAGAAABAAAAugAAAAAAAABSAAAADQAAAGZpeGVkVmlld0xlZnR5AAAAQAYAAAEAAABgBgAAAQAA
AOoAAAAAAAAAAAEAAGIAAAAGAAAA8BQAAFIAAAAMAAAAZmluaXNoQnV0dG9uMBMAAFIAAAASAAAA
cHJldmlvdXNQYWdlQnV0dG9uYBYAAFIAAAAOAAAAbmV4dFBhZ2VCdXR0b24AAAAAggQAAAAAAADK
AAAAAAAAANAAAABiAAAAAQAAAMIEAAAAAAAA4AQAAGIAAAACAAAAIgIAAAAAAAALAAAANQMAACIC
AAAAAAAAJQMAAC8AAACwEgAAggUAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////////
//////8FAAAAmgEAAJcBAACxAQAAygAAAAAAAADQAAAAYgAAAAUAAABAGQAAMBMAAGAWAADQFwAA
8BQAANAFAAAAAAAAEwAAAOIFAAAAAAAAugAAAAAAAABSAAAAEQAAAGZpeGVkUHJldmlvdXNMZWZ0
AQAAALoAAAAAAAAAUgAAABIAAABmaXhlZFByZXZpb3VzUmlnaHQBAAAAugAAAAAAAABSAAAAEwAA
AGZpeGVkUHJldmlvdXNCb3R0b20LAAAAYAYAAPf///+aAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAAP
AAAAU3RhdGljUmVjdGFuZ2xlYgAAAA4AAAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAAAHAQFEAQAE
AEAcAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAQBwAAAAAAACCAAAABAAAALcl43eCBAAA
AAAAAMoAAAAAAAAA0AAAAGIAAAABAAAAwgQAAAAAAADgBAAAYgAAAAIAAAAiAgAAAAAAAAsAAAAn
AwAAIgIAAAAAAAAlAwAABQAAAEAcAACCBQAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////
/////////////wUAAACTAQAAlwEAAJUBAADKAAAAAAAAANAAAADABQAA0AUAAAAAAAATAAAA4gUA
AAAAAADgGwAAAQAAAAAcAAABAAAAIBwAAAsAAAC6AAAAAAAAAFIAAAAMAAAAZml4ZWRWaWV3VG9w
BQAAAOoAAAAAAAAAAAEAAGIAAAACAAAAgAIAAFIAAAAFAAAAUGFnZXMAAAAAAAAAAAAAAAAAAAAA
AAAAAEssAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAggQAAAAAAADKAAAAAAAAANAAAABi
AAAAAwAAAMIEAAAAAAAA4AQAAGIAAAACAAAAIgIAAAAAAAALAAAACwAAACICAAAAAAAASQMAAKMD
AACgAQAAwgQAAAAAAABABQAAYgAAAAEAAABSAAAADgAAAENVIFdpemFyZFNoZWxsoAEAAMIEAAAA
AAAAugAAAAAAAABSAAAACAAAAG1lbnVCYXI6YgAAAAEAAAAAAAAAoAEAAIIFAAAAAAAAcgAAACwA
AAAsAAAAAAAAAAAAAAD/////////////////////BQAAAAUAAACpAQAA1gEAAMoAAAAAAAAA0AAA
AGIAAAADAAAAgAIAAEAcAACwEgAA0AUAAAAAAAAVAAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAA
DgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJ
bWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAANAAAAU2hl
bGxWaWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAA
ZG9scGhpbmRyMDA1LmRsbAAAAAA='))!

