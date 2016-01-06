| package |
package := Package name: 'CU Browser Database UI'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

A *very* simple GUI to control and monitor the Browser Database.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.04'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Browser Database UI'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\GoPauseStopBar.bmp''
	).
!!'.

package classNames
	add: #BrowserDatabaseShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #BrowserDatabaseShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Browser Database';
	add: 'CU Package-relative File Locator';
	add: 'CU Sortblocks';
	add: 'CU Tools Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

CUToolShell subclass: #BrowserDatabaseShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

BrowserDatabaseShell guid: (GUID fromString: '{53902BC0-7A4A-4639-BE69-8A8EBF350216}')!
BrowserDatabaseShell comment: 'Copyright © Chris Uppal, 2003 - 2005.
chris.uppal@metagnostic.org
'!
!BrowserDatabaseShell categoriesForClass!Unclassified! !
!BrowserDatabaseShell methodsFor!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	self add: (Presenter new) name: 'Toolbar'.
	self add: (ListPresenter new) name: 'StatusList'.
!

disableBDB
	"command -- disable the Browser Database"

	| ok |

	ok := MessageBox
		confirm: 'Are you sure you wish to discard the current browser database ?'
		caption: 'Discard Browser Database'.

	ok ifTrue: [Cursor wait showWhile: [self model beDisabled]].!

model: aBrowserDatabase
	"set the model for this presenter, this is expected to be a BrowserDatabase"

	self model removeEventsTriggeredFor: self.

	super model: aBrowserDatabase.

	self model
		when: #stateChanged send: #onStateChanged to: self;
		when: #statisticsChanged send: #onStatisticsChanged to: self.

	self onStatisticsChanged.!

onStateChanged
	"private -- the browser database has just changed state"

	self toolbarPresenter view invalidateUserInterface.!

onStatisticsChanged
	"private -- the browser database has just changed its stats"

	| list |

	list := self model isEnabled
		ifTrue: [self model stats associations]
		ifFalse: [#()].
	self statusListPresenter list: (list asSortedCollection: (SortStringsAscending by: #key)).
	self statusListPresenter view isEnabled: list notEmpty.

	self onStateChanged.
!

pauseBDB
	"command -- disable the Browser Database"

	| ok |

	ok := MessageBox
		confirm: 'Are you sure you wish to pause the browser database ?'
		caption: 'Pause Browser Database'.

	ok ifTrue: [Cursor wait showWhile: [self model bePaused]].!

queryCommand: aCommandQuery
	"private -- set the enabledness etc of aCommandQuery"

	| cmd enabled checked |

	super queryCommand: aCommandQuery.
	cmd := aCommandQuery command.
	enabled := aCommandQuery isEnabled.
	checked := aCommandQuery isChecked.

	cmd == #runBDB ifTrue: [checked := self model isRunning].
	cmd == #pauseBDB ifTrue: [checked := self model isPaused. enabled := self model isEnabled].
	cmd == #stopBDB ifTrue: [checked := self model isStopped. enabled := self model isEnabled].
	cmd == #disableBDB ifTrue: [checked := self model isDisabled].

	aCommandQuery isEnabled: enabled.
	aCommandQuery isChecked: checked.
!

refreshStats
	"command -- update the stats display"

	self onStatisticsChanged.!

runBDB
	"command -- start the Browser Database"

	| ok |

	ok := MessageBox
		confirm: 'Are you sure you wish to run/resume the browser database ?'
		caption: 'Run Browser Database'.

	ok ifTrue: [Cursor wait showWhile: [self model beRunning]].!

shrinkBDB
	"command -- ask the BDB to optimise the space used by its internal tables"

	Cursor wait showWhile: [self model shrink].
!

statusListPresenter
	"private -- answer the presenter named 'StatusList'"

	^ self presenterNamed: 'StatusList'.
!

stopBDB
	"command -- stop (update then freeze) the Browser Database"

	| ok |

	ok := MessageBox
		confirm: 'Are you sure you wish to freeze the browser database ?'
		caption: 'Freeze Browser Database'.

	ok ifTrue: [Cursor wait showWhile: [self model beStopped]].!

toolbarPresenter
	"private -- answer the presenter named 'Toolbar'"

	^ self presenterNamed: 'Toolbar'.
! !
!BrowserDatabaseShell categoriesFor: #createComponents!initializing!private!subpresenters! !
!BrowserDatabaseShell categoriesFor: #disableBDB!commands!public! !
!BrowserDatabaseShell categoriesFor: #model:!accessing!initializing!models!public! !
!BrowserDatabaseShell categoriesFor: #onStateChanged!event handling!private! !
!BrowserDatabaseShell categoriesFor: #onStatisticsChanged!event handling!private! !
!BrowserDatabaseShell categoriesFor: #pauseBDB!commands!public! !
!BrowserDatabaseShell categoriesFor: #queryCommand:!commands!private! !
!BrowserDatabaseShell categoriesFor: #refreshStats!commands!public! !
!BrowserDatabaseShell categoriesFor: #runBDB!commands!public! !
!BrowserDatabaseShell categoriesFor: #shrinkBDB!commands!public! !
!BrowserDatabaseShell categoriesFor: #statusListPresenter!private!subpresenters! !
!BrowserDatabaseShell categoriesFor: #stopBDB!commands!public! !
!BrowserDatabaseShell categoriesFor: #toolbarPresenter!private!subpresenters! !

!BrowserDatabaseShell class methodsFor!

about
	"answer a string very briefly describing ourself"

	^ 'Browser Database Status Monitor.  Version 1.
Copyright © Chris Uppal, 2003-2005.
chris.uppal@metagnostic.org'.!

bugs
	"answer a String describing the less than outstanding work"

	^ 'Confirmation prompts are kinda wierd sometimes.
'.!

defaultModel
	"answer a default model to be assigned to the receiver when it is initialized"

	^ BrowserDatabase current.!

help
	"answer a string describing ourselves"

	^ '
Displays and controls the status of the Browser Database.

	-- chris
'.
!

initialize
	"private -- class-side initialisation.

		self initialize.
	"

	self reuseIfOpen: self superclass reuseIfOpen.
	self registerAsTool.
!

todo
	"answer a String describing the outstanding work"

	^ '
Revisit class update code now that metaclasses use #become: (if they do ??).
'.!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	self unRegisterAsTool.! !
!BrowserDatabaseShell class categoriesFor: #about!documentation!public! !
!BrowserDatabaseShell class categoriesFor: #bugs!documentation!public! !
!BrowserDatabaseShell class categoriesFor: #defaultModel!models!public! !
!BrowserDatabaseShell class categoriesFor: #help!documentation!public! !
!BrowserDatabaseShell class categoriesFor: #initialize!development!initializing!private! !
!BrowserDatabaseShell class categoriesFor: #todo!documentation!public! !
!BrowserDatabaseShell class categoriesFor: #uninitialize!development!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: BrowserDatabaseShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAADUUAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAAAAAABgIFAFBvaW50AAAAAFkCAABtAgAA
BwIAAAAAAAAAAAAAAAAAAKABAAAGBwwAQm9yZGVyTGF5b3V0AAAAAAEAAAABAAAAmgEAAAAAAACa
AAAAAAAAAFIAAAAUAAAARG9scGhpbiBDb250cm9sIEJhcnNSAAAABwAAAFRvb2xiYXJiAAAAGQAA
AAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAQDAEQBAAIAQAIAAAAAAAAGAQsAU3lzdGVtQ29sb3IA
AAAAHwAAAAAAAAAHAgAAAAAAAAYEBABGb250AAAAAAAAAAAQAAAABgEHAExPR0ZPTlQAAAAAcgAA
ADwAAADz////AAAAAAAAAAAAAAAAkAEAAAAAAAADAgEiQXJpYWwAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAACAgAAAAAAAMEAAADBAAAAAAAAAEACAAAAAAAAggAAAAgAAADNBP//AAAAAOoAAAAA
AAAAAAEAAGIAAAAAAAAA6gAAAAAAAAAAAQAAUAMAAMoAAAAAAAAA0AAAAFADAADqAAAAAAAAAPAA
AABQAwAAAAAAACAAAAAAAAAAAgIAAAAAAAAhAAAAIQAAAAICAAAAAAAALQAAAC0AAAAAAAAABgMK
AEZsb3dMYXlvdXQAAAAAAQAAAAEAAAC6AAAAAAAAAFIAAAAEAAAAbGVmdAYBDwBNZXNzYWdlU2Vx
dWVuY2UAAAAAygAAAAAAAADQAAAAYgAAAAIAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABS
AAAAEAAAAGNyZWF0ZUF0OmV4dGVudDpiAAAAAgAAAAICAAAAAAAAAQAAAAEAAAACAgAAAAAAAEkC
AAAzAAAAQAIAADIEAAAAAAAAugAAAAAAAABSAAAACgAAAHVwZGF0ZVNpemViAAAAAAAAAEACAAAG
AQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////////////
/wAAAAAAAAAAJAEAABkAAADKAAAAAAAAANAAAABiAAAAAQAAAJoBAAAAAAAAUAIAAGIAAAAZAAAA
AAAAAEACAABiAAAAAgAAAIIAAAAEAAAARAsARAEAAgAwBQAAAAAAALICAAAAAAAAHwAAAAAAAAAH
AgAAAAAAANICAAAAAAAAAAAAABAAAADyAgAAAAAAAHIAAAA8AAAA8////wAAAAAAAAAAAAAAAJAB
AAAAAAAAAwIBIkFyaWFsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgIAAAAAAADBAAAAwQAA
AAAAAAAwBQAAAAAAAIIAAAAIAAAAzQT//wAAAADqAAAAAAAAAAABAABQAwAA6gAAAAAAAAAAAQAA
YgAAAAgAAADzPAAABgcNAFRvb2xiYXJCdXR0b24AAAAA8zwAAAAAAAAwBQAADQAAAEYFEgAEAAAA
Q29tbWFuZERlc2NyaXB0aW9uAAAAALoAAAAAAAAAUgAAAAYAAABydW5CREJSAAAAKgAAAFVwZGF0
ZSB0aGUgZGF0YWJhc2UgYXMgbWV0aG9kcyBhcmUgY2hhbmdlZAEAAAABAAAAAAAAAEYIBgADAAAA
Qml0bWFwAAAAAAAAAAAQAAAABgIWAFBhY2thZ2VSZXNvdXJjZUxvY2F0b3IAAAAAUgAAABYAAABD
VSBCcm93c2VyIERhdGFiYXNlIFVJUgAAAAoAAABSZXNvdXJjZXMvUgAAABIAAABHb1BhdXNlU3Rv
cEJhci5ibXAAAAAAAAAAAAcAAAACAgAAAAAAAKEAAAAhAAAAAQAAAPU8AAACBgAAAAAAAPU8AAAA
AAAAMAUAAA0AAAAiBgAAAAAAALoAAAAAAAAAUgAAAAgAAABwYXVzZUJEQlIAAAAmAAAAVGVtcG9y
YXJpbHkgc3RvcCB1cGRhdGluZyB0aGUgZGF0YWJhc2UBAAAAAQAAAAAAAACABgAAAwAAAPc8AAAC
BgAAAAAAAPc8AAAAAAAAMAUAAA0AAAAiBgAAAAAAALoAAAAAAAAAUgAAAAcAAABzdG9wQkRCUgAA
ADkAAABNYWtlIHRoZSBkYXRhYmFzZSBhIGZyb3plbiBzbmFwc2hvdCBvZiB0aGUgY3VycmVudCBz
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'))!

