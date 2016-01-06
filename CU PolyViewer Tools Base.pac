| package |
package := Package name: 'CU PolyViewer Tools Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org

Slight specialisation of PolyViewer to duplicate the stuff in ''CU Tools Base'' for PolyViewer-based apps.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '0.0009 (unpublished)'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU PolyViewer Tools Base'')
	propertyAt: #ExternalResourceFileNames
	put: #(''Resources\Metagnostic.ico'').
!!
'.

package classNames
	add: #PolyViewerToolShell;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #PolyViewerToolShell -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Package-relative File Locator';
	add: 'CU PolyViewer';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	yourself).

package!

"Class Definitions"!

PolyViewerShell subclass: #PolyViewerToolShell
	instanceVariableNames: 'isTopMost'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

PolyViewerToolShell guid: (GUID fromString: '{2C3A686D-D704-4D4D-9891-086AEF35015F}')!
PolyViewerToolShell comment: 'Copyright © Chris Uppal, 2001.
chris.uppal@metagnostic.org'!
!PolyViewerToolShell categoriesForClass!Unclassified! !
!PolyViewerToolShell methodsFor!

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
		caption: ('Known deficiencies in ' , self class toolName).!

toggleTopMost
	"command -- toggle whether this is a 'topmost' window"

	(isTopMost := isTopMost not)
		ifTrue: [self view beTopMost]
		ifFalse: [self view beNotTopMost].! !
!PolyViewerToolShell categoriesFor: #bugs!commands!public! !
!PolyViewerToolShell categoriesFor: #canForgetWindowSize!commands!public! !
!PolyViewerToolShell categoriesFor: #captionExtension:!accessing!public! !
!PolyViewerToolShell categoriesFor: #confirm:!helpers!public! !
!PolyViewerToolShell categoriesFor: #forgetWindowSize!commands!public! !
!PolyViewerToolShell categoriesFor: #help!commands!public! !
!PolyViewerToolShell categoriesFor: #helpAbout!commands!public! !
!PolyViewerToolShell categoriesFor: #initialize!initializing!private! !
!PolyViewerToolShell categoriesFor: #queryCommand:!commands!private! !
!PolyViewerToolShell categoriesFor: #rememberWindowSize!commands!public! !
!PolyViewerToolShell categoriesFor: #show!operations!public! !
!PolyViewerToolShell categoriesFor: #todo!commands!public! !
!PolyViewerToolShell categoriesFor: #toggleTopMost!commands!public! !

!PolyViewerToolShell class methodsFor!

about
	"answer a string very briefly describing ourself"

#subclassResponsibility.

	^ 'CU PolyViewerToolShell.  Version 0.
Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'.
!

applicationMenu
	"answer a PolyViewerCommandList representing the basic menu for PolyViewer
	flavoured toolshells"

#subclassResponsibility.
	^ PolyViewerCommandList new.
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
	"answers the name Icon that can be used to represent this class"

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
	"private -- class-side initialisation.

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
	it := self allInstances detect: [:each | each isOpen and: [each isTearOff not]] ifNone: [^ super show].
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

standardMenu
	"answer a PolyViewerCommandList representing the basic menu to use for all
	instances (i.e. before any extra menus, or per-tab menus are added)"

	^ self toolshellMenu mergedWith: self applicationMenu.!

tabbedViewName
	"answer the name of the view resource to use for normal
	PolyViewer views"

	"override this back to 'Default view' so subclasses are less aware of the way PolyViewer works"
	^ 'Default view'.!

todo
	"answer a String describing the outstanding work"

	#subclassResponsibility.
	^ 'implement %s class>>#todo !!' sprintfWith: self name.
!

toolName
	"answer the String name to use for the name of the tool we define"

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

	^ stream contents.
		!

toolshellMenu
	"answer a PolyViewerCommandList representing the basic menu for PolyViewer
	flavoured toolshells"

	^ (PolyViewerCommandList new)

		"tools menu"
		add: ((PolyViewerCommandList new)
			text: '&Tools';

			"tools=>options menu"
			add: PolyViewerCommandSeparator new;
			add: ((PolyViewerCommandList new)
				text: '&Options';

				add: ((PolyViewerCommandItem new)
					text: 'Always on &top';
					accelerator: 'CTRL+T';
					command: #toggleTopMost;
					yourself);
				add: PolyViewerCommandSeparator new;
				add: ((PolyViewerCommandItem new)
					text: '&Remember window size';
					command: #rememberWindowSize;
					yourself);
				add: ((PolyViewerCommandItem new)
					text: '&Forget window size';
					command: #forgetWindowSize;
					yourself);

				yourself);

			yourself);

		"help menu"
		add: ((PolyViewerCommandList new)
			text: '&Help';

			add: ((PolyViewerCommandItem new)
				text: '&Help';
				accelerator: 'F1';
				command: #help;
				yourself);

			add: PolyViewerCommandSeparator new;
			add: ((PolyViewerCommandItem new)
				text: '&Bugs';
				command: #bugs;
				yourself);
			add: ((PolyViewerCommandItem new)
				text: '&Todo';
				command: #todo;
				yourself);

			add: PolyViewerCommandSeparator new;
			add: ((PolyViewerCommandItem new)
				text: ('&About ' , self toolName);
				command: #helpAbout;
				yourself);

			yourself);

		yourself.
!

unRegisterAsTool
	"remove ourself from the 'system tools'"

	| icon |

	"remove ourself from the 'Extra tools' folder"
	icon := (Smalltalk at: #SmalltalkSystemIcon) show: self description: self toolName.
	(Smalltalk at: #SmalltalkSystem) current removeSystemFolderIcon: icon.

	"remove ourself from the 'Dolphin Options' set"
	(Smalltalk at: #SmalltalkSystem) unregisterTool: self.
! !
!PolyViewerToolShell class categoriesFor: #about!documentation!public! !
!PolyViewerToolShell class categoriesFor: #applicationMenu!constants!public! !
!PolyViewerToolShell class categoriesFor: #bugs!documentation!public! !
!PolyViewerToolShell class categoriesFor: #defaultExtent!accessing!public! !
!PolyViewerToolShell class categoriesFor: #defaultExtent:!accessing!public! !
!PolyViewerToolShell class categoriesFor: #defaultIconName!constants!public! !
!PolyViewerToolShell class categoriesFor: #description!documentation!public! !
!PolyViewerToolShell class categoriesFor: #help!documentation!public! !
!PolyViewerToolShell class categoriesFor: #icon!constants!public! !
!PolyViewerToolShell class categoriesFor: #initialize!development!initializing!private! !
!PolyViewerToolShell class categoriesFor: #publishedAspects!constants!must strip!public! !
!PolyViewerToolShell class categoriesFor: #publishedEvents!constants!public! !
!PolyViewerToolShell class categoriesFor: #registerAsTool!development!initializing!public! !
!PolyViewerToolShell class categoriesFor: #registryAt:!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryAt:put:!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryEntry!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryEntryRoot!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryKeyName!constants!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryRootName!constants!private!registry! !
!PolyViewerToolShell class categoriesFor: #registryValue:default:!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #removeRegistryAt:!accessing!private!registry! !
!PolyViewerToolShell class categoriesFor: #reuseIfOpen!accessing!public! !
!PolyViewerToolShell class categoriesFor: #reuseIfOpen:!accessing!public! !
!PolyViewerToolShell class categoriesFor: #show!instance creation!public! !
!PolyViewerToolShell class categoriesFor: #show:on:asToolboxFor:!instance creation!public! !
!PolyViewerToolShell class categoriesFor: #showOn:!instance creation!public! !
!PolyViewerToolShell class categoriesFor: #showOn:asToolboxFor:!instance creation!public! !
!PolyViewerToolShell class categoriesFor: #standardMenu!constants!public! !
!PolyViewerToolShell class categoriesFor: #tabbedViewName!constants!public! !
!PolyViewerToolShell class categoriesFor: #todo!documentation!public! !
!PolyViewerToolShell class categoriesFor: #toolName!constants!displaying!public! !
!PolyViewerToolShell class categoriesFor: #toolshellMenu!constants!public! !
!PolyViewerToolShell class categoriesFor: #unRegisterAsTool!development!initializing!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: PolyViewerToolShell name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
ZXJSAAAAEwAAAFBvbHlWaWV3ZXJQcmVzZW50ZXJSAAAACwAAAFRhYmJlZCB2aWV3AAAAAJIDAAAA
AAAAygAAAAAAAADQAAAAYgAAAAEAAADSAwAAAAAAAPADAABiAAAAAgAAAAIDAAAAAAAAAQAAAAEA
AAACAwAAAAAAAHsGAAAfAgAAQAcAAIIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////
////////////AAAAAAAAAAA9AwAADwEAAGIAAAAAAAAAwAQAAAAAAAAVAAAA6gAAAAAAAAAAAQAA
YgAAAAQAAABABwAAUgAAABMAAABQb2x5Vmlld2VyUHJlc2VudGVyIAIAAFIAAAAHAAAAVG9vbGJh
cgAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAACSAwAA
AAAAAMoAAAAAAAAA0AAAAGIAAAACAAAA0gMAAAAAAADwAwAAYgAAAAIAAAACAwAAAAAAAAsAAAAL
AAAAAgMAAAAAAACLBgAAgQIAAKABAADSAwAAAAAAALoAAAAAAAAAUgAAAAgAAABtZW51QmFyOmIA
AAABAAAAAAAAAKABAACCBAAAAAAAAHIAAAAsAAAALAAAAAAAAAAAAAAA////////////////////
/wUAAAAFAAAASgMAAEUBAADKAAAAAAAAANAAAABiAAAAAwAAACACAABABwAA0AQAAMAEAAAAAAAA
FQAAAEYFBAADAAAASWNvbgAAAAAAAAAAEAAAAAYCFgBQYWNrYWdlUmVzb3VyY2VMb2NhdG9yAAAA
AFIAAAAYAAAAQ1UgUG9seVZpZXdlciBUb29scyBCYXNlUgAAAAoAAABSZXNvdXJjZXMvUgAAAA8A
AABNZXRhZ25vc3RpYy5pY28AAAAA'))!

