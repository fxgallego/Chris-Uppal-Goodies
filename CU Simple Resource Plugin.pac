| package |
package := Package name: 'CU Simple Resource Plugin'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

*Very* simple pluigin for the CHB/SB that allows you to manage the selected class''s resources.

See class comment of SimpleResourcePlugin for more.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.05'.


package classNames
	add: #SimpleResourcePlugin;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #SimpleResourcePlugin -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Sortblocks';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

ClassBrowserPluginAbstract subclass: #SimpleResourcePlugin
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SimpleResourcePlugin guid: (GUID fromString: '{39E4706F-CB19-4681-AE68-288E0416593C}')!
SimpleResourcePlugin comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

*Very* simple pluiggin for the CHB/SB that allows you to manage the selected class''s resources.

Install by adding SimpleResourcePlugin to the #plugins list of any of the class browsers (through their ''Tools=>Options'' menu), or by evaluating methods in the class-side ''installing'' category.

Deficiencies:
	- should show all resources in package(s) when package(s) but no class is selected in SB;
	- should have more operations (rename, drag-move, etc).'!
!SimpleResourcePlugin categoriesForClass!Browser-Plugins! !
!SimpleResourcePlugin methodsFor!

browseDefinitions
	"command -- generic browse definitions command, in this case we open an RB on a list
	of the resources with the same name"

	| selection resIds rb |

	selection := self selectedResourceID ifNil: [^ self].

	rb := ResourceBrowser show.
	(rb presenterNamed: 'resources') filterBlock: [:each | each name = selection name].
	rb caption: ('Resources named ' , selection name).!

browseExternalResources
	"command -- open a resource browser on the list of external resources used by
	the selected resource"

	| selection resIds rb |

	selection := self selectedResourceID ifNil: [^ self].

	resIds := self hiddenObjectsSatisfying: [:each | each isKindOf: ResourceIdentifier].

	resIds isEmpty ifTrue: [^ MessageBox notify: (selection displayString, ' uses no external resources')].

	rb := ResourceBrowser show.
	(rb presenterNamed: 'resources') filterBlock: [:each | resIds includes: each].
	rb caption: ('Resources used by ' , selection displayString).
!

browseReferences
	"command -- generic browse references command, in this case we 'open' a resource
	browser on the list of resources that refer (as an external resource reference) to the current
	selection"

	| selection rb |

	selection := self selectedResourceID ifNil: [^ self].

	rb := ResourceBrowser showResourcesReferencing: selection.
	rb caption: ('Resources referring (externally) to ' , selection displayString).!

canOpenNewView
	"private -- answer whether we may perform the #openNewView command"

	| class |

	class := self selectedClass.

	^ class notNil and: [class respondsTo: #editNewDefaultView].
!

classFilter
	"private -- answer a <monadicValuavle> that evaluates to true for the currently selected class"

	^ self selectedClass
		ifNil: [ [:each | false] ]
		ifNotNil: [:it | [:each | each owningClass = it ] ].!

createComponents
	"private -- create presenters in order that they may be bound into MVP triads"

	self
		add: (ResourceListPresenter new) name: 'Resources';
		yourself.

	^ super createComponents.
!

createSchematicWiring
	"private -- arrange triggering between our components"

	self browser when: #classSelected send: #onBrowserClassSelected to: self.

	^ super createSchematicWiring.
!

displayOn: aStream
	"append our 'visual' name to the given stream, this is used to set the name in the CHB's tabs collection"

	aStream nextPutAll: 'Resources'!

hiddenObjectsSatisfying: a1Block
	"private -- answer a Collection of all the 'hidden object' in our selected resources that satisfy a1Block"

	| resID hidden answer |

	resID := self selectedResourceID ifNil: [^ #()].

	hidden := resID resource accessor hiddenObjects.
	answer := hidden select: a1Block.

	answer := answer asSortedCollection: (SortStringsAscending caching: #printString).

	^ answer asArray.
		
!

inspectHiddenObjectsSatisfying: a1Block
	"private -- open an inspector on the collection of all the 'hidden object' in our selected resources that satisfy a1Block"

	(self hiddenObjectsSatisfying: a1Block) inspect.!

listBlocks
	"command -- list the blocks use by the selected resource"

	self selectedResourceID isNil ifTrue: [^ self].

	self  inspectHiddenObjectsSatisfying: [:each | each isKindOf: BlockClosure].!

listClasses
	"command -- list the classes in our selected resource"

	self selectedResourceID isNil ifTrue: [^ self].

	"don't list metaclasses"
	self inspectHiddenObjectsSatisfying: [:each | each isKindOf: Class].!

listExternalResources
	"command -- list the external resources used by our selected resource"

	self selectedResourceID isNil ifTrue: [^ self].

	self inspectHiddenObjectsSatisfying: [:each | each isKindOf: ResourceIdentifier].
!

listSymbols
	"command -- list the symbols used in selected resource"

	self selectedResourceID isNil ifTrue: [^ self].

	self inspectHiddenObjectsSatisfying: [:each | each isKindOf: Symbol].
!

onBrowserClassSelected
	"called when the selected class has changed in our parent browser"

	self isCurrentCard ifTrue: [self updateFilters]!

onShownInBrowser
	"private -- called when this card is displayed in our parent browser"

	self updateFilters.!

onViewOpened
	"private -- set up whatever we can't do before the view is opened"

	| block listView |

	super onViewOpened.

	block := [:ctx | self ownerDrawResource: ctx. ctx := nil].
	listView := self resourcesPresenter view.
	listView customDrawBlock: block.
	listView allColumns do: [:each | each customDrawBlock: block].

!

openNewView
	"command -- open a new VC to create a new view resource"

	self canOpenNewView ifFalse: [^ self].

	self selectedClass editNewDefaultView.
!

ownerDrawResource: anNMLVCUSTOMDRAW
	"private -- called when Windows wants us to fill in the given NMLVCUSTOMDRAW object
	for an item in the method category pane"

	| resid |

	resid := anNMLVCUSTOMDRAW item.
	resid isNil ifTrue: [^ self].

	(resid isLoose) ifTrue:
		[anNMLVCUSTOMDRAW forecolor: ClassBrowserAbstract looseMethodColor].
!

queryCommand: aCommandQuery
	"set the enabledness of the command represented by aCommandQuery"

	| cmd enabled |

	super queryCommand: aCommandQuery.

	cmd := aCommandQuery commandSymbol.
	enabled := aCommandQuery isEnabled.

	cmd == #openNewView ifTrue: [enabled := self canOpenNewView].

	"these require a non-empty selection"
	(#(
		#listBlocks
		#listClasses
		#listExternalResources
		#listSymbols
		#ListInResourcesMenu
		#browseSelectedResources
		#browseExternalResources
		#browseDefinitions
		#browseReferences
		#BrowseResourcesMenu
	) includes: cmd) ifTrue: [enabled := self selectedResourceID notNil].

	aCommandQuery isEnabled: enabled.
!

resourcesPresenter
	"private -- answer the presenter named 'Resources'"

	^ self presenterNamed: 'Resources'.
!

selectedClass
	"private -- answer the currently selected instance class or nil"

	^ self browser actualClass ifNotNil: [:it | it instanceClass].!

selectedResourceID
	"private -- answer the currently selected resource or nil if there isn't one"

	^ self resourcesPresenter selectionOrNil.!

updateFilters
	"private -- reset our resource presenter's filters to show only the selected classes"

	^ self resourcesPresenter filterBlock: self classFilter.
! !
!SimpleResourcePlugin categoriesFor: #browseDefinitions!commands!public! !
!SimpleResourcePlugin categoriesFor: #browseExternalResources!commands!public! !
!SimpleResourcePlugin categoriesFor: #browseReferences!commands!public! !
!SimpleResourcePlugin categoriesFor: #canOpenNewView!commands!private! !
!SimpleResourcePlugin categoriesFor: #classFilter!helpers!private! !
!SimpleResourcePlugin categoriesFor: #createComponents!initializing!private!subpresenters! !
!SimpleResourcePlugin categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!SimpleResourcePlugin categoriesFor: #displayOn:!displaying!public! !
!SimpleResourcePlugin categoriesFor: #hiddenObjectsSatisfying:!helpers!private! !
!SimpleResourcePlugin categoriesFor: #inspectHiddenObjectsSatisfying:!helpers!private! !
!SimpleResourcePlugin categoriesFor: #listBlocks!commands!public! !
!SimpleResourcePlugin categoriesFor: #listClasses!commands!public! !
!SimpleResourcePlugin categoriesFor: #listExternalResources!commands!public! !
!SimpleResourcePlugin categoriesFor: #listSymbols!commands!public! !
!SimpleResourcePlugin categoriesFor: #onBrowserClassSelected!event handling!private! !
!SimpleResourcePlugin categoriesFor: #onShownInBrowser!event handling!private! !
!SimpleResourcePlugin categoriesFor: #onViewOpened!drawing!event handling!initializing!private! !
!SimpleResourcePlugin categoriesFor: #openNewView!commands!public! !
!SimpleResourcePlugin categoriesFor: #ownerDrawResource:!drawing!private! !
!SimpleResourcePlugin categoriesFor: #queryCommand:!commands!public! !
!SimpleResourcePlugin categoriesFor: #resourcesPresenter!private!subpresenters! !
!SimpleResourcePlugin categoriesFor: #selectedClass!accessing!private! !
!SimpleResourcePlugin categoriesFor: #selectedResourceID!accessing!private! !
!SimpleResourcePlugin categoriesFor: #updateFilters!private!updating! !

!SimpleResourcePlugin class methodsFor!

addToAllBrowsers
	"add this class to all the available class browser tools.

		self addToAllBrowsers.
	"

	ClassBrowserAbstract withAllSubclasses do: [:each | self addToBrowser: each].!

addToBrowser: aBrowserClass
	"add this class (plugin) to the given browser tool's list of plugins"

	| existing |

	existing := aBrowserClass plugins.
	(existing includes: self) ifFalse: [existing add: self].!

beAvailablePlugin
	"private -- add ourself to the marker category for plugins."

	(ClassCategory name: 'Browser-Plugins') addClass: self.
!

initialize
	"private -- class-side initialisation

		self initialize.
	"

	"commented out until we know that it's safe !!
	self addToAllBrowsers.
"!

removeFromAllBrowsers
	"remove this class from all the available class browser tools.

		self removeFromAllBrowsers.
	"

	ClassBrowserAbstract withAllSubclasses do: [:each | self addToBrowser: each].!

removeFromBrowser: aBrowserClass
	"add this class (plugin) to the given browser tool's list of plugins"

	aBrowserClass plugins remove: self ifAbsent: [].!

uninitialize
	"private -- class-side tear-down

		self uninitialize.
	"

	self removeFromAllBrowsers.! !
!SimpleResourcePlugin class categoriesFor: #addToAllBrowsers!installing!public! !
!SimpleResourcePlugin class categoriesFor: #addToBrowser:!installing!public! !
!SimpleResourcePlugin class categoriesFor: #beAvailablePlugin!development!private! !
!SimpleResourcePlugin class categoriesFor: #initialize!initializing!private! !
!SimpleResourcePlugin class categoriesFor: #removeFromAllBrowsers!installing!public! !
!SimpleResourcePlugin class categoriesFor: #removeFromBrowser:!installing!public! !
!SimpleResourcePlugin class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: SimpleResourcePlugin name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
dE1lbnU6YgAAAAEAAABgAwAAMAIAAKIOAAAAAAAAugAAAAAAAABSAAAABQAAAHRleHQ6YgAAAAEA
AABSAAAADQAAAFJlc291cmNlIG5hbWUwAgAAog4AAAAAAAC6AAAAAAAAAFIAAAAMAAAAY29sdW1u
T3JkZXI6YgAAAAEAAABiAAAAAwAAAAUAAAADAAAABwAAADACAAAGAQ8AV0lORE9XUExBQ0VNRU5U
AAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAAAAAAXgEAAPoAAADK
AAAAAAAAANAAAADQAgAA8g4AAAAAAADBAAAAwQAAAAAAAAAXAAAA6gAAAAAAAAAAAQAAYgAAAAIA
AAAwAgAAUgAAAAkAAABSZXNvdXJjZXMAAAAAYg4AAAAAAADKAAAAAAAAANAAAABiAAAAAQAAAKIO
AAAAAAAAwA4AAGIAAAACAAAA8g4AAAAAAAALAAAACwAAAPIOAAAAAAAAvQIAAPUBAACgAQAAAhAA
AAAAAAByAAAALAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAGMBAAD/AAAA
ygAAAAAAAADQAAAAYgAAAAEAAAAwAgAAQBAAAAAAAAATAAAARgUEAAMAAABJY29uAAAAAAAAAAAQ
AAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgA
AABJbWFnZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAARAAAA
Q29udGFpbmVyVmlldy5pY28OAh8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJhcnlQcm94eQAAAABS
AAAAEAAAAGRvbHBoaW5kcjAwNS5kbGwAAAAA'))!

