| package |
package := Package name: 'CU Filtering Method Browser'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

FilteringMethodBrowser is an extension of the system method browser that allows you to select methods to browse using more sophisticated criteria than is (easily) possible using the Dolphin tools.  It also allows you to change the criteria interactively.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris


History:

9.00
-	Now use PackageResourceLocator for icons, etc.

8.00
-	Increased icon column width for (bloody) WinXP.

7.00
-	I''d missed one #refersTo: in the changes for D5.1.

6.00
-	Changes for D5.1 (replaced #refersTo: with #refersToLiteral:).

5.00
-	Changes for D5 pl3 (sort blocks).

4.00
-	Bugfix: forgot to change class references filter in v2.00.

3.00
-	Bugfix: package filters were only working with *my* packages.

2.00
-	Changed interpretation of using Behavior and CompiledCode as filters.

1.00
-	First release.
'.

package basicPackageVersion: '9.00'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Filtering Method Browser'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\Filterbar.bmp''
		''Resources\FilteringMethodBrowser.ico''
		''Resources\InvertedTick.ico''
		''Resources\NoTick.ico''
		''Resources\Tick.ico''
	).
!!'.

package classNames
	add: #FilteringMethodBrowser;
	yourself.

package methodNames
	add: #Behavior -> #isRelevantToMethodBlock;
	add: #BlockClosure -> #isRelevantToMethodBlock;
	add: #CompiledMethod -> #isRelevantToMethodBlock;
	add: #CompiledMethod -> #overridenMethodRoot;
	add: #MethodCategory -> #isRelevantToMethodBlock;
	add: #MethodProtocol -> #isRelevantToMethodBlock;
	add: #Object -> #isRelevantToMethodBlock;
	add: #Package -> #isRelevantToMethodBlock;
	add: #PackageFolder -> #isRelevantToMethodBlock;
	add: #PackageFolder -> #isRelevantToMethodBlock:;
	add: #String -> #isRelevantToMethodBlock;
	add: #Symbol -> #isRelevantToMethodBlock;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #FilteringMethodBrowser -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Package-relative File Locator';
	add: 'CU Sortblocks';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

MethodBrowserShell subclass: #FilteringMethodBrowser
	instanceVariableNames: 'filterPresenter statusBar allMode filters filterStates'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Behavior methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ [:method | method methodClass instanceClass == self instanceClass].
! !
!Behavior categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!BlockClosure methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	"if we are a 1Block ourselves, then assume that we are a filter block in our own right"
	^ self argumentCount = 1
		ifTrue: [self]
		ifFalse: [super isRelevantToMethodBlock].! !
!BlockClosure categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!CompiledMethod methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	"select 'definitions' (i.e methods that are overrides of the same method as we are)"
	^ [:method | method selector == self selector and: [self overridenMethodRoot == method overridenMethodRoot]].!

overridenMethodRoot
	"Answer the root method that we override, or ourself if we do not override any method."

	| sup |
#CUadded.

	sup := self methodClass superclass ifNil: [^ self].
	^ (sup lookupMethod: self selector)
		ifNil: [self]
		ifNotNil: [:it | it overridenMethodRoot].! !
!CompiledMethod categoriesFor: #isRelevantToMethodBlock!helpers!public! !
!CompiledMethod categoriesFor: #overridenMethodRoot!helpers!public! !

!MethodCategory methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ [:method | self includesMethod: method].! !
!MethodCategory categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!MethodProtocol methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ [:method | self includesMethod: method].
! !
!MethodProtocol categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!Object methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ Message selector: #refersToLiteral: argument: self.
! !
!Object categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!Package methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ [:method | (self manager packageOfMethod: method)
			ifNil: [self includesClass: method methodClass]
			ifNotNil: [:it | it == self]].
! !
!Package categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!PackageFolder methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."

	| folderName |
#CUadded.

	"ugly hack to make #isRelevantToMethodBlock: easier and quicker"
	folderName := File
				fullPathOf: self pathname
				relativeTo: (SessionManager current imageBase).
	folderName := (File appendPathDelimiter: folderName) asLowercase.

	^ self isRelevantToMethodBlock: folderName.!

isRelevantToMethodBlock: aFolderName
	"Private - Answer a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the method lies
	in or below the named folder.
	aFolderName is required to be in lowercase and to end with the path separator (so we
	can use String>>beginsWith: to test containment easily)"
#CUadded.

	^ [:method | method owningPackage 
			ifNil: [false]
			ifNotNil: [:package | package packageFileName asLowercase beginsWith: aFolderName]].
! !
!PackageFolder categoriesFor: #isRelevantToMethodBlock!helpers!public! !
!PackageFolder categoriesFor: #isRelevantToMethodBlock:!helpers!public! !

!String methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ Message selector: #containsSource: argument: self.
! !
!String categoriesFor: #isRelevantToMethodBlock!helpers!public! !

!Symbol methodsFor!

isRelevantToMethodBlock
	"Answers a <monadicValuable> which, when passed a CompiledMethod
	as an argument, will answer a Boolean indicating whether the receiver is
	relevant to the method."
#CUadded.

	^ Message
		selector: #sendsSpecialSelector:
		argument: (VMLibrary default
				indexOfSpecialSelector: self
				ifAbsent: [^ Message selector: #refersToLiteral: argument: self]).! !
!Symbol categoriesFor: #isRelevantToMethodBlock!helpers!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

FilteringMethodBrowser guid: (GUID fromString: '{BC4AAF40-A426-11D3-8725-D20FA7AC0702}')!
FilteringMethodBrowser comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

FilteringMethodBrowser adds a filter panel to a standard method browser.  This allows you to build rather more complicated "queries" to select methods to browse.  The easiest way to find out about it is just to use it, though there should also be some documentation in the Metagnostic/Docs/ folder if you installed that from the download.

	-- chris'!
!FilteringMethodBrowser categoriesForClass!Unclassified! !
!FilteringMethodBrowser methodsFor!

addCategoryMembersFilter
	"command -- prompt to add a 'members of category' filter"

	| choices |

	choices := MethodCategory allMethodCategories asSortedCollection.

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Members of category...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | filterPresenter model add: each].!

addClassHierarchyMembersFilter
	"command -- prompt to add a 'members of class or subclasses' filter"

	| choices |

	choices := (Smalltalk values select: [:each | each isKindOf: Behavior]) asSortedCollection.

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Members of Class or subclasses...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | self addClassHierarchyMembersFilter: each].
!

addClassHierarchyMembersFilter: aClass
	"command -- add a filter that will match members of aClass and its subclasses"

	| string |

	 string := (String writeStream)
			nextPutAll: '[:it | it methodClass instanceClass includesBehavior: ';
			display: aClass;
			nextPutAll: ']';
			contents.

	self addCompiledExpression: string.
!

addClassMembersFilter
	"command -- prompt to add a 'members of class' filter"

	| choices |

	choices := (Smalltalk values select: [:each | each isKindOf: Behavior]) asSortedCollection.

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Members of Class...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | self addClassMembersFilter: each].!

addClassMembersFilter: aClass
	"command -- add a filter that will match members of aClass"

	filterPresenter model add: aClass.
!

addClassReferencesFilter
	"command -- prompt to add a 'references to class' filter"

	| choices |

	choices := (Smalltalk values select: [:each | each isKindOf: Behavior]) asSortedCollection.

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'References to Class...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | self addClassReferencesFilter: each].
!

addClassReferencesFilter: aClass
	"command -- add a filter that will match references to aClass"

	| string |

	string := (String writeStream)
				nextPutAll: '[:it | it refersToLiteral: ##(Smalltalk associationAt: ';
				print: aClass name;
				nextPutAll: ')]';
				contents.
	self addCompiledExpression: string
!

addCompiledExpression
	"command -- add an object to the filter list by prompting for
	an expression to parse"

	| expression |

	expression := Prompter
			prompt: 'Please enter an expression:'
			caption: 'Add object...'.
	(expression isNil or: [expression isEmpty])
		ifFalse: [self addCompiledExpression: expression].!

addCompiledExpression: aString
	"command -- compile aString and add the result as a filter"

	| obj |

	[obj := Compiler
			evaluate: aString 
			for: nil 
			evaluationPools: #()
			logged: false]
		on: CompilerErrorNotification
		do: [:cn | ^ MessageBox
				errorMsg: cn text
				caption: 'Error in expression...'].

	filterPresenter model add: obj.!

addGlobalReferencesFilter
	"command -- prompt to add a 'references to global' filter"

	| choices |

	choices := (Smalltalk associations reject: [:each | each value isKindOf: Behavior]) asSortedCollection: (SortStringsAscending by: #key).

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Add a Global variable...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | filterPresenter model add: each].
!

addPackageMembersFilter
	"command -- prompt to add a 'members of package' filter"

	| choices |

	choices := Package manager packages asSortedCollection: (SortStringsAscending by: #name).

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Members of package...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | filterPresenter model add: each].!

addProtocolMembersFilter
	"command -- prompt to add a 'members of protocol' filter"

	| choices |

	choices := MethodProtocol allMethodProtocols asSortedCollection.

	choices := ChoicePrompter
			multipleChoices: choices
			caption: 'Members of protocol...'.
	choices isNil ifTrue: [^self].

	choices do: [:each | filterPresenter model add: each].!

addSelectorDefinitionsFilter
	"command -- prompt to add a 'definitions of selector' filter"

	| string |

	string := Prompter
			prompt: 'Please enter a Selector'
			caption: 'Definitions of Selector...'.
	(string isNil or: [string isEmpty]) ifTrue: [^self].

	 string := (String writeStream)
			nextPutAll: '[:it | it selector == ';
			print: string asSymbol;
			nextPutAll: ']';
			contents.

	self addCompiledExpression: string.!

addSelectorReferencesFilter
	"command -- prompt to add a 'references to symbol' filter"

	| string |

	string := Prompter
			prompt: 'Please enter a Symbol'
			caption: 'References to Symbol...'.
	(string isNil or: [string isEmpty]) ifTrue: [^self].

	filterPresenter model add: string asSymbol.
!

addStringMatchFilter
	"command -- prompt to add a 'methods containing source' filter"

	| string |

	string := Prompter
			prompt: 'Please enter a string (no wildcards)'
			caption: 'Add String...'.
	(string isNil or: [string isEmpty]) ifTrue: [^self].

	filterPresenter model add: string.!

addWildcardFilter
	"command -- prompt to add a 'methods matching wildcards' filter"

	| expression |

	expression := Prompter
			prompt: 'Please enter a wildcard (#*) expression:'
			caption: 'Add wildcard pattern...'.
	(expression isNil or: [expression isEmpty]) ifTrue: [^self].

	(Smalltalk includesKey: #WildcardMatcher)
		ifTrue:
			[| matcher |
			matcher := ((Smalltalk at: #WildcardMatcher) pattern: expression)
						ignoreCase: true;
						mustMatchFromStart: false;
						mustMatchToEnd: false;
						yourself.
			filterPresenter model add: matcher]
		ifFalse:
			[| string |
			string := (String writeStream)
					nextPutAll: '[:it | ';
					print: ('*' , expression , '*');
					nextPutAll: ' match: it getSource]';
					contents.
			self addCompiledExpression: string].!

buildAddFilterMenu: aMenu
	"private -- this is invoked when the dynamic add filter menu is about to be displayed;
	update it appropriately"

	| command item |

	aMenu clear.		"we build it fresh each time"

	#(
		('Definitions of selector...' #addSelectorDefinitionsFilter)
		('References to symbol...' #addSelectorReferencesFilter)
		('References to class...' #addClassReferencesFilter)
		('References to global...' #addGlobalReferencesFilter)
		('Members of class...' #addClassMembersFilter)
		('Members of class and subclasses...' #addClassHierarchyMembersFilter)
		('Members of package...' #addPackageMembersFilter)
		('Members of category...' #addCategoryMembersFilter)
		('Members of protocol...' #addProtocolMembersFilter)
		('Methods containing source...' #addStringMatchFilter)
		('Methods matching wildcard pattern...' #addWildcardFilter)
"		('Methods matching regexp...' #addRegexpFilter)			-- too slow to be usable"
	) do:
		[:each |
		command := Message selector: each second.
		item := CommandMenuItem command: command description: each first.
		aMenu addItem: item].

	^ aMenu.
!

captionString
	"private -- answer a String to use as our caption"

	| all str sep |

	all := self enabledFilters.

	all isEmpty ifTrue: [^ allMode ifTrue: ['All methods'] ifFalse: ['No methods']].

	str := String writeStream.
	sep := allMode ifTrue: [' and '] ifFalse: [' or '].

	str nextPutAll: 'Methods relevant to: '.
	all
		do:[:each | str nextPutAll: ((self isInverted: each) ifTrue: ['not '] ifFalse: ['']); display: each]
		separatedBy: [str nextPutAll: sep].

	^ str contents.!

contentsOfDragSession: aSession
	"private -- answer the interesting Objects from aSession, if any"

	| got |

	got := filterPresenter model list asSet.

	^ (((aSession dragObjects select: [:each | each isFormatAvailable: #Object])
		collect: [:each | each object])
			reject: [:each | got includes: each]).

!

createComponents
	"create sub-presenters that they may be linked into MVP triads"

	filterPresenter := self add: ListPresenter new name: 'filters'.
	statusBar := self add: TextPresenter new name: 'status'.

	^ super createComponents.
!

createSchematicWiring
	"set up triggers between our components"
	
	filterPresenter
		when: #actionPerformed send: #toggleEnabled to: self;
		when: #drop: send: #onDropOverFilters: to: self;
		when: #dragOver: send: #onDragOverFilters: to: self.

	^ super createSchematicWiring.

!

disableFilter
	"command -- disable the selected filters"

	self setStateOfSelection: #disableFilter.!

enabledFilters
	"private -- badly named, answer a Collection of the filters that are not disabled"

	^ filterPresenter list reject: [:each | self isDisabled: each].!

enableFilter
	"command -- set the selected filters to enabled (as opposed to disabled or inverted)"

	self setStateOfSelection: #enableFilter.!

filterBlock
	"private -- answer a <monadicValuable> which can act as the 'summation' of
	the filter blocks corresponding to the selected filter objects"

	^ allMode
		ifTrue: [ [:method | filters allSatisfy: [:filter | filter value: method]] ]
		ifFalse: [ [:method | filters anySatisfy: [:filter | filter value: method]] ].!

filterBlockFor: anObject
	"private -- answer a boolean valued <monadicValuable> derived from anObject"

	| block |

	block := anObject isRelevantToMethodBlock.

	(self isInverted: anObject) ifTrue: [block := self invertBlock: block].

	^ block.!

help
	| locator file |

	#CUtodo.  "change the locator"
	locator := PackageRelativeFileLocator package: self class owningPackage.
	file := locator localFileSpecFor: 'Docs\FMB.html'.
	ShellLibrary default shellOpen: file.!

initialize
	"private -- establish a coherent intial state"

	filters := #().
	filterStates := IdentityDictionary new.
	allMode := true.

	^ super initialize.!

invertBlock: a1Block
	"private -- answer a block that inverts the sense of a1Block"

	^ [:it | (a1Block value: it) not].!

invertFilter
	"command -- set the selected filters to inverted"

	self setStateOfSelection: #invertFilter.!

isDisabled: anObject
	"answer whether the filter associated with anObject is disabled"

	^ (self stateOf: anObject) = #disableFilter.!

isInverted: anObject
	"answer whether the filter associated with anObject is inverted"

	^ (self stateOf: anObject) = #invertFilter.!

isNotDisabled: anObject
	"answer whether the filter associated with anObject is not disabled"

	^ (self stateOf: anObject) ~= #disableFilter.!

isNotInverted: anObject
	"answer whether the filter associated with anObject is not inverted"

	^ (self stateOf: anObject) ~= #invertFilter.!

methods
	"private -- answer a Collection of methods from our model (a class)
	which satisfy the filter represented by the set of selected filter objects"

	| all filter |

	"we don't use:
		^ (self model selectMethods: self filterBlock) asOrderedCollection.
	because it's *way* too slow (all that set manipulation to no purpose)"

	all := OrderedCollection new.
	filter := self filterBlock.
	self model environment allRoots do:
		[:root | root withAllSubclassesDo:
			[:class |
				all addAll: (class methodDictionary values select: filter).
				all addAll: (class class methodDictionary values select: filter)]].

	^ all.

			!

model: anObject
	"set our model to be the given object"

	super model: anObject.

	self
		onFiltersChanged;
		updateCaption.!

onAboutToDisplayMenu: aMenu
	"private -- this is invoked when aMenu is about to be popped-up;
	update it appropriately"

	| name |

	name := aMenu name.
	name == #dynamicAddFilterMenu ifTrue: [^ self buildAddFilterMenu: aMenu].

	^ super onAboutToDisplayMenu: aMenu.!

onDragOverFilters: aSession
	"private -- a d&d session is passing over the filters pane.
	Update the session accordingly"

	| op |

	(self contentsOfDragSession: aSession) isEmpty ifTrue: [^ self].

	op := aSession intendedOperation.
	op == #move ifTrue: [op := #copy].	"interpret move as copy"
	aSession operation: op.


!

onDropOverFilters: aSession
	"private -- a d&d session is attempting to drop on the filters pane.
	Update the session accordingly"

	| op |

	op := aSession operation.

	"we never do anything that should cause the source to 'cut' what was dropped"
	aSession operation: nil.

	"could possably accept #link too -- interpreting it as #remove"
	op == #copy ifFalse: [^ self].

	"note that we add objects in their capacities as #Objects.  We don't care what
	they are as long as they know how to supply a filter block"
	filterPresenter model addAll: (self contentsOfDragSession: aSession).
!

onFiltersChanged
	"private -- the set of selected filter objects has changed, update the
	displayed methods accordingly"

	self
		updateCaption;
		updateFilters.

	Cursor wait showWhile: [self browser
					filter: self filterBlock;
					list: self methods].

	self updateStatus.
!

onViewOpened
	"invoked as we go live"

	super onViewOpened.

	(filterPresenter view allColumns first)
		getImageBlock: [:it | self stateIconIndexOf: it];
		customDrawBlock: [:it | self ownerDrawFilter: it].

	self onFiltersChanged.

!

ownerDrawFilter: anNMLVCUSTOMDRAW
	"private -- called when Windows wants us to fill in the given NMLVCUSTOMDRAW object
	for an item in the filter list"

	| filter color |

	filter := anNMLVCUSTOMDRAW item.
	filter isNil ifTrue: [^ self].
	(self stateOf: filter) = #disableFilter ifFalse: [^ self].

	color := anNMLVCUSTOMDRAW forecolor.
	color := color fadedBy: ClassBrowserAbstract grayedMethodFadeFactor.
	anNMLVCUSTOMDRAW forecolor: color.
!

queryCommand: aCommandQuery
	"the usual..."

	super queryCommand: aCommandQuery.

	aCommandQuery command == #removeFilterObject ifTrue:
		[aCommandQuery isEnabled: filterPresenter hasSelection].

	aCommandQuery command == #setAllMode ifTrue:
		[aCommandQuery isChecked: allMode].

	aCommandQuery command == #setAnyMode ifTrue:
		[aCommandQuery isChecked: allMode not].

	(#(#enableFilter #disableFilter #invertFilter) includes: aCommandQuery command) ifTrue:
		[ | sel state |
		sel := filterPresenter selection.
		state := aCommandQuery command.
		aCommandQuery isChecked: (sel notEmpty and: [sel allSatisfy: [:each | (self stateOf: each) = state]]).
		aCommandQuery isEnabled: sel notEmpty].

	aCommandQuery command == #dynamicAddFilterMenu ifTrue:
		[aCommandQuery isEnabled: true].!

removeFilterObject
	"command -- remove the currently selected filter objects
	from the display"

	| sel |

	sel := filterPresenter selection.

	filterPresenter model removeAll: sel.
	sel do: [:each | filterStates removeKey: each ifAbsent: []].

	self onFiltersChanged.!

setAllMode
	"command -- set the display into ALL mode, in which all active filters
	are required to select each method for it to be included"

	allMode ifTrue: [^ self].

	allMode := true.
	self enabledFilters size = 1
		ifTrue: [self updateCaption]
		ifFalse: [self onFiltersChanged].!

setAnyMode
	"command -- set the display into ANY mode, in which any active filter
	that selects a method will allow it to be included"

	allMode ifFalse: [^ self].

	allMode := false.
	self enabledFilters size = 1
		ifTrue: [self updateCaption]
		ifFalse: [self onFiltersChanged].!

setStateOfSelection: aSymbol
	"private -- set the state of the selected filters to aSymbol"

	| sel |

	sel := filterPresenter selection.
	
	(sel allSatisfy: [:each | (self stateOf: each) = aSymbol]) ifTrue: [^ self].
	sel do: [:each | filterStates at: each put: aSymbol].

	filterPresenter view invalidate.

	self onFiltersChanged.!

stateIconIndexOf: anObject
	"private -- answer an Icon imageIndex corresponding to the state
	of the filter derived from anObject"

	| state name |

	state := self stateOf: anObject.

	name := state = #enableFilter
			ifTrue: ['Tick.ico']
			ifFalse: [state = #invertFilter
				ifTrue: ['InvertedTick.ico']
				ifFalse: ['NoTick.ico']].

	^ (self class icon: name) imageIndex.!

stateOf: anObject
	"answer what the status of the filter associated with
	anObject is.  It can be one of #(#disableFilter #enableFilter #invertFilter)"

	^ filterStates at: anObject ifAbsent: [#disableFilter].!

toggleEnabled
	"command -- cycle the selected filters around the possible states"

	| sel state |

	sel := filterPresenter selection.
	sel isEmpty ifTrue: [^ self].

	"we don't try to cycle them individually -- that'd be too confusing"
	state := self stateOf: sel first.
	state := state = #enableFilter
			ifTrue: [#invertFilter]
			ifFalse: [state = #invertFilter
				ifTrue: [#disableFilter]
				ifFalse: [#enableFilter]].

	self setStateOfSelection: state.!

updateCaption
	"private -- update our caption to display our filters"

	super caption: self captionString.!

updateFilters
	"private -- update our filters from the filter presenter and filter states"

	filters := self enabledFilters collect: [:each | self filterBlockFor: each].
!

updateStatus
	"private -- update our status line"

	| count text |

	count := self browser list size.
	text := count = 0
			ifTrue: ['No methods']
			ifFalse: [count = 1
				ifTrue: ['1 method']
				ifFalse: [count displayString , ' methods']].

	statusBar value: text.! !
!FilteringMethodBrowser categoriesFor: #addCategoryMembersFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassHierarchyMembersFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassHierarchyMembersFilter:!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassMembersFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassMembersFilter:!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassReferencesFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addClassReferencesFilter:!commands!public! !
!FilteringMethodBrowser categoriesFor: #addCompiledExpression!commands!public! !
!FilteringMethodBrowser categoriesFor: #addCompiledExpression:!commands!public! !
!FilteringMethodBrowser categoriesFor: #addGlobalReferencesFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addPackageMembersFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addProtocolMembersFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addSelectorDefinitionsFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addSelectorReferencesFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addStringMatchFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #addWildcardFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #buildAddFilterMenu:!menus!private! !
!FilteringMethodBrowser categoriesFor: #captionString!helpers!private! !
!FilteringMethodBrowser categoriesFor: #contentsOfDragSession:!helpers!private! !
!FilteringMethodBrowser categoriesFor: #createComponents!initializing!presenters!public! !
!FilteringMethodBrowser categoriesFor: #createSchematicWiring!event handling!initializing!public! !
!FilteringMethodBrowser categoriesFor: #disableFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #enabledFilters!accessing!private! !
!FilteringMethodBrowser categoriesFor: #enableFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #filterBlock!helpers!private! !
!FilteringMethodBrowser categoriesFor: #filterBlockFor:!helpers!private! !
!FilteringMethodBrowser categoriesFor: #help!commands!public! !
!FilteringMethodBrowser categoriesFor: #initialize!initializing!private! !
!FilteringMethodBrowser categoriesFor: #invertBlock:!helpers!private! !
!FilteringMethodBrowser categoriesFor: #invertFilter!commands!public! !
!FilteringMethodBrowser categoriesFor: #isDisabled:!public!testing! !
!FilteringMethodBrowser categoriesFor: #isInverted:!public!testing! !
!FilteringMethodBrowser categoriesFor: #isNotDisabled:!public!testing! !
!FilteringMethodBrowser categoriesFor: #isNotInverted:!public!testing! !
!FilteringMethodBrowser categoriesFor: #methods!helpers!private! !
!FilteringMethodBrowser categoriesFor: #model:!initializing!public! !
!FilteringMethodBrowser categoriesFor: #onAboutToDisplayMenu:!event handling!menus!private! !
!FilteringMethodBrowser categoriesFor: #onDragOverFilters:!event handling!private! !
!FilteringMethodBrowser categoriesFor: #onDropOverFilters:!event handling!private! !
!FilteringMethodBrowser categoriesFor: #onFiltersChanged!event handling!private! !
!FilteringMethodBrowser categoriesFor: #onViewOpened!event handling!initializing!public! !
!FilteringMethodBrowser categoriesFor: #ownerDrawFilter:!helpers!private! !
!FilteringMethodBrowser categoriesFor: #queryCommand:!commands!public! !
!FilteringMethodBrowser categoriesFor: #removeFilterObject!commands!public! !
!FilteringMethodBrowser categoriesFor: #setAllMode!commands!public! !
!FilteringMethodBrowser categoriesFor: #setAnyMode!commands!public! !
!FilteringMethodBrowser categoriesFor: #setStateOfSelection:!commands!public! !
!FilteringMethodBrowser categoriesFor: #stateIconIndexOf:!accessing!private! !
!FilteringMethodBrowser categoriesFor: #stateOf:!accessing!public! !
!FilteringMethodBrowser categoriesFor: #toggleEnabled!commands!public! !
!FilteringMethodBrowser categoriesFor: #updateCaption!private!updating! !
!FilteringMethodBrowser categoriesFor: #updateFilters!private!updating! !
!FilteringMethodBrowser categoriesFor: #updateStatus!private!updating! !

!FilteringMethodBrowser class methodsFor!

displayOn: aStream
	"append a user-centric representation of ourselves to aStream"

	aStream nextPutAll: 'Filtering Method Browser'.!

icon
	"answers an Icon that can be used to represent this class"

	^ self icon: self defaultIconName.!

icon: aString
	"answers an Icon from the package-relative name, aString"

	"this will have to be recompiled if the package we live in changes its name"
	^ (Smalltalk at: #Icon)
		fromFile: aString
		usingLocator: (PackageResourceLocator packageNamed: ##(self owningPackage name)).
!

initialize
	"private -- class-side initalisation.

		self initialize.
	"

	"register ourselves as an 'additional tool'"
	Smalltalk developmentSystem
		addAdditionalToolsFolderIcon: self toolsFolderIcon;
		registerTool: self.

	self reuseIfOpen: false.
!

uninitialize
	"private -- class tear-down.

		self uninitialize.
	"

	Smalltalk developmentSystem
		removeSystemFolderIconNamed: self toolDescription;
		unregisterTool: self.! !
!FilteringMethodBrowser class categoriesFor: #displayOn:!displaying!public! !
!FilteringMethodBrowser class categoriesFor: #icon!constants!public! !
!FilteringMethodBrowser class categoriesFor: #icon:!constants!public! !
!FilteringMethodBrowser class categoriesFor: #initialize!initializing!private! !
!FilteringMethodBrowser class categoriesFor: #uninitialize!class hierarchy-removing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: FilteringMethodBrowser name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: '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
AAAATG9jYWwgRGVmaW4maXRpb25zIG9mugAAAAAAAABSAAAAFAAAAGxvY2FsRGVmaW5pdGlvbnNN
ZW514hYAAAAAAAAAAAAAEAAAAGIAAAAAAAAAUgAAABQAAAAmTG9jYWwgUmVmZXJlbmNlcyB0b7oA
AAAAAAAAUgAAABMAAABsb2NhbFJlZmVyZW5jZXNNZW51EhcAAAAAAAABAAAAsgoAAAAAAAC6AAAA
AAAAAFIAAAAcAAAAYnJvd3NlTWV0aG9kSW5oZXJpdGFuY2VDaGFpblIAAAASAAAASW4maGVyaXRh
bmNlIENoYWluAQAAAAEAAAAAAAAAAAAAAAAAAAASFwAAAAAAAAEAAACyCgAAAAAAALoAAAAAAAAA
UgAAABUAAABicm93c2VJbmhlcml0ZWRNZXRob2RSAAAAFgAAACZTdXBlcmNsYXNzIERlZmluaXRp
b24BAAAAAQAAAAAAAAAAAAAAAAAAAIIYAAAAAAAAARAAABIXAAAAAAAAAQAgALIKAAAAAAAAugAA
AAAAAABSAAAAEQAAAGJyb3dzZU1ldGhvZENsYXNzUgAAAAoAAAAmQnJvd3NlICUxAQAAAAEAAAAA
AAAAAAAAAAAAAAASFwAAAAAAAAEAAACyCgAAAAAAALoAAAAAAAAAUgAAABMAAABicm93c2VNZXRo
b2RQYWNrYWdlUgAAABIAAABCcm8md3NlICUxIFBhY2thZ2UBAAAAAQAAAAAAAAAAAAAAAAAAAFIA
AAAHAAAAJk1ldGhvZAAAAAAAAAAAAAAAAAAWAAAAAAAAggAAAAgAAADnA///AAAAAAYCBwBNZXNz
YWdlAAAAALoAAAAAAAAAUgAAAAgAAABzZWxlY3RvcmIAAAAAAAAAQBEAAIARAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAAEYMDgAFAAAATGlzdFZpZXdDb2x1bW4A
AAAAUgAAAAgAAABTZWxlY3RvcisBAACwCAAAAiAAAAAAAAAgIAAAYgAAAAAAAAAGBAwAQmxvY2tD
bG9zdXJlAAAAACYDDQBNZXRob2RDb250ZXh0AgAAAAEAAAAmBRIAQ29tcGlsZWRFeHByZXNzaW9u
AQAAAI8CAACqAAAAAAAAALAVAABSAAAAEAAAAFNvcnRlZENvbGxlY3Rpb25SAAAABAAAAGRvSXRi
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'))!

