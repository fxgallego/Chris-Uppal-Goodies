| package |
package := Package name: 'CU Browser Database'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

This builds and maintains a cross-reference database of which methods refer to which, and of which selectors are defined anywhere.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.04'.

package basicScriptAt: #postinstall put: 'BrowserDatabase checkSystemEvents.
!!'.

package classNames
	add: #BrowserDatabase;
	add: #OverridenMethodCategory;
	add: #UndefinedSendsCategory;
	add: #UnusedMethodsCategory;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Storage Size';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package setManualPrerequisites: #(
	'CU Storage Size').

package!

"Class Definitions"!

VirtualMethodCategory subclass: #OverridenMethodCategory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VirtualMethodCategory subclass: #UndefinedSendsCategory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VirtualMethodCategory subclass: #UnusedMethodsCategory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Model subclass: #BrowserDatabase
	instanceVariableNames: 'system definedSelectors referredToSelectors noisy state todo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

OverridenMethodCategory guid: (GUID fromString: '{49F611F9-8D19-426C-903E-7AE1D8A78B89}')!
OverridenMethodCategory comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

A virtual method category that represents methods that are overridden in subclasses.'!
!OverridenMethodCategory categoriesForClass!Unclassified! !
!OverridenMethodCategory methodsFor!

includesMethod: aMethod
	"answer whether the receiver includes the <CompiledMethod>, method."

	[| analyser |
	analyser := BrowserDatabase current.
	^ analyser isEnabled and: [analyser isMethodOverriden: aMethod]]
		on: Error
		do: [:error | ^ false].
! !
!OverridenMethodCategory categoriesFor: #includesMethod:!public!testing! !

!OverridenMethodCategory class methodsFor!

initialize
	"private -- initialize the receiver.

		self initialize.
	"
	self addPseud: (self newNamed: self pseudName).
!

pseudName
	"private -- answer the name we will install ourselves as"

	^ self pseudPrefix , 'overridden'.!

uninitialize
	"private -- uninitialize the receiver as it is about to be removed from the system.

		self uninitialize.
	"

	self removePseud: self pseudName.! !
!OverridenMethodCategory class categoriesFor: #initialize!initializing!private! !
!OverridenMethodCategory class categoriesFor: #pseudName!constants!private! !
!OverridenMethodCategory class categoriesFor: #uninitialize!class hierarchy-removing!private! !

UndefinedSendsCategory guid: (GUID fromString: '{2410D137-4367-4CF9-A822-8779F39184CA}')!
UndefinedSendsCategory comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

A virtual method category that represents methods that send undefined messages.'!
!UndefinedSendsCategory categoriesForClass!Unclassified! !
!UndefinedSendsCategory methodsFor!

includesMethod: aMethod
	"answer whether the receiver includes the <CompiledMethod>, method."

	[| analyser |
	analyser := BrowserDatabase current.
	^ analyser isEnabled and: [analyser doesMethodHaveUndefinedSends: aMethod]]
		on: Error
		do: [:error | ^ false].
! !
!UndefinedSendsCategory categoriesFor: #includesMethod:!public!testing! !

!UndefinedSendsCategory class methodsFor!

initialize
	"private -- initialize the receiver.

		self initialize.
	"
	self addPseud: (self newNamed: self pseudName).
!

pseudName
	"private -- answer the name we will install ourselves as"

	^ self pseudPrefix , 'with undefined sends'.!

uninitialize
	"private -- uninitialize the receiver as it is about to be removed from the system.

		self uninitialize.
	"

	self removePseud: self pseudName.! !
!UndefinedSendsCategory class categoriesFor: #initialize!initializing!private! !
!UndefinedSendsCategory class categoriesFor: #pseudName!constants!private! !
!UndefinedSendsCategory class categoriesFor: #uninitialize!class hierarchy-removing!private! !

UnusedMethodsCategory guid: (GUID fromString: '{D16FA4C1-7143-11D5-8727-A78243202A3E}')!
UnusedMethodsCategory comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

A virtual method category that represents methods that are not referred to by other methods.'!
!UnusedMethodsCategory categoriesForClass!Unclassified! !
!UnusedMethodsCategory methodsFor!

includesMethod: aMethod
	"answer whether the receiver includes the <CompiledMethod>, method."

	[| analyser |
	analyser := BrowserDatabase current.
	^ analyser isEnabled and: [(analyser isMethodReferredTo: aMethod) not]]
		on: Error
		do: [:error | ^ false].
! !
!UnusedMethodsCategory categoriesFor: #includesMethod:!public!testing! !

!UnusedMethodsCategory class methodsFor!

initialize
	"private -- initialize the receiver
		self initialize.
	"
	self addPseud: (self newNamed: self pseudName).
!

pseudName
	"private -- answer the name we will install ourselves as"

	^ self pseudPrefix , 'not referred to'.!

uninitialize
	"private -- uninitialize the receiver as it is about to be removed from the system
		self uninitialize.
	"

	self removePseud: self pseudName.! !
!UnusedMethodsCategory class categoriesFor: #initialize!initializing!private! !
!UnusedMethodsCategory class categoriesFor: #pseudName!constants!private! !
!UnusedMethodsCategory class categoriesFor: #uninitialize!class hierarchy-removing!private! !

BrowserDatabase guid: (GUID fromString: '{2E0A1A6F-298C-49E5-8A18-4FD430C6ADAB}')!
BrowserDatabase comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

This builds and maintains a database of which methods refer to which, and of which selectors are defined anywhere.

This makes it possible to implement categories like ''not referred to'', and ''with undefined sends'', since we can check the status in a few microseconds.

The downside is that it takes up about a megabyte of extra image space (on my machine, obviously it''ll depend on how much stuff you have loaded into the image).  Inspecting:

	self current stats.

will tell you more about how many resources the database is consuming.
'!
!BrowserDatabase categoriesForClass!Unclassified! !
!BrowserDatabase methodsFor!

action: a0Block
	"private -- either evaluate a block now or, if we are paused, save it on the todo list
	for later"

	state == #Paused
		ifTrue: [todo addLast: a0Block]
		ifFalse: [a0Block value].

	self trigger: #statisticsChanged.!

addClass: aClass
	"private -- add aClass to our cache"

	noisy ifTrue: [aClass traceWith: 'Adding class to Browser Database'].

	aClass selectors do: [:each | self addMethod: (aClass compiledMethodAt: each)].
!

addMethod: aMethod
	"private -- add aMethod to our cache"
	| prev |

	prev := self addMethodDefinition: aMethod.

	noisy ifTrue: [aMethod traceWith: (prev isNil ifTrue: ['Adding method to Browser Database'] ifFalse: ['Replacing method in Browser Database'])].

	prev isNil ifFalse: [self removeReferencesFromMethod: prev].
	self addReferencesFromMethod: aMethod.
!

addMethodDefinition: aMethod
	"private -- record the definition of aMethod; answer the previous version
	or nil"
	| defs selector class |

	"NB: horrid hackery to avoid too many sub-collections; we
	don't build a Collection until we've got > 1 elements"

	selector := aMethod selector.
	class := aMethod methodClass.

	defs := definedSelectors at: selector ifAbsentPut: [aMethod].
	defs == aMethod ifTrue: [^ nil].

	"defs may just be a single method, not a collection thereof"
	(defs isKindOf: CompiledMethod) ifTrue:
		[| prev |
		prev := defs.
		(prev methodClass = class)
			ifTrue: [defs := aMethod]
			ifFalse: [defs := Array with: prev with: aMethod. prev := nil].
		definedSelectors at: selector put: defs.
		^ prev].

	"try to find, overwrite, and answer, the previous version"	
	defs keysAndValuesDo:
		[:i :each | each methodClass = class ifTrue:
			[defs at: i put: aMethod.
			^ each]].

	"it wasn't there, just add it"
	definedSelectors at: selector put: (defs copyWith: aMethod).
	^ nil.!

addReferencesFromMethod: aMethod
	"private -- remember which selectors aMethod refers to"

	aMethod allSymbolLiterals do: [:each | self addSelectorReference: each method: aMethod].
!

addSelectorReference: aSelector method: aMethod
	"private -- remember that aMethod refers to aSelector"
	| refs |

	"NB: horrid hackery to avoid too many sub-collections; we
	don't build a Collection until we've got > 1 elements"

	refs := referredToSelectors at: aSelector ifAbsentPut: [aMethod].
	refs == aMethod ifTrue: [^ self].

	"refs may just be a single method, not a collection thereof"
	refs := (refs isKindOf: CompiledMethod)
			ifTrue: [Array with: refs with: aMethod]
			ifFalse: [refs copyWith: aMethod].

	referredToSelectors at: aSelector put: refs.
!

allDefinedSelectors
	"answer a Set of all the selectors that are defined anywhere"

	^ definedSelectors keys.

!

allDefinitionsDo: a2Block
	"evaluate a2Block for each of the methods we know about.
	Answers the result of the last evaluation"

	^ definedSelectors keysAndValuesDo:
		[:selector :defs |
		(defs isKindOf: CompiledMethod)
			ifTrue: [a2Block value: selector value: defs]
			ifFalse: [defs do: [:each | a2Block value: selector value: each]]].
!

allReferencesDo: a2Block
	"evaluate a2Block for each of the methods we know about.
	Answers the result of the last evaluation"

	^ referredToSelectors keysAndValuesDo:
		[:selector :refs |
		(refs isKindOf: CompiledMethod)
			ifTrue: [a2Block value: selector value: refs]
			ifFalse: [refs do: [:each | a2Block value: selector value: each]]].
!

allReferredToSelectors
	"answer a Set of all the selectors that are referred to anywhere"

	^ referredToSelectors keys.
!

allSelectors
	"answer a Set of all the selectors that we know about"

	^ self allReferredToSelectors union: self allDefinedSelectors.
!

beDisabled
	"ensure that our database is purged and that we are not listening for events"

	self
		declineEvents;
		discardTodoList;
		discardTables.
	self setState: #Disabled.!

bePaused
	"ensure that we are currently trapping events as they occur but instead of updating
	our databases, we are just stuffing the events into our todo list.  Note will rebuild the
	database first if we don't already have one"

	self isTrappingEvents ifFalse: [self rebuildTables; solicitEvents].
	state == #Paused ifFalse: [self discardTodoList].
	self setState: #Paused.!

beRunning
	"ensure that we are currently trapping events as they occur and that all our stored
	(from when/if we were #Paused) actions have been applied"

	self isTrappingEvents ifFalse: [self rebuildTables; solicitEvents].
	state == #Paused ifTrue: [self executeTodoList].
	self setState: #Running.
!

beStopped
	"ensure that we are currently ignoring any events and have no pending list, and
	that our data is correct at the time we enter this state."

	self isTrappingEvents ifFalse: [self rebuildTables].
	state == #Paused ifTrue: [self executeTodoList].
	self declineEvents.
	self setState: #Stopped.!

check
	"create a new database and get it to check ourself  Reports any differences
	to the Transcript.
	Answers true if there were no discrepancies.
	Warning: very slow..."

	^ self checkCurrency & ((self class for: system state: #Stopped) check: self).!

check: aBrowerDatabase
	"check the validity of aBrowserDatabase against the data
	we contain.  Reports any differences to the Transcript.
	Answers true if there were no discrepancies.
	Warning: very slow..."

	| ours theirs ok |

	ok := true.

	ours := self allDefinedSelectors.
	theirs := aBrowerDatabase allDefinedSelectors.
	(theirs - ours) asSortedCollection do: [:each | ok := false. each traceWith: 'Added definition of selector'].
	(ours - theirs) asSortedCollection do: [:each | ok := false. each traceWith: 'Lost definition of selector'].
	(ours union: theirs) asSortedCollection do:
		[:selector |
		ours := (self definitionsOf: selector) asSet.
		theirs := (aBrowerDatabase definitionsOf: selector) asSet.
		(ours equals: theirs) ifFalse:
			[ok := false. 
			selector traceWith: 'Changed definitions of'.
			(theirs - ours) asSortedCollection traceWith: '	Added'.
			(ours - theirs) asSortedCollection traceWith: '	Lost']].


	ours := self allReferredToSelectors.
	theirs := aBrowerDatabase allReferredToSelectors.
	(theirs - ours) asSortedCollection do: [:each | ok := false. each traceWith: 'Added referred to selector'].
	(ours - theirs) asSortedCollection do: [:each | ok := false. each traceWith: 'Lost referred to selector'].
	(ours union: theirs) asSortedCollection do:
		[:selector |
		ours := (self referencesTo: selector) asSet.
		theirs := (aBrowerDatabase referencesTo: selector) asSet.
		(ours equals: theirs) ifFalse:
			[ok := false. 
			selector traceWith: 'Changed references to'.
			(theirs - ours) asSortedCollection traceWith: '	Added'.
			(ours - theirs) asSortedCollection traceWith: '	Lost']].

	^ ok.!

checkAllMethods
	"nasty, since Dolphin is in the habit of replacing methods without telling
	us, this just runs through all the methods we know about, and for each
	that has been surreptitiously changed, will replace our old idea of it, by
	our new idea"

	| replace add remove |

	"look to see which methods in our defs table are no longer current"
	replace := IdentityDictionary new.
	self allDefinitionsDo:
		[:selector :old || new |
		new := self currentVersionOfMethod: old.		"may be nil"
		new == old ifFalse: [replace at: old put: new]].

	"see which methods are still OK in the sense that their old analysis still applies"
	add := OrderedCollection new.
	remove := OrderedCollection new.
	replace keysAndValuesDo:
		[:old :new |
		(self isMethod: old equivalentTo: new) ifFalse:
			[add add: new.
			remove add: old]].
	replace removeAllKeys: remove.

	"update to use the new versions"
	remove do: [:each | self removeMethod: each].
	add do: [:each | each isNil ifFalse: [self addMethod: each]].
	replace isEmpty ifFalse:
		[self updateDefinitionsUsing: replace.
		self updateReferencesUsing: replace].!

checkCurrency
	"answer whether our database contains any methods which are not 'current';
	Writes any discrepancies to the Transcript"

	| ok classes |

	ok := true.

	classes := IdentitySet new.

	self allDefinitionsDo:
		[:selector :method || new |
		classes add: method methodClass.
		new := self currentVersionOfMethod: method.
		method == new ifFalse:
			[ok := false.
			method traceWith: 'Method is not current']].

	classes := classes do: [:each | each == (self currentVersionOfClass: each) ifFalse:
			[ok := false.
			each traceWith: 'Class is not current']].

	^ ok.!

classesDefining: aSelector
	"answer a Set of all the classes which define aSelector"

	^ (self definitionsOf: aSelector) collect: [:each | each methodClass].!

countDefinitionsOf: aSelector
	"answer a count of all the methods with aSelector"

	| defs |

	defs := definedSelectors at: aSelector ifAbsent: [^ 0].
	^ (defs isKindOf: CompiledMethod)
		ifTrue: [1]
		ifFalse: [defs size].!

countDefinitionsOf: aSelector satisfying: a1Block
	"answer a count of all the methods with aSelector and for which
	a1Block evaluates to true"

	| defs |

	defs := 0.
	self definitionsOf: aSelector satisfying: a1Block do: [:each | defs := defs + 1].

	^ defs.

!

countReferencesTo: aSelector
	"answer a count of all the methods which refer to aSelector"
	| refs |

	refs := referredToSelectors at: aSelector ifAbsent: [^ 0].
	^ (refs isKindOf: CompiledMethod)
		ifTrue: [1]
		ifFalse: [refs size].
!

countReferencesTo: aSelector satisfying: a1Block
	"answer a count of all the methods which refer to aSelector and for which
	a1Block evaluates to true"

	| refs |

	refs := 0.
	self referencesTo: aSelector satisfying: a1Block do: [:each | refs := refs + 1].

	^ refs.
!

currentVersionOfClass: aClass
	"private -- answer the current version of aClass, or nil if it is no
	longer defined"

	| class |

	aClass isMeta ifFalse:
		[^ system environment at: aClass name ifAbsent: [nil]].

	class := self currentVersionOfClass: aClass instanceClass.
	^ class isNil
		ifTrue: [nil]
		ifFalse: [class class].
!

currentVersionOfMethod: aMethod
	"private -- answer the current version of aMethod, or nil if it is no
	longer defined"

	| class |

	class := self currentVersionOfClass: aMethod methodClass.
	^ class isNil
		ifTrue: [nil]
		ifFalse: [class compiledMethodAt: aMethod selector ifAbsent: [nil]].
!

declineEvents
	"private -- unhook ourselves from the system"

	system removeEventsTriggeredFor: self.!

definitionsOf: aSelector
	"answer a list of all the methods with aSelector"

	| defs |

	defs := OrderedCollection new.
	self definitionsOf: aSelector do: [:each | defs add: each].

	^ defs.
!

definitionsOf: aSelector do: a1Block
	"evaluate a1Block for each of the methods with aSelector.
	Answers the result of the last evaluation"

	| defs |

	defs := definedSelectors at: aSelector ifAbsent: [^ nil].
	^ (defs isKindOf: CompiledMethod)
		ifTrue: [a1Block value: defs]
		ifFalse: [defs do: a1Block].
!

definitionsOf: aSelector satisfying: a1Block
	"answer a list of all the methods with aSelector and for which
	a1Block evaluates to true"

	| defs |

	defs := OrderedCollection new.
	self definitionsOf: aSelector satisfying: a1Block do: [:each | defs add: each].

	^ defs.
!

definitionsOf: aSelector satisfying: aDiscriminator do: a1Block
	"evaluate a1Block for each of the methods with aSelector and for which
	aDiscriminator evaluates to true"

	self definitionsOf: aSelector do: [:each | (aDiscriminator value: each) ifTrue: [ a1Block value: each]].
!

discardTables
	"private -- discard any stored data"

	definedSelectors := IdentityDictionary new: 0.
	referredToSelectors := IdentityDictionary new: 0.
	self trigger: #statisticsChanged.
!

discardTodoList
	"private -- discard any queue of actions waiting to be performed"

	todo := OrderedCollection new.
	self trigger: #statisticsChanged.
!

doesMethodHaveUndefinedSends: aMethod
	"answer true iff any of the selectors in aMethod are nowhere defined"

	^ aMethod messages anySatisfy: [:each | (self isSelectorDefined: each) not].
!

executeTodoList
	"private -- execute actions waiting to be performed"

	todo do: [:each | each value].
	self discardTodoList.!

initialize
	"private -- establish a coherent initial state"

	noisy := false.
	self beDisabled.

!

isDisabled
	"answer whether we are currently disabled -- i.e. we contain no data and listen for no
	events"

	^ state == #Disabled.!

isEnabled
	"answer whether we are currently enabled (in which case we have at least a pasing resemblance
	to the reality, though it will be going out-of-date with each change unless we are also #Running)"

	^ state ~~ #Disabled.!

isMethod: aMethod equivalentTo: aMethodOrNil
	"private -- answer whether the new version of a method, which may be nil,
	is equivalent to the old one.  Note that we 'know' that the classes don't need
	to be compared, and that they have the same selectors"

	aMethodOrNil isNil ifTrue: [^ false].

	aMethod literalCount = aMethodOrNil literalCount ifFalse: [^ false].
	aMethod byteCodes = aMethodOrNil byteCodes ifFalse: [^ false].
	1 to: aMethod literalCount do:
		[:i |
		(aMethod literalAt: 1) = (aMethodOrNil literalAt: 1) ifFalse: [^ false]].

	^ true.
!

isMethodOverriden: aMethod
	"answer true iff aMethod is overriden in a subclass"

	| class |

	class := aMethod methodClass.

	self
		definitionsOf: aMethod selector
		satisfying: [:each | each methodClass inheritsFrom: class]
		do: [:each | ^ true].

	^ false.!

isMethodReferredTo: aMethod
	"answer true iff no *other* method refers to aMethod's selector"

	^ self isSelectorReferredToExternally: aMethod selector.
!

isPaused
	"answer whether we are currently remembering changes as they occur, but not applying
	them to the database"

	^ state == #Paused.!

isRunning
	"answer whether we are currently applying changes to methods as they occur"

	^ state == #Running.
!

isSelectorDefined: aSelector
	"answer true iff at least one class defines aSelector"

	^ definedSelectors includesKey: aSelector.!

isSelectorReferredTo: aSelector
	"answer true iff at least one method refers to aSelector"

	^ referredToSelectors includesKey: aSelector.!

isSelectorReferredToExternally: aSelector
	"answer true iff at least one method refers to aSelector, excluding methods
	which have that selector themselves"
	| refs |

	refs := referredToSelectors at: aSelector ifAbsent: [^ false].
	^ (refs isKindOf: CompiledMethod)
		ifTrue: [refs selector ~= aSelector]
		ifFalse: [refs anySatisfy: [:each | each selector ~= aSelector]].
!

isStopped
	"answer whether we currently are ignoring changes as they occur, although we may still
	have some old data available"

	^ state == #Stopped.!

isTrappingEvents
	"private -- answer whether we are currently Obseving changes to methods
	and classes as they happen"

	^ #(#Running #Paused) includes: state.!

methodsOverriding: aMethod
	"answer an OrderedCollection of all the overrides of aMethod"

	| class methods |

	class := aMethod methodClass.
	methods := OrderedCollection new.

	self
		definitionsOf: aMethod selector
		satisfying: [:each | each methodClass inheritsFrom: class]
		do: [:each | methods add: each].

	^ methods.!

noisy
	"private -- answer whether we trace method/class changes to the transcript"

	^ noisy.!

noisy: aBool
	"private -- set whether we trace method/class changes to the transcript"

	noisy := aBool.!

onClassAdded: aClass
	"private -- called when a new class is added to the system"

	self action: [self addClass: aClass; addClass: aClass class].

!

onClassRemoved: aClass
	"private -- called when a class is removed from the system"

	self action: [self removeClass: aClass; removeClass: aClass class].
!

onClassUpdated: aClass
	"private -- called when a class is changed"

	"this is annoying, Dolphin is quite happy to change a class, posibly #become:ing
	it to the new class, possibly not, and then recompile all of its methods *without*
	triggering change notification for the old ones dying, or the new ones being born.
	So all we can do is check *every single* method (since we don't know whether the
	class we've just been told about is the same as the one we needed to be told about
	(and don't forget that this applies to subclass methods too)).  Since we are assuming
	that no new selectors have been added to any class (only the actual method objects
	changed in the method dicts), we don't need to refresh completely, which is just as
	well because that would be *really* slow"
	#CUtodo.  "the above comment is no longer accurate -- should code to take advantage"
	#CUtodo.  "if we're in #Paused mode then there's no need to enqueue this more than once"
	self action: [self checkAllMethods].!

onMethodAdded: aCompilationResult
	"private -- called when a new method is added to the system"

	self action: [self addMethod: aCompilationResult method].
!

onMethodRemoved: aMethod
	"private -- called when a method is removed from the system"

	self action: [self removeMethod: aMethod].
!

onMethodUpdated: aCompilationResult
	"private -- called when a an existing method is redefined"

	#CUtodo. "should take advantage of CompilationResult>>oldMethod"
	self action: [self addMethod: aCompilationResult method].!

printOn: aStream
	"append a developer-friendly representation of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' (';
		display: state;
		nextPutAll: ')'.!

quietlyDo: a0Block
	"private -- answer the result of evaluating a0Block with 'noisy' turned off"

	^ noisy
		ifTrue: [noisy := false, a0Block ensure: [noisy := true]]
		ifFalse: [a0Block value].!

rebuildTables
	"private -- replace any data that we hold with stuff newly got from the system"

	| size |

	size := Symbol allInstances size.
	definedSelectors := IdentityDictionary new: size.
	referredToSelectors := IdentityDictionary new: size.
	system environment allBehaviorsDo: [:each | self addClass: each].
	self shrink.!

referencesTo: aSelector
	"answer a list of all the methods which refer to aSelector"

	| refs |

	refs := OrderedCollection new.
	self referencesTo: aSelector do: [:each | refs add: each].

	^ refs.
!

referencesTo: aSelector do: a1Block
	"evaluate a1Block for each of the methods which refer to aSelector.
	Answers the result of the last evaluation"

	| refs |

	refs := referredToSelectors at: aSelector ifAbsent: [^ nil].
	^ (refs isKindOf: CompiledMethod)
		ifTrue: [a1Block value: refs]
		ifFalse: [refs do: a1Block].
!

referencesTo: aSelector satisfying: a1Block
	"answer a list of all the methods which refer to aSelector and for which
	a1Block evaluates to true"

	| refs |

	refs := OrderedCollection new.
	self referencesTo: aSelector satisfying: a1Block do: [:each | refs add: each].

	^ refs.
!

referencesTo: aSelector satisfying: aDiscriminator do: a1Block
	"evaluate a1Block for each of the methods which refer to aSelector and for which
	aDiscriminator evaluates to true"

	self referencesTo: aSelector do: [:each | (aDiscriminator value: each) ifTrue: [ a1Block value: each]].
!

removeClass: aClass
	"private -- remove aClass from our cache.  NB: this doesn't include the metaclass"

	noisy ifTrue: [aClass traceWith: 'Removing class from Browser Database'].

	aClass selectors do: [:each | self removeMethod: (aClass compiledMethodAt: each)].
!

removeMethod: aMethod
	"private -- remove aMethod from our cache"

	noisy ifTrue: [aMethod traceWith: 'Removing method from Browser Database'].

	self removeMethodDefinition: aMethod.
	self removeReferencesFromMethod: aMethod.
!

removeMethodDefinition: aMethod
	"private -- forget the definition of aMethod"
	| defs class selector |

	"NB: horrid hackery to avoid too many sub-collections; we
	don't build a Collection until we've got > 1 elements"

	selector := aMethod selector.
	class := aMethod methodClass.

	defs := definedSelectors at: selector ifAbsent: [^ self].

	"defs may just be a single method, not a collection thereof"
	(defs isKindOf: CompiledMethod) ifTrue:
		[defs methodClass = class ifTrue: [definedSelectors removeKey: selector].
		^ self].

	defs := defs reject: [:each | each methodClass = class].
	defs isEmpty ifTrue: [definedSelectors removeKey: selector. ^ self].
	defs size = 1 ifTrue: [defs := defs first].
	definedSelectors at: selector put: defs.
!

removeReferencesFromMethod: aMethod
	"private -- forget that aMethod refers to any Selector"

	aMethod allSymbolLiterals do: [:each | self removeSelectorReference: each method: aMethod].
!

removeSelectorReference: aSelector method: aMethod
	"private -- forget that aMethod refers to aSelector"
	| refs |

	"NB: horrid hackery to avoid too many sub-collections; we
	don't build a Collection until we've got > 1 elements"

	refs := referredToSelectors at: aSelector ifAbsent: [^ self].

	"refs may just be a single method, not a collection thereof"
	(refs isKindOf: CompiledMethod) ifTrue:
		[refs = aMethod ifTrue: [referredToSelectors removeKey: aSelector].
		^ self].

	refs := refs reject: [:each | each = aMethod].
	refs isEmpty ifTrue: [referredToSelectors removeKey: aSelector. ^ self].
	refs size = 1 ifTrue: [refs := refs first].
	referredToSelectors at: aSelector put: refs.
!

reset
	"force us to discard all our data, and refresh it in case it has become stale"

	| oldState |

	oldState := state.
	self quietlyDo: [self disable; state: oldState].!

rootClassesDefining: aSelector
	"answer a Set of all the classes which define aSelector, eliminating any classes which only
	override a definition in a superclass"

	| classes |

	classes := self classesDefining: aSelector.

	^ classes reject: [:each | classes anySatisfy: [:root | each inheritsFrom: root]].!

selectorsDefinedInPackageAndPrerequisites: aPackage
	"answer a Set of all the selectors defined by aPackage and its prerequisites"

	| all |

	all := aPackage allPrerequisites.
	all add: aPackage.

	^ self selectorsDefinedInPackages: all.!

selectorsDefinedInPackages: aCollectionOfPackages
	"answer a Set of all the selectors defined by the given Collection of packages"

	| all |

	all := IdentitySet new.

	aCollectionOfPackages do: [:package | package allMethods do: [:each | all add: each selector]].

	^ all.!

selectorsReferredToByPackage: aPackage
	"answer a Set of all the selectors referred to by the given package"

	| all |

	all := IdentitySet new.

	aPackage allMethods do: [:each | all addAll: each allSymbolLiterals].

	^ all.
!

setState: aSymbol
	"private -- remember that our state is now given by aSymbol, and also tell any Observers"

	state := aSymbol.

	self trigger: #stateChanged.
	self trigger: #statisticsChanged.!

shrink
	"private -- shrink our tables to their optimum sizes"

	definedSelectors shrink.
	referredToSelectors shrink.
	self trigger: #statisticsChanged.

!

solicitEvents
	"private -- hook ourselves up as an Observer of the system"

	"NB: this should be kept in synch with (class side) #wantedSystemEvents"

	system
		when: #classAdded: send: #onClassAdded: to: self;
		when: #classUpdated: send: #onClassUpdated: to: self;
		when: #classRemoved: send: #onClassRemoved: to: self;
		when: #methodAdded: send: #onMethodAdded: to: self;
		when: #methodUpdated: send: #onMethodUpdated: to: self;
		when: #methodRemoved: send: #onMethodRemoved: to: self;
		yourself.
	
!

state
	"answer one of  #Running #Paused #Stopped #Disabled according as we are
		Running:	normal operation, trapping method changes as they happen.
		Paused:	trapping method changes but enqueing them to be actioned later.
		Stopped:	ignoring method changes but still holding onto our database.
		Disabled:	same as Stopped but also with no database.
	"

	^ state.!

state: aSymbol
	"set our state to one of  #Running #Paused #Stopped #Disabled"

	aSymbol == #Running ifTrue: [^ self beRunning].
	aSymbol == #Paused ifTrue: [^ self bePaused].
	aSymbol == #Stopped ifTrue: [^ self beStopped].
	aSymbol == #Disabled ifTrue: [^ self beDisabled].
!

stats
	"answer an IdentityDictionary with the following elements:
		#State				-- our current state (see #state)
		#SizeInBytes			-- total size taken up by this instance's database
		#DefinitionRecords		-- number of definition records we hold
		#DefinitionBytes		-- total size of the definition records we hold
		#DefinitionTableLoad	-- hash table load of the definitions table
		#DefinitionTableAPE		-- average probes per element for the definitions table
		#ReferenceRecords		-- number of reference records we hold
		#ReferenceBytes		-- total size of the reference records we hold
		#ReferenceTableLoad	-- hash table load of the references table
		#ReferenceTableAPE	-- average probes per element for the references table
		#PendingQueueLength	-- length of queue of pending updates (0 if not #Paused)

	self current stats.
	"

	| defs defBytes refs refBytes |

	defs := 0.
	defBytes := definedSelectors storageSize + definedSelectors basicValues storageSize.
	definedSelectors do:
		[:each |
		(each isKindOf: CompiledMethod)
			ifTrue: [defs := defs + 1]
			ifFalse: [defBytes := defBytes + each storageSize. defs := defs + each size]].

	refs := 0.
	refBytes := referredToSelectors storageSize + referredToSelectors basicValues storageSize.
	referredToSelectors do:
		[:each |
		(each isKindOf: CompiledMethod)
			ifTrue: [refs := refs + 1]
			ifFalse: [refBytes := refBytes + each storageSize. refs := refs + each size]].

	^ (IdentityDictionary new)
		at: #State put: self state;
		at: #SizeInBytes put: self storageSize + defBytes + refBytes;
		at: #DefinitionRecords put: defs;
		at: #DefinitionBytes put: defBytes;
		at: #DefinitionTableLoad put: (definedSelectors size asFloat / definedSelectors basicSize);
		at: #DefinitionTableAPE put: definedSelectors averageProbesPerElement;
		at: #ReferenceRecords put: refs;
		at: #ReferenceBytes put: refBytes;
		at: #ReferenceTableLoad put: (referredToSelectors size asFloat / referredToSelectors basicSize);
		at: #ReferenceTableAPE put: referredToSelectors averageProbesPerElement;
		at: #PendingQueueLength put: todo size;
		yourself.
!

system: aSmalltalkSystem
	"private -- set the SmalltalkSystem which we summarise"

	system := aSmalltalkSystem.
!

undefinedSendsFromMethod: aMethod
	"answer a Set of all the selectors in aMethod which are nowhere defined"

	^ aMethod messages reject: [:each | self isSelectorDefined: each].
!

updateDefinitionsUsing: aDictionary
	"private -- use aDictionary to drive replacing any stale defined methods
	by new versions"

	definedSelectors keysAndValuesDo:
		[:selector :defs |
		(defs isKindOf: CompiledMethod)
			ifTrue: [aDictionary at: defs ifPresent: [:it | definedSelectors at: selector put: it]]
			ifFalse: [defs keysAndValuesDo: [:i :each | aDictionary at: each ifPresent: [:it | defs at: i put: it]]]].!

updateReferencesUsing: aDictionary
	"private -- use aDictionary to drive replacing any stale referred to methods
	by new versions"

	referredToSelectors keysAndValuesDo:
		[:selector :refs |
		(refs isKindOf: CompiledMethod)
			ifTrue: [aDictionary at: refs ifPresent: [:it | referredToSelectors at: selector put: it]]
			ifFalse: [refs keysAndValuesDo: [:i :each | aDictionary at: each ifPresent: [:it | refs at: i put: it]]]].
! !
!BrowserDatabase categoriesFor: #action:!helpers!private! !
!BrowserDatabase categoriesFor: #addClass:!operations!private! !
!BrowserDatabase categoriesFor: #addMethod:!operations!private! !
!BrowserDatabase categoriesFor: #addMethodDefinition:!definitions!private! !
!BrowserDatabase categoriesFor: #addReferencesFromMethod:!private!references! !
!BrowserDatabase categoriesFor: #addSelectorReference:method:!private!references! !
!BrowserDatabase categoriesFor: #allDefinedSelectors!definitions!public! !
!BrowserDatabase categoriesFor: #allDefinitionsDo:!definitions!enumerating!public! !
!BrowserDatabase categoriesFor: #allReferencesDo:!enumerating!public!references! !
!BrowserDatabase categoriesFor: #allReferredToSelectors!public!references! !
!BrowserDatabase categoriesFor: #allSelectors!accessing!definitions!public! !
!BrowserDatabase categoriesFor: #beDisabled!public!states! !
!BrowserDatabase categoriesFor: #bePaused!public!states! !
!BrowserDatabase categoriesFor: #beRunning!public!states! !
!BrowserDatabase categoriesFor: #beStopped!public!states! !
!BrowserDatabase categoriesFor: #check!checking!public! !
!BrowserDatabase categoriesFor: #check:!checking!public! !
!BrowserDatabase categoriesFor: #checkAllMethods!checking!operations!public!states! !
!BrowserDatabase categoriesFor: #checkCurrency!checking!public! !
!BrowserDatabase categoriesFor: #classesDefining:!definitions!public! !
!BrowserDatabase categoriesFor: #countDefinitionsOf:!definitions!enumerating!public! !
!BrowserDatabase categoriesFor: #countDefinitionsOf:satisfying:!definitions!enumerating!public! !
!BrowserDatabase categoriesFor: #countReferencesTo:!enumerating!public!references! !
!BrowserDatabase categoriesFor: #countReferencesTo:satisfying:!enumerating!public!references! !
!BrowserDatabase categoriesFor: #currentVersionOfClass:!helpers!private! !
!BrowserDatabase categoriesFor: #currentVersionOfMethod:!helpers!private! !
!BrowserDatabase categoriesFor: #declineEvents!event handling!private! !
!BrowserDatabase categoriesFor: #definitionsOf:!definitions!public! !
!BrowserDatabase categoriesFor: #definitionsOf:do:!definitions!enumerating!public! !
!BrowserDatabase categoriesFor: #definitionsOf:satisfying:!definitions!public! !
!BrowserDatabase categoriesFor: #definitionsOf:satisfying:do:!definitions!enumerating!public! !
!BrowserDatabase categoriesFor: #discardTables!operations!private! !
!BrowserDatabase categoriesFor: #discardTodoList!operations!private! !
!BrowserDatabase categoriesFor: #doesMethodHaveUndefinedSends:!definitions!public!testing! !
!BrowserDatabase categoriesFor: #executeTodoList!operations!private! !
!BrowserDatabase categoriesFor: #initialize!initializing!private! !
!BrowserDatabase categoriesFor: #isDisabled!public!states!testing! !
!BrowserDatabase categoriesFor: #isEnabled!public!states!testing! !
!BrowserDatabase categoriesFor: #isMethod:equivalentTo:!helpers!private! !
!BrowserDatabase categoriesFor: #isMethodOverriden:!definitions!public!testing! !
!BrowserDatabase categoriesFor: #isMethodReferredTo:!public!references!testing! !
!BrowserDatabase categoriesFor: #isPaused!public!states!testing! !
!BrowserDatabase categoriesFor: #isRunning!public!states!testing! !
!BrowserDatabase categoriesFor: #isSelectorDefined:!definitions!public!testing! !
!BrowserDatabase categoriesFor: #isSelectorReferredTo:!public!references!testing! !
!BrowserDatabase categoriesFor: #isSelectorReferredToExternally:!public!references!testing! !
!BrowserDatabase categoriesFor: #isStopped!public!states!testing! !
!BrowserDatabase categoriesFor: #isTrappingEvents!private!states!testing! !
!BrowserDatabase categoriesFor: #methodsOverriding:!definitions!public! !
!BrowserDatabase categoriesFor: #noisy!accessing!private! !
!BrowserDatabase categoriesFor: #noisy:!accessing!private! !
!BrowserDatabase categoriesFor: #onClassAdded:!event handling!private! !
!BrowserDatabase categoriesFor: #onClassRemoved:!event handling!private! !
!BrowserDatabase categoriesFor: #onClassUpdated:!event handling!private! !
!BrowserDatabase categoriesFor: #onMethodAdded:!event handling!private! !
!BrowserDatabase categoriesFor: #onMethodRemoved:!event handling!private! !
!BrowserDatabase categoriesFor: #onMethodUpdated:!event handling!private! !
!BrowserDatabase categoriesFor: #printOn:!printing!public! !
!BrowserDatabase categoriesFor: #quietlyDo:!helpers!private! !
!BrowserDatabase categoriesFor: #rebuildTables!operations!private! !
!BrowserDatabase categoriesFor: #referencesTo:!public!references! !
!BrowserDatabase categoriesFor: #referencesTo:do:!enumerating!public!references! !
!BrowserDatabase categoriesFor: #referencesTo:satisfying:!public!references! !
!BrowserDatabase categoriesFor: #referencesTo:satisfying:do:!enumerating!public!references! !
!BrowserDatabase categoriesFor: #removeClass:!operations!private! !
!BrowserDatabase categoriesFor: #removeMethod:!operations!private! !
!BrowserDatabase categoriesFor: #removeMethodDefinition:!definitions!private! !
!BrowserDatabase categoriesFor: #removeReferencesFromMethod:!private!references! !
!BrowserDatabase categoriesFor: #removeSelectorReference:method:!private!references! !
!BrowserDatabase categoriesFor: #reset!checking!operations!public! !
!BrowserDatabase categoriesFor: #rootClassesDefining:!definitions!public! !
!BrowserDatabase categoriesFor: #selectorsDefinedInPackageAndPrerequisites:!definitions!public! !
!BrowserDatabase categoriesFor: #selectorsDefinedInPackages:!definitions!public! !
!BrowserDatabase categoriesFor: #selectorsReferredToByPackage:!public!references! !
!BrowserDatabase categoriesFor: #setState:!public!states! !
!BrowserDatabase categoriesFor: #shrink!operations!private! !
!BrowserDatabase categoriesFor: #solicitEvents!event handling!private! !
!BrowserDatabase categoriesFor: #state!accessing!public!states! !
!BrowserDatabase categoriesFor: #state:!accessing!public!states! !
!BrowserDatabase categoriesFor: #stats!accessing!public! !
!BrowserDatabase categoriesFor: #system:!initializing!private! !
!BrowserDatabase categoriesFor: #undefinedSendsFromMethod:!definitions!public! !
!BrowserDatabase categoriesFor: #updateDefinitionsUsing:!definitions!operations!private! !
!BrowserDatabase categoriesFor: #updateReferencesUsing:!operations!private!references! !

!BrowserDatabase class methodsFor!

checkCurrent
	"create a new database and use it to check the current one.
	NB: does not replace the old one.

		self checkCurrent.
	"

	^ self current check.!

checkSystemEvents
	"private -- called during package installation, checks that
	the system objects do indeed publish the events that we
	expect, answers whether that is the case.  Issues a popup
	warning and writes a complaint to the transcript, if not.

		self checkSystemEvents.
	"

	| want got str |

	want := self wantedSystemEvents asSet.
	got := SmalltalkSystem current publishedEvents asSet.
	want := (want - got) asSortedCollection.

	want isEmpty ifTrue: [^ true].

	str := String writeStream
		nextPutAll: 'The following event(s) are required by package ';
		print: self owningPackage name;
		cr.
	want do: [:each | str tab; print: each; cr].
	str
		nextPutAll: 'but are not published by SmalltalkSystem.';
		cr.

	Transcript
		nextPutAll: 'Warning.  ';
		nextPutAll: str contents;
		cr.

	str cr; nextPutAll: '(This message has also been written to the Transcript)'.

	MessageBox
		warning: str contents
		caption: self owningPackage name.

	^ false.!

current
	"answer the default instance (the one which watches SmalltalkSystem current).
	Note that the default instance is initially disabled, you need to activate it explicitly
	before it will start collecting data"

	current isNil ifTrue: [current := self for: SmalltalkSystem current].

	^ current.!

for: aSmalltalkSystem
	"answer a new instance which is initially disabled, but which knows how to observe, and
	will holds data about, aSmalltalkSystem"

	^ (self new)
		system: aSmalltalkSystem;
		yourself.!

for: aSmalltalkSystem state: aSymbol
	"answer a new instance holds data about aSmalltalkSystem, and which is initially
	in the state () indicated by aSymbol"

	^ (self for: aSmalltalkSystem)
		state: aSymbol;
		yourself.!

publishedEventsOfInstances
	"answer the published events triggered by our instances"

	^ (super publishedEventsOfInstances)
		add: #stateChanged;
		add: #statisticsChanged;
		yourself.
!

resetCurrent
	"helper method which forcibly resets the 'current' instance.
	Time millisecondsToRun: [
		self resetCurrent.
	]. 
	Profiler profile: [
		self resetCurrent.
	]. 
	"

	self current reset.!

resetCurrentInBackground
	"helper method which runs a background process to reset the 'current' instance.
		self resetCurrentInBackground.
	NB: I doubt if this is threadsafe.  It should be OK to use if all you are doing while it
	runs is browsing, but if you redefine stuff during this rescan then you are on you own...
	"

	[| time |
	time := Time millisecondsToRun: [current := self for: SmalltalkSystem current].
	Transcript
		display: self name;
		display: ' rebuilt in ';
		display: time / 1000.0;
		display: ' seconds';
		cr] forkAt: Processor userBackgroundPriority.
!

uninitialize
	"undo the initialization of the receiver.
		self uninitialize.
	"

	current := nil.!

wantedSystemEvents
	"private -- answer a Collection of the Symbol events that instances
	rely on getting from the system"


	"NB: this should be kept in synch with (instance side) #solicitEvents"
	^ #(
		#classAdded: #classUpdated: #classRemoved:
		#methodAdded: #methodUpdated: #methodRemoved:
	).! !
!BrowserDatabase class categoriesFor: #checkCurrent!helpers!public!testing! !
!BrowserDatabase class categoriesFor: #checkSystemEvents!helpers!private! !
!BrowserDatabase class categoriesFor: #current!accessing!public! !
!BrowserDatabase class categoriesFor: #for:!instance creation!public! !
!BrowserDatabase class categoriesFor: #for:state:!instance creation!public! !
!BrowserDatabase class categoriesFor: #publishedEventsOfInstances!development!events!public! !
!BrowserDatabase class categoriesFor: #resetCurrent!helpers!public! !
!BrowserDatabase class categoriesFor: #resetCurrentInBackground!helpers!public! !
!BrowserDatabase class categoriesFor: #uninitialize!initializing!public! !
!BrowserDatabase class categoriesFor: #wantedSystemEvents!constants!private! !

"Binary Globals"!

"Resources"!

