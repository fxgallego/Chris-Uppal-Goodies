| package |
package := Package name: 'CU Java Wrapper Wizard'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

The Wrapper Wizard is a tool for creating JNIPort Wrapper Classes corresponding to Java classes.  It is invoked from the Status Monitor''s ''Classes'' page.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.05'.


package classNames
	add: #JavasWrapperSettingsPresenter;
	add: #JavaWrapperWizard;
	yourself.

package methodNames
	add: #JavaClassWrapperInstaller -> #canGenerateWrapper;
	add: #JavaInstanceClassWrapperGenerator -> #canGenerateWrapper;
	add: #JavaStaticWrapperGenerator -> #canGenerateWrapper;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #JavasWrapperSettingsPresenter -> 'Default view';
	add: #JavaWrapperWizard -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU Java Status Monitor';
	add: 'CU Java Wrapper Generation';
	add: 'CU Package-relative File Locator';
	add: 'CU Sortblocks';
	add: 'CU Tools Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	yourself).

package setManualPrerequisites: #(
	'CU Java Status Monitor').

package!

"Class Definitions"!

Presenter subclass: #JavasWrapperSettingsPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CUWizardShell subclass: #JavaWrapperWizard
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaClassWrapperInstaller methodsFor!

canGenerateWrapper
	"private -- answer true iff it looks as if we've got enough correct data to go ahead"

	^ self classGenerator canGenerateWrapper.! !
!JavaClassWrapperInstaller categoriesFor: #canGenerateWrapper!private!testing! !

!JavaInstanceClassWrapperGenerator methodsFor!

canGenerateWrapper
	"private -- answer true iff it looks as if we've got enough correct data to go ahead"

	#CUtodo.  "this is enough for the wrapper wizard, but too weak in general"
	#CUtodo.  "should reflect back to the target class rather than coding it here"

	(targetClass inheritsFrom: JavaInstance) ifFalse: [^ false].

	((targetClass class includesSelector: #javaClassName)
		and: [targetClass javaClassName asJNIClassName ~= classStatic name asJNIClassName])
			ifTrue: [^ false].

	^ true.! !
!JavaInstanceClassWrapperGenerator categoriesFor: #canGenerateWrapper!public!testing! !

!JavaStaticWrapperGenerator methodsFor!

canGenerateWrapper
	"private -- answer true iff it looks as if we've got enough correct data to go ahead"

	#CUtodo.  "this is enough for the wrapper wizard, but too weak in general"
	#CUtodo.  "should reflect back to the target class rather than coding it here"

	(targetClass inheritsFrom: JavaStatic) ifFalse: [^ false].

	((targetClass class includesSelector: #javaClassName)
		and: [targetClass javaClassName asJNIClassName ~= classStatic name asJNIClassName])
			ifTrue: [^ false].

	^ true.! !
!JavaStaticWrapperGenerator categoriesFor: #canGenerateWrapper!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

JavasWrapperSettingsPresenter guid: (GUID fromString: '{1C454B15-7511-4C11-B3A1-22A21ECC48E2}')!
JavasWrapperSettingsPresenter comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavasWrapperSettingsPresenter categoriesForClass!Unclassified! !
!JavasWrapperSettingsPresenter methodsFor!

adjustViewFor: aJavaClassWrapperInstaller
	"private -- set our model from the settings in aJavaClassWrapperInstaller.
	We do it this way since there are a few other changes we make dependent
	on the installer that are not defined in the settings object -- basically turning
	off irrelevant options"

	| source classSide |

	source := aJavaClassWrapperInstaller source.
	classSide := aJavaClassWrapperInstaller isForClassSide.

	self enableAbstractMethods: classSide not.
	self enableBridgeMethods: (classSide not and: [aJavaClassWrapperInstaller isForJava5]).

	self enableConstructors: (classSide and: [source isInterface not]).
	self enableMethods: (classSide and: [source isInterface]) not.
	self enableFields: (classSide or: [source isInterface not]).!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	self add: BooleanPresenter new name: 'IncludeSuperclasses'.
	self add: BooleanPresenter new name: 'IncludeAbstractMethods'.
	self add: BooleanPresenter new name: 'IncludeBridgeMethods'.

	self add: BooleanPresenter new name: 'IncludePublicFields'.
	self add: BooleanPresenter new name: 'IncludeDefaultAccessFields'.
	self add: BooleanPresenter new name: 'IncludeProtectedFields'.
	self add: BooleanPresenter new name: 'IncludePrivateFields'.
	self add: BooleanPresenter new name: 'IncludePublicMethods'.
	self add: BooleanPresenter new name: 'IncludeDefaultAccessMethods'.
	self add: BooleanPresenter new name: 'IncludeProtectedMethods'.
	self add: BooleanPresenter new name: 'IncludePrivateMethods'.
	self add: BooleanPresenter new name: 'IncludePublicConstructors'.
	self add: BooleanPresenter new name: 'IncludeDefaultAccessConstructors'.
	self add: BooleanPresenter new name: 'IncludeProtectedConstructors'.
	self add: BooleanPresenter new name: 'IncludePrivateConstructors'.
!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self includeSuperclassesPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeAbstractMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeBridgeMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.

	self includePublicFieldsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeDefaultAccessFieldsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeProtectedFieldsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includePrivateFieldsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includePublicMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeDefaultAccessMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeProtectedMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includePrivateMethodsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includePublicConstructorsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeDefaultAccessConstructorsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includeProtectedConstructorsPresenter when: #valueChanged send: #onSettingsChanged to: self.
	self includePrivateConstructorsPresenter when: #valueChanged send: #onSettingsChanged to: self.
!

enableAbstractMethods: aBoolean
	"private -- enable/disable our include abstract methods choice box"

	self includeAbstractMethodsPresenter view isEnabled: aBoolean.
!

enableBridgeMethods: aBoolean
	"private -- enable/disable our include abstract methods choice box"

	self includeBridgeMethodsPresenter view isEnabled: aBoolean.
!

enableConstructors: aBoolean
	"private -- enable/disable our include XXX constructor choice boxes"

	self includePrivateConstructorsPresenter view isEnabled: aBoolean.
	self includeDefaultAccessConstructorsPresenter view isEnabled: aBoolean.
	self includeProtectedConstructorsPresenter view isEnabled: aBoolean.
	self includePublicConstructorsPresenter view isEnabled: aBoolean.

!

enableFields: aBoolean
	"private -- enable/disable our include XXX field choice boxes"

	self includePrivateFieldsPresenter view isEnabled: aBoolean.
	self includeDefaultAccessFieldsPresenter view isEnabled: aBoolean.
	self includeProtectedFieldsPresenter view isEnabled: aBoolean.
	self includePublicFieldsPresenter view isEnabled: aBoolean.

!

enableMethods: aBoolean
	"private -- enable/disable our include XXX method choice boxes"

	self includePrivateMethodsPresenter view isEnabled: aBoolean.
	self includeDefaultAccessMethodsPresenter view isEnabled: aBoolean.
	self includeProtectedMethodsPresenter view isEnabled: aBoolean.
	self includePublicMethodsPresenter view isEnabled: aBoolean.

!

includeAbstractMethodsPresenter
	"private -- answer the presenter named 'IncludeAbstractMethods'"

	^ self presenterNamed: 'IncludeAbstractMethods'.
!

includeBridgeMethodsPresenter
	"private -- answer the presenter named 'IncludeBridgeMethods'"

	^ self presenterNamed: 'IncludeBridgeMethods'.
!

includeDefaultAccessConstructorsPresenter
	"private -- answer the presenter named 'IncludeDefaultAccessConstructors'"

	^ self presenterNamed: 'IncludeDefaultAccessConstructors'.
!

includeDefaultAccessFieldsPresenter
	"private -- answer the presenter named 'IncludeDefaultAccessFields'"

	^ self presenterNamed: 'IncludeDefaultAccessFields'.
!

includeDefaultAccessMethodsPresenter
	"private -- answer the presenter named 'IncludeDefaultAccessMethods'"

	^ self presenterNamed: 'IncludeDefaultAccessMethods'.
!

includePrivateConstructorsPresenter
	"private -- answer the presenter named 'IncludePrivateConstructors'"

	^ self presenterNamed: 'IncludePrivateConstructors'.
!

includePrivateFieldsPresenter
	"private -- answer the presenter named 'IncludePrivateFields'"

	^ self presenterNamed: 'IncludePrivateFields'.
!

includePrivateMethodsPresenter
	"private -- answer the presenter named 'IncludePrivateMethods'"

	^ self presenterNamed: 'IncludePrivateMethods'.
!

includeProtectedConstructorsPresenter
	"private -- answer the presenter named 'IncludeProtectedConstructors'"

	^ self presenterNamed: 'IncludeProtectedConstructors'.
!

includeProtectedFieldsPresenter
	"private -- answer the presenter named 'IncludeProtectedFields'"

	^ self presenterNamed: 'IncludeProtectedFields'.
!

includeProtectedMethodsPresenter
	"private -- answer the presenter named 'IncludeProtectedMethods'"

	^ self presenterNamed: 'IncludeProtectedMethods'.
!

includePublicConstructorsPresenter
	"private -- answer the presenter named 'IncludePublicConstructors'"

	^ self presenterNamed: 'IncludePublicConstructors'.
!

includePublicFieldsPresenter
	"private -- answer the presenter named 'IncludePublicFields'"

	^ self presenterNamed: 'IncludePublicFields'.
!

includePublicMethodsPresenter
	"private -- answer the presenter named 'IncludePublicMethods'"

	^ self presenterNamed: 'IncludePublicMethods'.
!

includeSuperclassesPresenter
	"private -- answer the presenter named 'IncludeSuperclasses'"

	^ self presenterNamed: 'IncludeSuperclasses'.
!

model: aJavaWrapperGeneratorSettings
	"private -- set our model"

	super model: aJavaWrapperGeneratorSettings.

	self includeSuperclassesPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeAllSuperclasses).
	self includeAbstractMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeAbstractMethods).
	self includeBridgeMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeBridgeMethods).

	self includePublicFieldsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePublicFields).
	self includeDefaultAccessFieldsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeDefaultAccessFields).
	self includeProtectedFieldsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeProtectedFields).
	self includePrivateFieldsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePrivateFields).
	self includePublicMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePublicMethods).
	self includeDefaultAccessMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeDefaultAccessMethods).
	self includeProtectedMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeProtectedMethods).
	self includePrivateMethodsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePrivateMethods).
	self includePublicConstructorsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePublicConstructors).
	self includeDefaultAccessConstructorsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeDefaultAccessConstructors).
	self includeProtectedConstructorsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includeProtectedConstructors).
	self includePrivateConstructorsPresenter model: (aJavaWrapperGeneratorSettings aspectValue: #includePrivateConstructors).
!

onSettingsChanged
	"private -- one of the settings we present has changed, inform any observers"

	self trigger: #settingsChanged.! !
!JavasWrapperSettingsPresenter categoriesFor: #adjustViewFor:!initializing!models!private!updating! !
!JavasWrapperSettingsPresenter categoriesFor: #createComponents!initializing!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #enableAbstractMethods:!helpers!private! !
!JavasWrapperSettingsPresenter categoriesFor: #enableBridgeMethods:!helpers!private! !
!JavasWrapperSettingsPresenter categoriesFor: #enableConstructors:!helpers!private! !
!JavasWrapperSettingsPresenter categoriesFor: #enableFields:!helpers!private! !
!JavasWrapperSettingsPresenter categoriesFor: #enableMethods:!helpers!private! !
!JavasWrapperSettingsPresenter categoriesFor: #includeAbstractMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeBridgeMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeDefaultAccessConstructorsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeDefaultAccessFieldsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeDefaultAccessMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePrivateConstructorsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePrivateFieldsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePrivateMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeProtectedConstructorsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeProtectedFieldsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeProtectedMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePublicConstructorsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePublicFieldsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includePublicMethodsPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #includeSuperclassesPresenter!private!subpresenters! !
!JavasWrapperSettingsPresenter categoriesFor: #model:!initializing!models!private! !
!JavasWrapperSettingsPresenter categoriesFor: #onSettingsChanged!event handling!private! !

!JavasWrapperSettingsPresenter class methodsFor!

defaultModel
	"answer a model object to use by default"

	^ JavaWrapperGeneratorSettings new.!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by JVM instances"	

	^ (super publishedEventsOfInstances)
		add: #settingsChanged;
		yourself.
! !
!JavasWrapperSettingsPresenter class categoriesFor: #defaultModel!models!public! !
!JavasWrapperSettingsPresenter class categoriesFor: #publishedEventsOfInstances!constants!development!public! !

JavaWrapperWizard guid: (GUID fromString: '{6F65076B-F08A-4FB4-B0F0-18E746E48B5B}')!
JavaWrapperWizard comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaWrapperWizard categoriesForClass!Unclassified! !
!JavaWrapperWizard methodsFor!

allSelectorsPresenter
	"private -- answer the presenter named 'AllSelectors'"

	^ self presenterNamed: 'AllSelectors'.
!

canFinish
	"private -- answer true iff we may perform the #finish command ?"

	^ super canFinish and: [self model canGenerateWrapper].
!

canNextPage
	"private -- answer true iff we may perform the #nextPage command ?"

	^ super canNextPage and: [self model canGenerateWrapper].
!

constructorsPresenter
	"private -- answer the presenter named 'Constructors'"

	^ self presenterNamed: 'Constructors'.
!

createComponents
	"private -- create subpresenters for our various subviews"

	super createComponents.

	"page 1"
	self add: TextPresenter new name: 'JavaClass'.
	self add: ClassHierarchySelector new name: 'SmalltalkClass'.

	"page 2"
	self add: JavasWrapperSettingsPresenter new name: 'Settings'.
	self add: ListPresenter new name: 'Constructors'.
	self add: ListPresenter new name: 'Wrappers'.
	self add: ListPresenter new name: 'Getters'.
	self add: ListPresenter new name: 'Setters'.
	self add: ListPresenter new name: 'Others'.

	"page 3"
	self add: TextPresenter new name: 'SmalltalkClassname'.
	self add: ListPresenter new name: 'AllSelectors'.
	self add: BooleanPresenter new name: 'RegisterNewClass'.!

createSchematicWiring
	"private - arrange triggering between our components"

	super createSchematicWiring.

	self settingsPresenter when: #settingsChanged send: #updateLists to: self.

	self smalltalkClassPresenter when: #selectionChanged send: #onClassSelected to: self.
!

enableAccessors: aBoolean
	"private -- enable/disable our accessors lists"

	self gettersPresenter view isEnabled: aBoolean.
	self settersPresenter view isEnabled: aBoolean.
!

enableConstructors: aBoolean
	"private -- enable/disable our constructor list"

	self constructorsPresenter view isEnabled: aBoolean.
!

enableWrappers: aBoolean
	"private -- enable/disable our wrapper list"

	self wrappersPresenter view isEnabled: aBoolean.
!

finish
	"command -- finish the wizard's operation"

	| cap msg installer |

	self canFinish ifFalse: [^ self].

	installer := self model.

	installer install.

	cap := 'Wrapped %s' sprintfWith: installer source name.

	msg := 'Generated %d methods in %s'
			sprintfWith: installer installed size
			with: installer destination name.
	installer problems isEmpty
		ifFalse: [msg := msg , ('; encountered %d problems (see Transcript)' sprintfWith: installer problems size)].

	self registerNewClassPresenter value ifFalse:
		[MessageBox notify: msg caption: cap.
		^ self view exit].

	installer problems isEmpty ifFalse:
		[msg := msg , '.
Register class anyway ?'.
		(MessageBox confirm: msg caption: cap) ifTrue: [installer registerDestination].
		^ self view exit].

	installer registerDestination.
	msg := msg , ', and registered it.'.
	MessageBox notify: msg caption: cap.

	self view exit.!

gettersPresenter
	"private -- answer the presenter named 'Getters'"

	^ self presenterNamed: 'Getters'.
!

help
	"command -- display the help for the Wrapper Wizard"

	| locator file |

	locator := PackageRelativeFileLocator package: self class owningPackage.
	file := locator localFileSpecFor: 'Docs\JNIPort\wrapper-wizard.html'.
	[ShellLibrary default shellOpen: file]
		on: Error
		do: [:err | MessageBox
			notify: ('Sorry.  No help for the ' , self class toolName , ' is available')
			caption: 'JVM Status Help'].
!

informationIconIndex
	"private -- answer icon index to use to tag columns with an alert"

	^ Icon information imageIndex.!

javaClassPresenter
	"private -- answer the presenter named 'JavaClass'"

	^ self presenterNamed: 'JavaClass'.
!

maxPages
	"answer how many pages we have"

	^ 3.!

methodImage: aJavaMethodGenerator
	"private -- answer the icon index to use for the given generator"

	(self model isDangerousOverwrite: aJavaMethodGenerator)
		ifTrue: [^ self warningIconIndex].

	(self model isAcceptableOverwrite: aJavaMethodGenerator)
		ifTrue: [^ self informationIconIndex].

	^ nil.!

methodImageBlock: aClass
	"private -- answer a 1Block which can be used to generate tag images for the
	currently selected method"

	^ aClass isNil
		ifTrue: [nil]
		ifFalse: [[:generator | self methodImage: generator]].
!

methodInfoTip: aJavaMethodGenerator prefix: aString
	"private -- answer the infotip text to use for the global named by aSymbol"

	"I have no idea what we are supposed to do with the prefix"

	| installer |

	installer := self model.

	(installer isDangerousOverwrite: aJavaMethodGenerator)
		ifTrue: [^ '%s is already defined and is not in the %s category'
				sprintfWith: aJavaMethodGenerator methodName
				with: installer autoCategoryName].

	(installer isAcceptableOverwrite: aJavaMethodGenerator)
		ifTrue: [^ '%s is already defined, but is in the %s category'
				sprintfWith: aJavaMethodGenerator methodName
				with: installer autoCategoryName].

	^ nil.
!

methodInfoTipBlock: aClass
	"private -- answer a 2Block which can be used to generate infotip text for the
	currently selected record's classes list"

	^ aClass isNil
		ifTrue: [nil]
		ifFalse: [[:generator :pfx | self methodInfoTip: generator prefix: pfx]].
!

methodStatusColumn
	"private -- answer the Column where we display method statii"

	^ self allSelectorsPresenter view allColumns first.!

model: aJavaClassWrapperInstaller
	"private -- set our model"

	super model: aJavaClassWrapperInstaller.

	self settingsPresenter model: aJavaClassWrapperInstaller settings.

	self javaClassPresenter value: aJavaClassWrapperInstaller source name.
	self smalltalkClassPresenter actualClass: aJavaClassWrapperInstaller destination ifAbsent: [].
	self smalltalkClassnamePresenter value: aJavaClassWrapperInstaller destination name.

	self
		updateView;
		updateLists.
!

newClass
	"command -- create a new class.  This is actually forwarded to our embedded class tree presenter"

	"unfortunately there is no API for controlling the suggested name"
	self smalltalkClassPresenter newClass.!

onClassSelected
	"private -- our target class has been changed"

	| target installer |

	target := self smalltalkClassPresenter actualClass.
	self smalltalkClassnamePresenter value: ('Methods to add to ' , target name).
	self methodStatusColumn
		getImageBlock: (self methodImageBlock: target);
		getInfoTipBlock: (self methodInfoTipBlock: target).

	installer := self model.
	installer destination: target.
	self registerNewClassPresenter value: (installer canRegisterDestination and: [installer source isGhost not]).
	self registerNewClassPresenter view isEnabled: installer canRegisterDestination.

	self updateLists.
!

onViewOpened
	"private -- called when we are more-or-less ready to roll"

	| source target |

	super onViewOpened.

	"turn off default context menu in the embedded class browser so that it'll
	inherit the more limited menu that we provide"
	((self smalltalkClassPresenter view)
		viewNamed: 'classes')
			contextMenu: nil.

	"bloody MVP and its deaf objects again..."
	self model: self model.!

othersPresenter
	"private -- answer the presenter named 'Others'"

	^ self presenterNamed: 'Others'.
!

registerNewClassPresenter
	"private -- answer the presenter named 'RegisterNewClass'"

	^ self presenterNamed: 'RegisterNewClass'.
!

settersPresenter
	"private -- answer the presenter named 'Setters'"

	^ self presenterNamed: 'Setters'.
!

settingsPresenter
	"private -- answer the presenter named 'Settings'"

	^ self presenterNamed: 'Settings'.
!

smalltalkClassnamePresenter
	"private -- answer the presenter named 'SmalltalkClassname'"

	^ self presenterNamed: 'SmalltalkClassname'.
!

smalltalkClassPresenter
	"private -- answer the presenter named 'SmalltalkClass'"

	^ self presenterNamed: 'SmalltalkClass'.
!

updateLists
	"private -- our displayed lists are out-of-date, refresh them"

	| list |

	list := self model canGenerateWrapper
			ifTrue: [self model methodGenerators]
			ifFalse: [#()].

	"it'd be nice -- but it's too much work -- to hide the cards with empty lists"
	self constructorsPresenter list: (list select: [:each | each isConstructor]).
	self wrappersPresenter list: (list select: [:each | each isWrapper]).
	self gettersPresenter list: (list select: [:each | each isGetter]).
	self settersPresenter list: (list select: [:each | each isSetter]).
	self othersPresenter list: (list select: [:each | each isHousekeeping]).

	self allSelectorsPresenter list: (list asSortedCollection: (SortStringsAscending by: #selector)) .
!

updateView
	"private -- update our View to reflect the specific model we are going to use"

	| source classSide |

	source := self model source.
	classSide := self model isForClassSide.

	self captionExtension: source name.

	self enableConstructors: (classSide and: [source isInterface not]).
	self enableWrappers: (classSide and: [source isInterface]) not.
	self enableAccessors: (classSide or: [source isInterface not]).

	self settingsPresenter adjustViewFor: self model.!

warningIconIndex
	"private -- answer icon index to use to tag columns with a warning"

	^ Icon warning imageIndex.!

wrappersPresenter
	"private -- answer the presenter named 'Wrappers'"

	^ self presenterNamed: 'Wrappers'.
! !
!JavaWrapperWizard categoriesFor: #allSelectorsPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #canFinish!commands!private! !
!JavaWrapperWizard categoriesFor: #canNextPage!commands!private! !
!JavaWrapperWizard categoriesFor: #constructorsPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #createComponents!initializing!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #createSchematicWiring!event handling!initializing!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #enableAccessors:!helpers!private! !
!JavaWrapperWizard categoriesFor: #enableConstructors:!helpers!private! !
!JavaWrapperWizard categoriesFor: #enableWrappers:!helpers!private! !
!JavaWrapperWizard categoriesFor: #finish!commands!private! !
!JavaWrapperWizard categoriesFor: #gettersPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #help!commands!public! !
!JavaWrapperWizard categoriesFor: #informationIconIndex!helpers!private! !
!JavaWrapperWizard categoriesFor: #javaClassPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #maxPages!constants!pages!private! !
!JavaWrapperWizard categoriesFor: #methodImage:!helpers!private! !
!JavaWrapperWizard categoriesFor: #methodImageBlock:!blocks!private! !
!JavaWrapperWizard categoriesFor: #methodInfoTip:prefix:!helpers!private! !
!JavaWrapperWizard categoriesFor: #methodInfoTipBlock:!blocks!private! !
!JavaWrapperWizard categoriesFor: #methodStatusColumn!helpers!private! !
!JavaWrapperWizard categoriesFor: #model:!initializing!models!private! !
!JavaWrapperWizard categoriesFor: #newClass!commands!public! !
!JavaWrapperWizard categoriesFor: #onClassSelected!event handling!private!updating! !
!JavaWrapperWizard categoriesFor: #onViewOpened!event handling!initializing!private! !
!JavaWrapperWizard categoriesFor: #othersPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #registerNewClassPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #settersPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #settingsPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #smalltalkClassnamePresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #smalltalkClassPresenter!private!subpresenters! !
!JavaWrapperWizard categoriesFor: #updateLists!event handling!private!updating! !
!JavaWrapperWizard categoriesFor: #updateView!initializing!models!private!updating! !
!JavaWrapperWizard categoriesFor: #warningIconIndex!helpers!private! !
!JavaWrapperWizard categoriesFor: #wrappersPresenter!private!subpresenters! !

!JavaWrapperWizard class methodsFor!

icon
	"answer an Icon representing the receiver"

	^ JVM icon.!

toolName
	"answer the Sting name to use for the name of the tool we define"

	^ 'Java Wrapper Wizard'.! !
!JavaWrapperWizard class categoriesFor: #icon!constants!public! !
!JavaWrapperWizard class categoriesFor: #toolName!constants!displaying!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: JavasWrapperSettingsPresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAEEnAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAADQAAAENvbnRhaW5lclZpZXdiAAAA
DwAAAAAAAAAAAAAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29s
b3IAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAoAEAAAYBDQBGcmFtaW5nTGF5b3V0AAAAAOoA
AAAAAAAA8AAAAGIAAAAKAAAAmgEAAAAAAACaAAAAAAAAAMABAABSAAAACAAAAENoZWNrQm94YgAA
ABAAAAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAAADIAFEAQAAAHACAABGBAsAAgAAAFZhbHVlSG9s
ZGVyAAAAAAAAAAAAAAAADgIRAFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABE
b2xwaGluUgAAAAwAAABTZWFyY2hQb2xpY3m6AAAAAAAAAFIAAAAFAAAAbmV2ZXIgAAAAAAAAAAAA
AAAHAAAAAAAAAAAAAAAAAAAAcAIAAAAAAACCAAAACAAAAOUF//8AAAAABgINAE51bGxDb252ZXJ0
ZXIAAAAAAAAAAAAAAAAAAAAABgEPAE1lc3NhZ2VTZXF1ZW5jZQAAAADKAAAAAAAAANAAAABiAAAA
AgAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAAAAAAAFIAAAAQAAAAY3JlYXRlQXQ6ZXh0ZW50OmIA
AAACAAAABgIFAFBvaW50AAAAAAEAAABRAAAAIgQAAAAAAAB3AgAAKQAAAHACAADSAwAAAAAAALoA
AAAAAAAAUgAAAAUAAAB0ZXh0OmIAAAABAAAAUgAAABoAAABJbmNsdWRlICdicmlkZ2UnIG1ldGhv
ZHMgP3ACAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////
/////////////wAAAAAoAAAAOwEAADwAAADKAAAAAAAAANAAAABiAAAAAAAAACIEAAAAAAAAwQAA
AMEAAAAAAAAAEwAAAEYIEgABAAAARnJhbWluZ0NvbnN0cmFpbnRzAAAAALoAAAAAAAAAUgAAAA8A
AABmaXhlZFBhcmVudExlZnQBAAAAugAAAAAAAABSAAAAEAAAAGZpeGVkUGFyZW50UmlnaHQBAAAA
ugAAAAAAAABSAAAAEwAAAGZpeGVkUHJldmlvdXNCb3R0b20BAAAAugAAAAAAAABSAAAADAAAAGZp
eGVkVmlld1RvcCkAAACaAQAAAAAAAIACAABiAAAAEAAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAA
AAMgAUQBAAAAoAUAANICAAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAA
AAAAAKAFAAAAAAAAggAAAAgAAADlBf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADK
AAAAAAAAANAAAABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAABAAAAKQAAACIE
AAAAAAAAdwIAACkAAACgBQAA0gMAAAAAAABgBAAAYgAAAAEAAABSAAAAGgAAAEluY2x1ZGUgYWJz
dHJhY3QgbWV0aG9kcyA/oAUAAKIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////
////////AAAAABQAAAA7AQAAKAAAAMoAAAAAAAAA0AAAAOAEAADwBAAAAAAAABMAAAACBQAAAAAA
ACAFAAABAAAAQAUAAAEAAABgBQAAAQAAAIAFAAApAAAAmgEAAAAAAACwAQAAYgAAAA8AAAAAAAAA
oAEAAGIAAAACAAAAggAAAAQAAAAAAABEAQACAPAGAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAA
AAAA8AYAAAYECgBHcmlkTGF5b3V0AAAAAAEAAAADAAAAAQAAAAEAAADqAAAAAAAAAAABAADgBAAA
AAAAAJIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAADSAwAAAAAAAPADAABiAAAAAgAAACIEAAAA
AAAAAQAAAIMAAAAiBAAAAAAAAPEAAADJAAAA8AYAAKIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEA
AAD/////////////////////AAAAAEEAAAB4AAAApQAAAMoAAAAAAAAA0AAAAGIAAAAFAAAAmgEA
AAAAAACaAAAAAAAAAMABAABSAAAACgAAAFN0YXRpY1RleHRiAAAAEAAAAAAAAADwBgAAYgAAAAIA
AACCAAAABAAAAAABAEQBAAAAEAgAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAAAQCAAAAAAA
AIIAAAAIAAAAAQb//wAAAAByAwAAAAAAAAAAAAAAAAAAAAAAAJIDAAAAAAAAygAAAAAAAADQAAAA
YgAAAAIAAADSAwAAAAAAAPADAABiAAAAAgAAACIEAAAAAAAAAQAAAAEAAAAiBAAAAAAAAPEAAAAp
AAAAEAgAANIDAAAAAAAAYAQAAGIAAAABAAAAUgAAABEAAABJbmNsdWRlIG1lbWJlcnMgPxAIAACi
BAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAAAAAAAAAAeAAAABQA
AADKAAAAAAAAANAAAADgBAAA8AQAAAAAAAATAAAAmgEAAAAAAAAgCAAAYgAAABAAAAAAAAAA8AYA
AGIAAAACAAAAggAAAAQAAAAAAQBEAQAAAGAJAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAA
YAkAAAAAAACCAAAACAAAAAEG//8AAAAAcgMAAAAAAAAAAAAAAAAAAAAAAACSAwAAAAAAAMoAAAAA
AAAA0AAAAGIAAAACAAAA0gMAAAAAAADwAwAAYgAAAAIAAAAiBAAAAAAAAAEAAAApAAAAIgQAAAAA
AADxAAAAKQAAAGAJAADSAwAAAAAAAGAEAABiAAAAAQAAAFIAAAAGAAAAUHVibGljYAkAAKIEAAAA
AAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////AAAAABQAAAB4AAAAKAAAAMoA
AAAAAAAA0AAAAOAEAADwBAAAAAAAABMAAACaAQAAAAAAACAIAABiAAAAEAAAAAAAAADwBgAAYgAA
AAIAAACCAAAABAAAAAABAEQBAAAAkAoAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACQCgAA
AAAAAIIAAAAIAAAAAQb//wAAAAByAwAAAAAAAAAAAAAAAAAAAAAAAJIDAAAAAAAAygAAAAAAAADQ
AAAAYgAAAAIAAADSAwAAAAAAAPADAABiAAAAAgAAACIEAAAAAAAAAQAAAFEAAAAiBAAAAAAAAPEA
AAApAAAAkAoAANIDAAAAAAAAYAQAAGIAAAABAAAAUgAAAAkAAABQcm90ZWN0ZWSQCgAAogQAAAAA
AAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAKAAAAHgAAAA8AAAAygAA
AAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAJoBAAAAAAAAIAgAAGIAAAAQAAAAAAAAAPAGAABiAAAA
AgAAAIIAAAAEAAAAAAEARAEAAADACwAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAMALAAAA
AAAAggAAAAgAAAABBv//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAA
AABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAABAAAAeQAAACIEAAAAAAAA8QAA
ACkAAADACwAA0gMAAAAAAABgBAAAYgAAAAEAAABSAAAAEAAAACdEZWZhdWx0IGFjY2VzcyfACwAA
ogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAPAAAAHgAAABQ
AAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAJoBAAAAAAAAIAgAAGIAAAAQAAAAAAAAAPAG
AABiAAAAAgAAAIIAAAAEAAAAAAEARAEAAADwDAAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
APAMAAAAAAAAggAAAAgAAAABBv//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAA
AAAAANAAAABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAABAAAAoQAAACIEAAAA
AAAA8QAAACkAAADwDAAA0gMAAAAAAABgBAAAYgAAAAEAAABSAAAABwAAAFByaXZhdGXwDAAAogQA
AAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAUAAAAHgAAABkAAAA
ygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAPAEAAAAAAAAEwAAAAIFAAAAAAAAIAUAAAEAAAC6
AAAAAAAAAFIAAAANAAAAZml4ZWRWaWV3TGVmdPEAAABgBQAACwAAAIAFAADJAAAAmgEAAAAAAACA
AgAAYgAAABAAAAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAAADIAFEAQAAAFAOAADSAgAAAAAAAAAA
AAAAAAAAAAMAACAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAABQDgAAAAAAAIIAAAAIAAAA5QX/
/wAAAAByAwAAAAAAAAAAAAAAAAAAAAAAAJIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAIAAADSAwAA
AAAAAPADAABiAAAAAgAAACIEAAAAAAAAAQAAAAEAAAAiBAAAAAAAAHcCAAApAAAAUA4AANIDAAAA
AAAAYAQAAGIAAAABAAAAUgAAAC4AAABJbmNsdWRlIGZpZWxkcyBhbmQgbWV0aG9kcyBmcm9tIHN1
cGVyY2xhc3NlcyA/UA4AAKIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////
////AAAAAAAAAAA7AQAAFAAAAMoAAAAAAAAA0AAAAOAEAADwBAAAAAAAABMAAAACBQAAAAAAACAF
AAABAAAAQAUAAAEAAAC6AAAAAAAAAFIAAAAOAAAAZml4ZWRQYXJlbnRUb3ABAAAAgAUAACkAAACa
AQAAAAAAALABAABiAAAADwAAAAAAAACgAQAAYgAAAAIAAACCAAAABAAAAAAAAEQBAAIAwA8AAAAA
AAASAgAAAAAAAB8AAAAAAAAABwAAAAAAAAAAAAAAAAAAAMAPAAAyBwAAAAAAAAEAAAAHAAAAFQAA
AAEAAADqAAAAAAAAAAABAABiAAAAGAAAAJoBAAAAAAAAgAIAAGIAAAAQAAAAAAAAAMAPAABiAAAA
AgAAAIIAAAAEAAAAAyABRAEAAABAEAAA0gIAAAAAAAAAAAAAAAAAAAADAAAgAAAAAAAAAAAAAAAH
AAAAAAAAAAAAAAAAAAAAQBAAAAAAAACCAAAACAAAAOUF//8AAAAAcgMAAAAAAAAAAAAAAAAAAAAA
AACSAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAABAAAA0gMAAAAAAADwAwAAYgAAAAIAAAAiBAAAAAAA
ABkBAABRAAAAIgQAAAAAAAB5AAAAKQAAAEAQAACiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA
/////////////////////4wAAAAoAAAAyAAAADwAAADKAAAAAAAAANAAAADgBAAA8AQAAAAAAAAT
AAAAUgAAABYAAABJbmNsdWRlUHJvdGVjdGVkRmllbGRzmgEAAAAAAACAAgAAYgAAABAAAAAAAAAA
wA8AAGIAAAACAAAAggAAAAQAAAADIAFEAQAAAGARAADSAgAAAAAAAAAAAAAAAAAAAAMAACAAAAAA
AAAAAAAAAAcAAAAAAAAAAAAAAAAAAABgEQAAAAAAAIIAAAAIAAAA5QX//wAAAAByAwAAAAAAAAAA
AAAAAAAAAAAAAJIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAADSAwAAAAAAAPADAABiAAAAAgAA
ACIEAAAAAAAAAQAAACkAAAAiBAAAAAAAAHkAAAApAAAAYBEAAKIEAAAAAAAAcgAAACwAAAAsAAAA
AAAAAAEAAAD/////////////////////AAAAABQAAAA8AAAAKAAAAMoAAAAAAAAA0AAAAOAEAADw
BAAAAAAAABMAAABSAAAAGQAAAEluY2x1ZGVQdWJsaWNDb25zdHJ1Y3RvcnOaAQAAAAAAAIACAABi
AAAAEAAAAAAAAADADwAAYgAAAAIAAACCAAAABAAAAAMgAUQBAAAAgBIAANICAAAAAAAAAAAAAAAA
AAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAIASAAAAAAAAggAAAAgAAADlBf//AAAA
AHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAANIDAAAAAAAA
8AMAAGIAAAACAAAAIgQAAAAAAAAZAQAAeQAAACIEAAAAAAAAeQAAACkAAACAEgAAogQAAAAAAABy
AAAALAAAACwAAAAAAAAAAQAAAP////////////////////+MAAAAPAAAAMgAAABQAAAAygAAAAAA
AADQAAAA4AQAAPAEAAAAAAAAEwAAAFIAAAAaAAAASW5jbHVkZURlZmF1bHRBY2Nlc3NGaWVsZHOa
AQAAAAAAAIACAABiAAAAEAAAAAAAAADADwAAYgAAAAIAAACCAAAABAAAAAMgAUQBAAAAoBMAANIC
AAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAKATAAAAAAAAggAA
AAgAAADlBf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAA
AQAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAABAAAAoQAAACIEAAAAAAAAeQAAACkAAACg
EwAAogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAUAAAADwA
AABkAAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAFIAAAAaAAAASW5jbHVkZVByaXZhdGVD
b25zdHJ1Y3RvcnOaAQAAAAAAAIACAABiAAAAEAAAAAAAAADADwAAYgAAAAIAAACCAAAABAAAAAMg
AUQBAAAAwBQAANICAAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
AMAUAAAAAAAAggAAAAgAAADlBf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAA
AAAAANAAAABiAAAAAQAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAAZAQAAKQAAACIEAAAA
AAAAeQAAACkAAADAFAAAogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//////////////////
//+MAAAAFAAAAMgAAAAoAAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAFIAAAATAAAASW5j
bHVkZVB1YmxpY0ZpZWxkc5oBAAAAAAAAgAIAAGIAAAAQAAAAAAAAAMAPAABiAAAAAgAAAIIAAAAE
AAAAAyABRAEAAADgFQAA0gIAAAAAAAAAAAAAAAAAAAADAAAgAAAAAAAAAAAAAAAHAAAAAAAAAAAA
AAAAAAAA4BUAAAAAAACCAAAACAAAAOUF//8AAAAAcgMAAAAAAAAAAAAAAAAAAAAAAACSAwAAAAAA
AMoAAAAAAAAA0AAAAGIAAAABAAAA0gMAAAAAAADwAwAAYgAAAAIAAAAiBAAAAAAAAI0AAAChAAAA
IgQAAAAAAAB5AAAAKQAAAOAVAACiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////
/////////0YAAABQAAAAggAAAGQAAADKAAAAAAAAANAAAADgBAAA8AQAAAAAAAATAAAAUgAAABUA
AABJbmNsdWRlUHJpdmF0ZU1ldGhvZHOaAQAAAAAAAIACAABiAAAAEAAAAAAAAADADwAAYgAAAAIA
AACCAAAABAAAAAMgAUQBAAAAABcAANICAAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAA
AAAAAAAAAAAAAAAAAAAXAAAAAAAAggAAAAgAAADlBf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAA
kgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAACN
AAAAUQAAACIEAAAAAAAAeQAAACkAAAAAFwAAogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//
//////////////////9GAAAAKAAAAIIAAAA8AAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAA
AFIAAAAXAAAASW5jbHVkZVByb3RlY3RlZE1ldGhvZHOaAQAAAAAAAIACAABiAAAAEAAAAAAAAADA
DwAAYgAAAAIAAACCAAAABAAAAAMgAUQBAAAAIBgAANICAAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAA
AAAAAAAABwAAAAAAAAAAAAAAAAAAACAYAAAAAAAAggAAAAgAAADlBf//AAAAAHIDAAAAAAAAAAAA
AAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAANIDAAAAAAAA8AMAAGIAAAACAAAA
IgQAAAAAAAABAAAAeQAAACIEAAAAAAAAeQAAACkAAAAgGAAAogQAAAAAAAByAAAALAAAACwAAAAA
AAAAAQAAAP////////////////////8AAAAAPAAAADwAAABQAAAAygAAAAAAAADQAAAA4AQAAPAE
AAAAAAAAEwAAAFIAAAAgAAAASW5jbHVkZURlZmF1bHRBY2Nlc3NDb25zdHJ1Y3RvcnOaAQAAAAAA
AIACAABiAAAAEAAAAAAAAADADwAAYgAAAAIAAACCAAAABAAAAAMgAUQBAAAAQBkAANICAAAAAAAA
AAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAEAZAAAAAAAAggAAAAgAAADl
Bf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAANID
AAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAACNAAAAKQAAACIEAAAAAAAAeQAAACkAAABAGQAAogQA
AAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////9GAAAAFAAAAIIAAAAoAAAA
ygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAFIAAAAUAAAASW5jbHVkZVB1YmxpY01ldGhvZHOa
AQAAAAAAAIACAABiAAAAEAAAAAAAAADADwAAYgAAAAIAAACCAAAABAAAAAMgAUQBAAAAYBoAANIC
AAAAAAAAAAAAAAAAAAAAAwAAIAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAGAaAAAAAAAAggAA
AAgAAADlBf//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAA
AQAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAABAAAAUQAAACIEAAAAAAAAeQAAACkAAABg
GgAAogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAKAAAADwA
AAA8AAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAFIAAAAcAAAASW5jbHVkZVByb3RlY3Rl
ZENvbnN0cnVjdG9yc5oBAAAAAAAAgAIAAGIAAAAQAAAAAAAAAMAPAABiAAAAAgAAAIIAAAAEAAAA
AyABRAEAAACAGwAA0gIAAAAAAAAAAAAAAAAAAAADAAAgAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAA
AAAAgBsAAAAAAACCAAAACAAAAOUF//8AAAAAcgMAAAAAAAAAAAAAAAAAAAAAAACSAwAAAAAAAMoA
AAAAAAAA0AAAAGIAAAABAAAA0gMAAAAAAADwAwAAYgAAAAIAAAAiBAAAAAAAABkBAAChAAAAIgQA
AAAAAAB5AAAAKQAAAIAbAACiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////////
/////4wAAABQAAAAyAAAAGQAAADKAAAAAAAAANAAAADgBAAA8AQAAAAAAAATAAAAUgAAABQAAABJ
bmNsdWRlUHJpdmF0ZUZpZWxkc5oBAAAAAAAAgAIAAGIAAAAQAAAAAAAAAMAPAABiAAAAAgAAAIIA
AAAEAAAAAyABRAEAAACgHAAA0gIAAAAAAAAAAAAAAAAAAAADAAAgAAAAAAAAAAAAAAAHAAAAAAAA
AAAAAAAAAAAAoBwAAAAAAACCAAAACAAAAOUF//8AAAAAcgMAAAAAAAAAAAAAAAAAAAAAAACSAwAA
AAAAAMoAAAAAAAAA0AAAAGIAAAABAAAA0gMAAAAAAADwAwAAYgAAAAIAAAAiBAAAAAAAAI0AAAB5
AAAAIgQAAAAAAAB5AAAAKQAAAKAcAACiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////
/////////////0YAAAA8AAAAggAAAFAAAADKAAAAAAAAANAAAADgBAAA8AQAAAAAAAATAAAAUgAA
ABsAAABJbmNsdWRlRGVmYXVsdEFjY2Vzc01ldGhvZHMAAAAAkgMAAAAAAADKAAAAAAAAANAAAABi
AAAAAQAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAADxAAAAgwAAACIEAAAAAAAAkQEAAMkA
AADADwAAogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////94AAAAQQAA
AEABAAClAAAAygAAAAAAAADQAAAAYgAAAA8AAACaAQAAAAAAACAIAABiAAAAEAAAAAAAAADADwAA
YgAAAAIAAACCAAAABAAAAAABAEQBAAAAcB4AAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAABw
HgAAAAAAAIIAAAAIAAAAAQb//wAAAAByAwAAAAAAAAAAAAAAAAAAAAAAAJIDAAAAAAAAygAAAAAA
AADQAAAAYgAAAAIAAADSAwAAAAAAAPADAABiAAAAAgAAACIEAAAAAAAAAQAAAAEAAAAiBAAAAAAA
AHkAAAApAAAAcB4AANIDAAAAAAAAYAQAAGIAAAABAAAAUgAAAAwAAABDb25zdHJ1Y3RvcnNwHgAA
ogQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAAAAAADwAAAAU
AAAAygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAJoBAAAAAAAAIAgAAGIAAAAQAAAAAAAAAMAP
AABiAAAAAgAAAIIAAAAEAAAAAAEARAEAAACgHwAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
AKAfAAAAAAAAggAAAAgAAAABBv//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAA
AAAAANAAAABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAACNAAAAAQAAACIEAAAA
AAAAeQAAACkAAACgHwAA0gMAAAAAAABgBAAAYgAAAAEAAABSAAAABwAAAE1ldGhvZHOgHwAAogQA
AAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////9GAAAAAAAAAIIAAAAUAAAA
ygAAAAAAAADQAAAA4AQAAPAEAAAAAAAAEwAAAJoBAAAAAAAAIAgAAGIAAAAQAAAAAAAAAMAPAABi
AAAAAgAAAIIAAAAEAAAAAAEARAEAAADQIAAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAANAg
AAAAAAAAggAAAAgAAAABBv//AAAAAHIDAAAAAAAAAAAAAAAAAAAAAAAAkgMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAgAAANIDAAAAAAAA8AMAAGIAAAACAAAAIgQAAAAAAAAZAQAAAQAAACIEAAAAAAAA
eQAAACkAAADQIAAA0gMAAAAAAABgBAAAYgAAAAEAAABSAAAABgAAAEZpZWxkc9AgAACiBAAAAAAA
AHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////4wAAAAAAAAAyAAAABQAAADKAAAA
AAAAANAAAADgBAAA8AQAAAAAAAATAAAAYBEAAEAZAADAFAAAYBoAAAAXAABAEAAAIBgAAKAcAACA
EgAAoBMAAOAVAACAGwAA8AQAAAAAAAATAAAAAgUAAAAAAAC6AAAAAAAAAFIAAAASAAAAZml4ZWRQ
cmV2aW91c1JpZ2h0AQAAADAOAACRAQAAugAAAAAAAABSAAAAEAAAAGZpeGVkUHJldmlvdXNUb3AB
AAAAYAUAAAEAAADqAAAAAAAAAAABAABiAAAABgAAAFAOAABSAAAAEwAAAEluY2x1ZGVTdXBlcmNs
YXNzZXOgBQAAUgAAABYAAABJbmNsdWRlQWJzdHJhY3RNZXRob2RzcAIAAFIAAAAUAAAASW5jbHVk
ZUJyaWRnZU1ldGhvZHMAAAAAkgMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAANIDAAAAAAAA8AMA
AGIAAAACAAAAIgQAAAAAAAALAAAACwAAACIEAAAAAAAAdwIAAH0BAACgAQAAogQAAAAAAAByAAAA
LAAAACwAAAAAAAAAAAAAAP////////////////////8FAAAABQAAAEABAADDAAAAygAAAAAAAADQ
AAAAYgAAAAUAAABQDgAAoAUAAHACAADwBgAAwA8AAPAEAAAAAAAAEwAAAEYFBAADAAAASWNvbgAA
AAAAAAAAEAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9scGhp
blIAAAAYAAAASW1hZ2VSZWxhdGl2ZUZpbGVMb2NhdG9yugAAAAAAAABSAAAABwAAAGN1cnJlbnRS
AAAAEQAAAENvbnRhaW5lclZpZXcuaWNvDgIfAFNUQkV4dGVybmFsUmVzb3VyY2VMaWJyYXJ5UHJv
eHkAAAAAUgAAABAAAABkb2xwaGluZHIwMDUuZGxsAAAAAA=='))!

(ResourceIdentifier class: JavaWrapperWizard name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAABGAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAGAQsAU3lzdGVtQ29sb3IAAAAAHwAAAAYC
BQBQb2ludAAAAADpAwAA6QMAAAcCAAAAAAAAAAAAAAAAAACgAQAABgENAEZyYW1pbmdMYXlvdXQA
AAAA6gAAAAAAAADwAAAAYgAAAAYAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAANAAAAQ29udGFp
bmVyVmlld2IAAAAPAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAAAAAARAEAAgCAAgAAAAAAAAIC
AAAAAAAAHwAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAgAIAAEICAAAAAAAA6gAAAAAAAADwAAAAYgAA
AAoAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAAKAAAAUHVzaEJ1dHRvbmIAAAARAAAAAAAAAIAC
AABiAAAAAgAAAIIAAAAEAAAAACABRAEAAAAgAwAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAA
ACADAAAAAAAAggAAAAgAAABlBP//AAAAAEYFEgAEAAAAQ29tbWFuZERlc2NyaXB0aW9uAAAAALoA
AAAAAAAAUgAAAAYAAABmaW5pc2hSAAAABwAAACZGaW5pc2gBAAAAAQAAAAAAAAAAAAAAAQAAAAYB
DwBNZXNzYWdlU2VxdWVuY2UAAAAAygAAAAAAAADQAAAAYgAAAAIAAAAGAwsATWVzc2FnZVNlbmQA
AAAAugAAAAAAAABSAAAAEAAAAGNyZWF0ZUF0OmV4dGVudDpiAAAAAgAAACICAAAAAAAAhQIAAAEA
AAAiAgAAAAAAAHkAAAA5AAAAIAMAACIEAAAAAAAAugAAAAAAAABSAAAABQAAAHRleHQ6YgAAAAEA
AABSAAAABwAAACZGaW5pc2ggAwAABgEPAFdJTkRPV1BMQUNFTUVOVAAAAAByAAAALAAAACwAAAAA
AAAAAAAAAP////////////////////9CAQAAAAAAAH4BAAAcAAAAygAAAAAAAADQAAAAYgAAAAAA
AAAiAgAAAAAAAMEAAADBAAAAAAAAABMAAABGCBIAAQAAAEZyYW1pbmdDb25zdHJhaW50cwAAAAC6
AAAAAAAAAFIAAAAOAAAAZml4ZWRWaWV3UmlnaHSJ////ugAAAAAAAABSAAAAEAAAAGZpeGVkUGFy
ZW50UmlnaHRN////ugAAAAAAAABSAAAADgAAAGZpeGVkUGFyZW50VG9wAQAAALoAAAAAAAAAUgAA
ABEAAABmaXhlZFBhcmVudEJvdHRvbQEAAACaAQAAAAAAADADAABiAAAAEQAAAAAAAACAAgAAYgAA
AAIAAACCAAAABAAAAAAgAUQBAAAA4AUAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAADgBQAA
AAAAAIIAAAAIAAAAZQT//wAAAACSAwAAAAAAALoAAAAAAAAAUgAAAAYAAABjYW5jZWxSAAAABgAA
AENhbmNlbAEAAAABAAAAAAAAAAAAAAABAAAA4gMAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAACIE
AAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAAA5AwAAAQAAACICAAAAAAAAeQAAADkAAADgBQAAIgQA
AAAAAACgBAAAYgAAAAEAAABSAAAABgAAAENhbmNlbOAFAADiBAAAAAAAAHIAAAAsAAAALAAAAAAA
AAABAAAA/////////////////////5wBAAAAAAAA2AEAABwAAADKAAAAAAAAANAAAAAgBQAAMAUA
AAAAAAATAAAAQgUAAAAAAABgBQAAif///4AFAAABAAAAoAUAAAEAAADABQAAAQAAAJoBAAAAAAAA
MAMAAGIAAAARAAAAAAAAAIACAABiAAAAAgAAAIIAAAAEAAAAACABRAEAAABQBwAAAAAAAAAAAAAA
AAAABwAAAAAAAAAAAAAAAAAAAFAHAAAAAAAAggAAAAgAAABlBP//AAAAAJIDAAAAAAAAugAAAAAA
AABSAAAADAAAAHByZXZpb3VzUGFnZVIAAAAHAAAAPCAmQmFjawEAAAABAAAAAAAAAAAAAAABAAAA
4gMAAAAAAADKAAAAAAAAANAAAABiAAAAAgAAACIEAAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAAAD
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
AAABAAAAIAAAAHAWAADiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA////////////////////
/zwAAABpAAAA2AEAAH0AAADKAAAAAAAAANAAAAAgBQAAMAUAAAAAAAATAAAAQgUAAAAAAAAwCgAA
eQAAAIAFAAABAAAAMA0AAAEAAAAQEAAAKQAAAJoBAAAAAAAAsA4AAGIAAAAQAAAAAAAAACAOAABi
AAAAAgAAAIIAAAAEAAAAAAEARAEAAABAGAAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAEAY
AAAAAAAAggAAAAgAAAAxBf//AAAAABIPAAAAAAAAAAAAAAAAAAAAAAAA4gMAAAAAAADKAAAAAAAA
ANAAAABiAAAAAgAAACIEAAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAAABAAAAIwEAACICAAAAAAAA
yQAAACkAAABAGAAAIgQAAAAAAACgBAAAYgAAAAEAAABSAAAAEAAAAFNtYWxsdGFsayBjbGFzczpA
GAAA4gQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAkQAAAGQA
AAClAAAAygAAAAAAAADQAAAAIAUAADAFAAAAAAAAEwAAAEIFAAAAAAAAMAoAAAEAAABQCgAAyQAA
ADANAAApAAAAEBAAACkAAACaAQAAAAAAADADAABiAAAAEQAAAAAAAAAgDgAAYgAAAAIAAACCAAAA
BAAAAAAgAUQBAAAAgBkAAAAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACAGQAAAAAAAIIAAAAI
AAAAZQT//wAAAACSAwAAAAAAAKASAABSAAAABgAAAE5ldy4uLgEAAAABAAAAAAAAAAAAAAABAAAA
4gMAAAAAAADKAAAAAAAAANAAAABiAAAAAwAAACIEAAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAADd
AAAAHQEAACICAAAAAAAAZQAAACkAAACAGQAAIgQAAAAAAAC6AAAAAAAAAFIAAAAKAAAAaXNFbmFi
bGVkOmIAAAABAAAAIAAAAIAZAAAiBAAAAAAAAKAEAABiAAAAAQAAAFIAAAAGAAAATmV3Li4ugBkA
AOIEAAAAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////bgAAAI4AAACgAAAA
ogAAAMoAAAAAAAAA0AAAACAFAAAwBQAAAAAAABMAAABCBQAAAAAAABANAAAVAAAAUAoAAGUAAAC6
AAAAAAAAAFIAAAAQAAAAZml4ZWRQcmV2aW91c1RvcPv///8wDQAA+////+oAAAAAAAAAAAEAAGIA
AAAEAAAAcBYAAFIAAAAJAAAASmF2YUNsYXNzcBEAAFIAAAAOAAAAU21hbGx0YWxrQ2xhc3MAAAAA
4gMAAAAAAADKAAAAAAAAANAAAABiAAAAAQAAACIEAAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAAAB
AAAAAQAAACICAAAAAAAAsQMAAFcDAAAgDgAA4gQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP//
//////////////////8AAAAAAAAAANgBAACrAQAAygAAAAAAAADQAAAAYgAAAAcAAAAwFQAAoA4A
ADAQAABwFgAAQBgAAIAZAABwEQAAMAUAAAAAAAATAAAAAg4AAAAAAAAFAAAAmgEAAAAAAACQAgAA
YgAAAA8AAAAAAAAAUA0AAGIAAAACAAAAggAAAAQAAAAAAABEAQACADAcAAAAAAAAAgIAAAAAAAAf
AAAAAAAAAAcAAAAAAAAAAAAAAAAAAAAwHAAAQgIAAAAAAADqAAAAAAAAAPAAAABiAAAACAAAAJoB
AAAAAAAAgBEAAGIAAAAOAAAAAAAAADAcAABiAAAAAgAAAIIAAAAEAAAAAAABRAEAAgCwHAAAAAAA
AAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAALAcAADiEwAAAAAAAJoAAAAAAAAAUgAAABYAAABDVSBK
YXZhIFdyYXBwZXIgV2l6YXJkUgAAAB0AAABKYXZhc1dyYXBwZXJTZXR0aW5nc1ByZXNlbnRlclIA
AAAMAAAARGVmYXVsdCB2aWV3AAAAAOIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAEAAAAiBAAAAAAA
AEAEAABiAAAAAgAAACICAAAAAAAACwAAAKsAAAAiAgAAAAAAAKcDAABBAQAAsBwAAOIEAAAAAAAA
cgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////////BQAAAFUAAADYAQAA9QAAABAVAAAw
BQAAAAAAABUAAABCBQAAAAAAADAKAAALAAAAgAUAAAEAAAAwDQAAHwAAABAQAABBAQAAmgEAAAAA
AACwDgAAYgAAABAAAAAAAAAAMBwAAGIAAAACAAAAggAAAAQAAAAAAQBEAQAAAOAdAAAAAAAAAAAA
AAAAAAAHAAAAAAAAAAAAAAAAAAAA4B0AAAAAAACCAAAACAAAADEF//8AAAAAEg8AAAAAAAAAAAAA
AAAAAAAAAADiAwAAAAAAAMoAAAAAAAAA0AAAAGIAAAACAAAAIgQAAAAAAABABAAAYgAAAAIAAAAi
AgAAAAAAAAEAAAApAAAAIgIAAAAAAACxAwAAZQAAAOAdAAAiBAAAAAAAAKAEAABiAAAAAQAAAFIA
AABrAAAAUGxlYXNlIHNldCB0aGUgb3B0aW9ucyB0byBzZWxlY3QgdGhlIGZpZWxkcyBhbmQgbWV0
aG9kcyBmb3Igd2hpY2ggdGhlIFdpemFyZCB3aWxsIGdlbmVyYXRlIHdyYXBwZXIgbWV0aG9kcy7g
HQAA4gQAAAAAAAByAAAALAAAACwAAAAAAAAAAQAAAP////////////////////8AAAAAFAAAANgB
AABGAAAAygAAAAAAAADQAAAAIAUAADAFAAAAAAAAEwAAAEIFAAAAAAAAMAoAAAEAAACABQAAAQAA
ADANAAABAAAAEBAAAGUAAACaAQAAAAAAAJoAAAAAAAAAwAEAAFIAAAANAAAAQ2FyZENvbnRhaW5l
cmIAAAAQAAAAAAAAADAcAABiAAAAAgAAAIIAAAAEAAAAAAAARAEAAgAgHwAAAAAAAAAAAAAAAAAA
BwAAAAAAAAAAAAAAAAAAACAfAADCDQAAAAAAAMoAAAAAAAAA0AAAAGIAAAAFAAAAAg4AAAAAAABS
AAAADAAAAENvbnN0cnVjdG9yc5oBAAAAAAAAmgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9u
IENvbnRyb2xzUgAAABkAAABNdWx0aXBsZVNlbGVjdGlvbkxpc3RWaWV3YgAAAB4AAAAAAAAAIB8A
AGIAAAACAAAAggAAAAQAAABJEAFEAQQAANAfAABGAwkAAgAAAExpc3RNb2RlbAAAAADKAAAAAAAA
ANAAAAAgBQAAAAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAFIAAAAHAAAARG9s
cGhpblIAAAAMAAAAU2VhcmNoUG9saWN5ugAAAAAAAABSAAAACAAAAGlkZW50aXR5AAAAAAAAAAAH
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
AAAAAAAAAAAAAAAAAOIDAAAAAAAAygAAAAAAAADQAAAAYgAAAAIAAAAiBAAAAAAAAEAEAABiAAAA
AgAAACICAAAAAAAAAQAAAC8DAAAiAgAAAAAAALEDAAApAAAAkDIAACIEAAAAAAAAoAQAAGIAAAAB
AAAAUgAAADUAAAAmUmVnaXN0ZXIgbmV3IHdyYXBwZXIgY2xhc3MgYWZ0ZXIgbWV0aG9kIGdlbmVy
YXRpb24gP5AyAADiBAAAAAAAAHIAAAAsAAAALAAAAAAAAAABAAAA/////////////////////wAA
AACXAQAA2AEAAKsBAADKAAAAAAAAANAAAAAgBQAAMAUAAAAAAAATAAAAQgUAAAAAAAAwCgAAAQAA
AIAFAAABAAAAMA0AABUAAAAQEAAAKQAAAJoBAAAAAAAAsA4AAGIAAAAQAAAAAAAAAAAxAABiAAAA
AgAAAIIAAAAEAAAAAAEARAEAAABANAAAAAAAAAAAAAAAAAAABwAAAAAAAAAAAAAAAAAAAEA0AAAA
AAAAggAAAAgAAAAxBf//AAAAABIPAAAAAAAAAAAAAAAAAAAAAAAA4gMAAAAAAADKAAAAAAAAANAA
AABiAAAAAgAAACIEAAAAAAAAQAQAAGIAAAACAAAAIgIAAAAAAAABAAAAKQAAACICAAAAAAAAsQMA
AGUAAABANAAAIgQAAAAAAACgBAAAYgAAAAEAAABSAAAApgAAAFBsZWFzZSBjb25maXJtIHRoYXQg
eW91IHdpc2ggdG8gZ2VuZXJhdGUgdGhlIG1ldGhvZHMgbmFtZWQgYmVsb3cuICBObyBleGlzdGlu
ZyBtZXRob2RzIHdpbGwgYmUgb3Zld3JpdHRlbiB1bmxlc3MgdGhleSBhcmUgaW4gdGhlICoqYXV0
by1nZW5lcmF0ZWQqKiBtZXRob2QgY2F0ZWdvcnkuDQpANAAA4gQAAAAAAAByAAAALAAAACwAAAAA
AAAAAQAAAP////////////////////8AAAAAFAAAANgBAABGAAAAygAAAAAAAADQAAAAIAUAADAF
AAAAAAAAEwAAAEIFAAAAAAAAMAoAAAEAAACABQAAAQAAADANAAABAAAAEBAAAGUAAACaAQAAAAAA
AJoAAAAAAAAA8B8AAFIAAAAIAAAATGlzdFZpZXdiAAAAHgAAAAAAAAAAMQAAYgAAAAIAAACCAAAA
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'))!

