| package |
package := Package name: 'CU Java Wrapper Generation'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

Objects and methods for using Java''s introspection to generate JNIPort Wrapper Classes semi-automatically.

Note, the code in this package is exceedingly messy.  Of course, code generation is always going to be farily messy, but in the case the real problem is trying to deal with a problem that varies in several independent directions -- ghost classes vs. non-ghosts, class-side vs. instance-side, fields vs. methods, primitive types vs. object references.  The result is very oddly factored in an attempt to handle the difficulty without gross duplication of code.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.06'.


package classNames
	add: #AmbiguousJavaMethodError;
	add: #JavaAmbiguousSelectorGenerator;
	add: #JavaClassNameMethodGenerator;
	add: #JavaClassWrapperGenerator;
	add: #JavaClassWrapperInstaller;
	add: #JavaCodeWrapperGenerator;
	add: #JavaConstructorWrapperGenerator;
	add: #JavaFieldAccessorGenerator;
	add: #JavaFieldGetterGenerator;
	add: #JavaFieldSetterGenerator;
	add: #JavaInstanceClassWrapperGenerator;
	add: #JavaListingMethodGenerator;
	add: #JavaMemberWrapperGenerator;
	add: #JavaMethodGenerator;
	add: #JavaMethodWrapperGenerator;
	add: #JavaSNIConstructorGenerator;
	add: #JavaStaticWrapperGenerator;
	add: #JavaWrapperGeneratorSettings;
	yourself.

package methodNames
	add: #JavaArrayClassStatic -> #keywordBase;
	add: #JavaArrayClassStatic -> #parameterBase;
	add: #JavaClassStatic -> #conversionToJavaString;
	add: #JavaInterfaceStatic -> #conversionToJavaString;
	add: #JavaObject -> #ambiguousJavaMethod:alternatives:;
	add: #JavaPrimitiveBooleanStatic -> #conversionToJavaString;
	add: #JavaPrimitiveBooleanStatic -> #conversionToSmalltalkString;
	add: #JavaPrimitiveCharStatic -> #conversionToJavaString;
	add: #JavaPrimitiveCharStatic -> #conversionToSmalltalkString;
	add: #JavaPrimitiveStatic -> #conversionToSmalltalkString;
	add: #JavaPrimitiveStatic -> #parameterBase;
	add: #JavaStatic -> #conversionToJavaString;
	add: #JavaStatic -> #conversionToSmalltalkString;
	add: #JavaStatic -> #genericTypeName;
	add: #JavaStatic -> #isJavaLangString;
	add: #JavaStatic -> #keywordBase;
	add: #JavaStatic -> #keywordName:;
	add: #JavaStatic -> #makeDictionary:;
	add: #JavaStatic -> #makePoolConstantsDictionary:;
	add: #JavaStatic -> #makePoolDictionary:;
	add: #JavaStatic -> #parameterBase;
	add: #JavaStatic -> #parameterName:;
	add: #JavaStatic -> #populateDictionary:keyBase:;
	add: #JVMSettings -> #wrapperGeneratorSettings;
	add: 'JavaClassStatic class' -> #genericTypeName;
	add: 'JavaInterfaceStatic class' -> #genericTypeName;
	add: 'JavaObject class' -> #inheritedConstructorSelectors;
	add: 'JavaPrimitiveStatic class' -> #genericTypeName;
	add: 'JavaStatic class' -> #genericTypeName;
	yourself.

package globalNames
	add: #JavaWrapperConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #JavaClassWrapperGenerator
	instanceVariableNames: 'classStatic targetClass installer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JavaClassWrapperInstaller
	instanceVariableNames: 'source destination settings isForClassSide methodFilter classGeneratorCache installed problems'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JavaMethodGenerator
	instanceVariableNames: 'targetClass output generateComment useJniObject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #AmbiguousJavaMethodError
	instanceVariableNames: 'alternatives'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaClassWrapperGenerator subclass: #JavaInstanceClassWrapperGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaClassWrapperGenerator subclass: #JavaStaticWrapperGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaAmbiguousSelectorGenerator
	instanceVariableNames: 'selector entries'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaClassNameMethodGenerator
	instanceVariableNames: 'className'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaListingMethodGenerator
	instanceVariableNames: 'selector entries'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaMemberWrapperGenerator
	instanceVariableNames: 'reflection name generateLongKeywords includeTypeInSelector selectorCache parametersCache genericTypeNameCache jniSignatureCache'
	classVariableNames: ''
	poolDictionaries: 'JavaWrapperConstants'
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaSNIConstructorGenerator
	instanceVariableNames: 'selector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMemberWrapperGenerator subclass: #JavaCodeWrapperGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMemberWrapperGenerator subclass: #JavaFieldAccessorGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaCodeWrapperGenerator subclass: #JavaConstructorWrapperGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaCodeWrapperGenerator subclass: #JavaMethodWrapperGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaFieldAccessorGenerator subclass: #JavaFieldGetterGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaFieldAccessorGenerator subclass: #JavaFieldSetterGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMSubSettings subclass: #JavaWrapperGeneratorSettings
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JavaWrapperConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaArrayClassStatic methodsFor!

keywordBase
	"private -- answer a suitable String to use for a keyword for parameter of this type without the :"

	^ self elementClass keywordBase , 'Array'.
!

parameterBase
	"private -- answer a suitable String to use for a parameter of this type"

	| base |

	base := self elementClass parameterBase.

	"ok, I admit this is overdone"
	"not to mention Anglo-centric"
	#(
		('y' 'ies')
		('ium' 'ia')
	"	('ma' 'mata')	-- e.g. stima/stigmata, forma/formata"
		('s' 'ses')
		('x' 'xes')	"and if someone is using arrays of oxen... well, life's a bitch"
	) do:
		[:each | (base endsWith: each first) ifTrue: [^ (base allButLast: each first size) , each second]].

	^  base , 's'.! !
!JavaArrayClassStatic categoriesFor: #keywordBase!accessing!generating!private! !
!JavaArrayClassStatic categoriesFor: #parameterBase!accessing!generating!private! !

!JavaClassStatic methodsFor!

conversionToJavaString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the JVM to use (if necessary)

	will create a string which uses converts the Smalltalk value named by the first string into a Java object"

	^ self isJavaLangString
		ifTrue: ['(%1 asJavaString: %2)']
		ifFalse: ['%1'].! !
!JavaClassStatic categoriesFor: #conversionToJavaString!constants!private! !

!JavaClassStatic class methodsFor!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	^ 'Object'.
! !
!JavaClassStatic class categoriesFor: #genericTypeName!constants!generating!public! !

!JavaInterfaceStatic methodsFor!

conversionToJavaString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the JVM to use (if necessary)

	will create a string which uses converts the Smalltalk value named by the first string into a Java object"

	^ '%1'.! !
!JavaInterfaceStatic categoriesFor: #conversionToJavaString!constants!private! !

!JavaInterfaceStatic class methodsFor!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	^ 'Object'.
! !
!JavaInterfaceStatic class categoriesFor: #genericTypeName!constants!generating!public! !

!JavaObject methodsFor!

ambiguousJavaMethod: aSelector alternatives: aCollection
	"private -- trigger an AmbiguousJavaMethodError to say that an automatically
	generated method could have referred to one of several Java methods.
	aCollection is an Array of Selectors of 'long form' generated methods with
	the same short form as the erroneous one"

	AmbiguousJavaMethodError
		signal: (aSelector , ' is ambiguous')
		selector: aSelector
		alternatives: aCollection.! !
!JavaObject categoriesFor: #ambiguousJavaMethod:alternatives:!exception signaling!private! !

!JavaObject class methodsFor!

inheritedConstructorSelectors
	"answer an IdentitySet of the selectors of our inherited automatically generated (including ghost methods)
	methods which wrap Java constructors.
	Note that this does not include our own selectors"

	| answer |

	answer := IdentitySet new.
	self allSuperclassesDo: [:each | [answer addAll: each generatedConstructorSelectors] on: MessageNotUnderstood do: [:err | ]].

	^ answer.! !
!JavaObject class categoriesFor: #inheritedConstructorSelectors!constants!listing wrapper methods!public! !

!JavaPrimitiveBooleanStatic methodsFor!

conversionToJavaString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the JVM to use (if necessary)

	will create a string which uses converts the Smalltalk value named by the first string into a Java object"

	^ '(%1 ifTrue: [1] ifFalse: [0])'.!

conversionToSmalltalkString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the object to generate a wrapper for the answer (if necessary)

	will answer code which converts the value named by the first string into a Smalltalk object"

	^ '%1 == 1'.! !
!JavaPrimitiveBooleanStatic categoriesFor: #conversionToJavaString!constants!private! !
!JavaPrimitiveBooleanStatic categoriesFor: #conversionToSmalltalkString!constants!private! !

!JavaPrimitiveCharStatic methodsFor!

conversionToJavaString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the JVM to use (if necessary)

	will create a string which uses converts the Smalltalk value named by the first string into a Java object"

	^ '(%1 asInteger)'.!

conversionToSmalltalkString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the object to generate a wrapper for the answer (if necessary)

	will answer code which converts the value named by the first string into a Smalltalk object"

	^ '%1 < 256 ifTrue: [Character value: %1] ifFalse: [%1]'.! !
!JavaPrimitiveCharStatic categoriesFor: #conversionToJavaString!constants!private! !
!JavaPrimitiveCharStatic categoriesFor: #conversionToSmalltalkString!constants!private! !

!JavaPrimitiveStatic methodsFor!

conversionToSmalltalkString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the object to generate a wrapper for the answer (if necessary)

	will answer code which converts the value named by the first string into a Smalltalk object"

	^ '%1'.!

parameterBase
	"private -- answer a suitable String to use for a parameter of this type"

	^ self name.
! !
!JavaPrimitiveStatic categoriesFor: #conversionToSmalltalkString!constants!private! !
!JavaPrimitiveStatic categoriesFor: #parameterBase!generating!private! !

!JavaPrimitiveStatic class methodsFor!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	^ self javaTypeName asString capitalized.
! !
!JavaPrimitiveStatic class categoriesFor: #genericTypeName!constants!generating!public! !

!JavaStatic methodsFor!

conversionToJavaString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the JVM to use (if necessary)

	will create a string which uses converts the Smalltalk value named by the first string into a Java object"

	^ '%1'.!

conversionToSmalltalkString
	"answer a Win32 format string such that subsituting an array with the following contents:

		String which names the variable holding the value.
		String which names the object to generate a wrapper for the answer (if necessary)

	will answer code which converts the value named by the first string into a Smalltalk object"

	^ '%2 wrapJNIObject: %1'.!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	^ self class genericTypeName.
!

isJavaLangString
	"answer whether we stand for the class java.lang.String"

	^ self == (self jvm findClass: #'java.lang.String').!

keywordBase
	"private -- answer a suitable String to use for a keyword for parameter of this type without the :"

	^ self name.
!

keywordName: useLongForm
	"answer a suitable name to use for a keyword for parameter of this type; if the Bool
	useLongForm is true then a longer form will be used that is less likely to be ambiguous.
	NB: the terminating ':' is *not* included"

	| base seps |

	base := self keywordBase.
	^ useLongForm
		ifTrue: [base copyWithoutAll: '$.']
		ifFalse: [base allButFirst: ((base lastIndexOf: $.) max: (base lastIndexOf: $$))].!

makeDictionary: aString
	"answer a LookupTable populated with entries in the form:
		(aString , <name>) := <value>
	for each public static final field which is of one of the primitive types	or java.lang.String"

	^ self
		populateDictionary: LookupTable new
		keyBase: aString.
!

makePoolConstantsDictionary: aString
	"answer a PoolConstantsDictionary populated with entries in the form:
		(aString , <name>) := <value>
	for each public static final field which is of one of the primitive types	or java.lang.String"

	^ self
		populateDictionary: PoolConstantsDictionary new
		keyBase: aString.
!

makePoolDictionary: aString
	"answer a PoolDictionary populated with entries in the form:
		aString , <name> = <value>
	for each public static final field which is of one of the primitive types or java.Lang.String"

	^ self
		populateDictionary: PoolConstantsDictionary new
		keyBase: aString.!

parameterBase
	"private -- answer a suitable String to use for a base for the name of a parameter of this type"

	| name index |

	"drop any package name or enclosing classname prefix"
	name := self name.
	^ name allButFirst: ((name lastIndexOf: $.) max: (name lastIndexOf: $$)).
!

parameterName: anInteger
	"answer a suitable name to use for a parameter of this type, if anInteger is > 0 then this is
	one of several parameters of the same type"

	| base str |

	base := self parameterBase.

	str := String writeStream.

	base first isLowercase ifFalse:
		[str nextPutAll: (base first isVowel ifTrue: ['an'] ifFalse: ['a'])].

	str nextPutAll: base.

	anInteger > 0 ifTrue: [str display: anInteger].

	^ str contents.!

populateDictionary: aDictionary keyBase: aString
	"populate aDictionary with entries in the form:
		aString , <name> = <value>
	for each public static final field which is of one of the primitive types or java.Lang.String.
	Answers the dictionary"

	| fields |

	fields := self fields select:
			[:each | each isFinal and: [| type | type := each type. type isPrimitive or: [type isJavaLangString]]].

	fields do:
		[:each || name value |
		name := each name.
		value := each getValueFrom: self.
		aDictionary at: (aString , name) put: value].

	^ aDictionary.

! !
!JavaStatic categoriesFor: #conversionToJavaString!constants!private! !
!JavaStatic categoriesFor: #conversionToSmalltalkString!constants!private! !
!JavaStatic categoriesFor: #genericTypeName!constants!generating!public! !
!JavaStatic categoriesFor: #isJavaLangString!public!testing! !
!JavaStatic categoriesFor: #keywordBase!generating!private! !
!JavaStatic categoriesFor: #keywordName:!generating!public! !
!JavaStatic categoriesFor: #makeDictionary:!generating!public! !
!JavaStatic categoriesFor: #makePoolConstantsDictionary:!generating!public! !
!JavaStatic categoriesFor: #makePoolDictionary:!generating!public! !
!JavaStatic categoriesFor: #parameterBase!generating!private! !
!JavaStatic categoriesFor: #parameterName:!generating!public! !
!JavaStatic categoriesFor: #populateDictionary:keyBase:!generating!public! !

!JavaStatic class methodsFor!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	#subclassResponsibility.
	^ 'Object'.
! !
!JavaStatic class categoriesFor: #genericTypeName!constants!generating!public! !

!JVMSettings methodsFor!

wrapperGeneratorSettings
	"answer the subcollection of settings for the wrapper class generation"

	^ self subSettings: #wrapperGeneratorSettings.! !
!JVMSettings categoriesFor: #wrapperGeneratorSettings!accessing!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #JavaWrapperConstants put: (PoolConstantsDictionary named: #JavaWrapperConstants)!
JavaWrapperConstants at: 'GENERATE_SNI_CONSTRUCTORS_MASK' put: 16r4000!
JavaWrapperConstants at: 'WRAP_ABSTRACT_METHODS_MASK' put: 16r1000!
JavaWrapperConstants at: 'WRAP_ALL_MEMBERS_MASK' put: 16rFFF!
JavaWrapperConstants at: 'WRAP_ALL_SUPERCLASSES_MASK' put: 16r2000!
JavaWrapperConstants at: 'WRAP_ANY_CONSTRUCTORS_MASK' put: 16rF00!
JavaWrapperConstants at: 'WRAP_ANY_DEFAULT_ACCESS_MASK' put: 16r222!
JavaWrapperConstants at: 'WRAP_ANY_DEFAULT_MASK' put: 16r222!
JavaWrapperConstants at: 'WRAP_ANY_FIELDS_MASK' put: 16rF0!
JavaWrapperConstants at: 'WRAP_ANY_METHODS_MASK' put: 16rF!
JavaWrapperConstants at: 'WRAP_ANY_PRIVATE_MASK' put: 16r888!
JavaWrapperConstants at: 'WRAP_ANY_PROTECTED_MASK' put: 16r444!
JavaWrapperConstants at: 'WRAP_ANY_PUBLIC_MASK' put: 16r111!
JavaWrapperConstants at: 'WRAP_BRIDGE_METHODS_MASK' put: 16r8000!
JavaWrapperConstants at: 'WRAP_DEFAULT_ACCESS_CONSTRUCTORS_MASK' put: 16r200!
JavaWrapperConstants at: 'WRAP_DEFAULT_ACCESS_FIELDS_MASK' put: 16r20!
JavaWrapperConstants at: 'WRAP_DEFAULT_ACCESS_METHODS_MASK' put: 16r2!
JavaWrapperConstants at: 'WRAP_DEFAULT_CONSTRUCTORS_MASK' put: 16r200!
JavaWrapperConstants at: 'WRAP_DEFAULT_FIELDS_MASK' put: 16r20!
JavaWrapperConstants at: 'WRAP_DEFAULT_METHODS_MASK' put: 16r2!
JavaWrapperConstants at: 'WRAP_NONPRIVATE_MEMBERS_MASK' put: 16r777!
JavaWrapperConstants at: 'WRAP_PRIVATE_CONSTRUCTORS_MASK' put: 16r800!
JavaWrapperConstants at: 'WRAP_PRIVATE_FIELDS_MASK' put: 16r80!
JavaWrapperConstants at: 'WRAP_PRIVATE_METHODS_MASK' put: 16r8!
JavaWrapperConstants at: 'WRAP_PROTECTED_CONSTRUCTORS_MASK' put: 16r400!
JavaWrapperConstants at: 'WRAP_PROTECTED_FIELDS_MASK' put: 16r40!
JavaWrapperConstants at: 'WRAP_PROTECTED_METHODS_MASK' put: 16r4!
JavaWrapperConstants at: 'WRAP_PUBLIC_CONSTRUCTORS_MASK' put: 16r100!
JavaWrapperConstants at: 'WRAP_PUBLIC_FIELDS_MASK' put: 16r10!
JavaWrapperConstants at: 'WRAP_PUBLIC_MEMBERS_MASK' put: 16r111!
JavaWrapperConstants at: 'WRAP_PUBLIC_METHODS_MASK' put: 16r1!
JavaWrapperConstants shrink!

"Classes"!

JavaClassWrapperGenerator guid: (GUID fromString: '{1EEF027A-F044-4299-BAEF-74B1E2C86FE6}')!
JavaClassWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaClassWrapperGenerator categoriesForClass!Unclassified! !
!JavaClassWrapperGenerator methodsFor!

addListingMethodGeneratorsTo: aCollection
	"private -- add listing-method generators to the given collection of member generators"

	aCollection add: (JavaListingMethodGenerator
				selector: #generatedConstructorSelectors
				entries: (aCollection select: [:each | each isConstructor] thenCollect: [:each | each selector])
				targetClass: targetClass class).

	aCollection add: (JavaListingMethodGenerator
				selector: #generatedGetterSelectors
				entries: (aCollection select: [:each | each isGetter] thenCollect: [:each | each selector])
				targetClass: targetClass class).

	aCollection add: (JavaListingMethodGenerator
				selector: #generatedSetterSelectors
				entries: (aCollection select: [:each | each isSetter] thenCollect: [:each | each selector])
				targetClass: targetClass class).

	aCollection add: (JavaListingMethodGenerator
				selector: #generatedWrapperSelectors
				entries: (aCollection select: [:each | each isWrapper] thenCollect: [:each | each selector])
				targetClass: targetClass class).
!

addSNIConstructorGeneratorsTo: aCollection
	"private -- add dummy constructors for generated constuctors that we inherit but which
	our Java class does not actually define"

	| inherited defined dummies |

	inherited := targetClass inheritedConstructorSelectors.
	defined := aCollection select: [:each | each isConstructor] thenCollect: [:each | each selector].

	dummies := (inherited - defined) collect: [:each | JavaSNIConstructorGenerator
								selector: each
								targetClass: targetClass].

	aCollection addAll: dummies.
!

addStandardMethodGeneratorsTo: aCollection
	"private -- add method generators for the 'standard' method #javaClassName
	and, possibly, #jvm"

	aCollection add: (JavaClassNameMethodGenerator
				className: classStatic name
				targetClass: targetClass class).

	installer addExtraMethodGeneratorsTo: aCollection.!

classStatic
	"answer the class static we are working from"

	^ classStatic.!

classStatic: aJavaStatic
	"private -- set the Java class static we will use"

	classStatic := aJavaStatic.
!

constructors
	"answer a Collection of JavaLangReflectConstructor objects for which we should generate wrappers"

	self subclassResponsibility.
!

getters
	"answer a Collection of JavaLangReflectField objects for which we should generate getter wrappers"

	self subclassResponsibility.
!

installer: aJavaClassWrapperInstaller
	"private -- set the JavaClassWrapperInstaller that will drive us"

	installer := aJavaClassWrapperInstaller.
!

memberMethodGenerators
	"private -- answer a Collection of wrapper method generators for every member that we are targetting"

	| constructors wrappers getters setters |

	constructors := self constructors.
	wrappers := self wrappers.
	getters := self getters.

	"on the whole, it's better to remove fields that are hidden"
	getters := self removeHiddenFieldsFrom: getters.

	"NB: do this *after* filtering since otherwise we could have getter and setter referring to different fields!!"
	setters := self settersFromGetters: getters.

	^ (OrderedCollection new)
		addAll: (constructors collect: [:each | installer constructorGeneratorFor: each]);
		addAll: (wrappers collect: [:each | installer methodGeneratorFor: each]);
		addAll: (getters collect: [:each | installer getterGeneratorFor: each]);
		addAll: (setters collect: [:each | installer setterGeneratorFor: each]);
		yourself.!

methodGenerators
	"answer a Collection of wrapper method generators for every member that we are targetting plus
	the 'standard' and 'listing' methods"

	| answer settings |

	settings := installer settings.

	"start with the real methods"
	answer := self memberMethodGenerators.

	"filter out the unwanted stuff"
	answer := settings selectGeneratorsFrom: answer.
	installer methodFilter ifNotNil: [:filter | answer := answer select: filter].

	"avoid inheriting constructors that the target class doesn't actually have"
	settings generateSNIConstructors ifTrue:
		[self addSNIConstructorGeneratorsTo: answer].

	"remove unnecessary overriding methods"
	self removeOverridesFrom: answer.

	"force long-form selectors where necessary to (try to) ensure we have no name clashes"
	self resolveNameClashesIn: answer.

	"add in the various housekeeping methods"
	self addListingMethodGeneratorsTo: answer.
	self addStandardMethodGeneratorsTo: answer.

	^ answer.!

printOn: aStream
	"append a developer-centric description of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' for: ';
		print: classStatic.!

removeHiddenFieldsFrom: aCollectionOfFields
	"private -- given the collection of JavaLangReflectField objects, answer a copy without any
	that are hidden by others"

	^ aCollectionOfFields reject: [:each | aCollectionOfFields anySatisfy: [:other | other ~~ each and: [other hides: each]]].!

removeOverridesFrom: aCollection
	"private -- remove any unecessary generators (ones that override an already OK definition) from aCollection"

	"this is tricky to do properly, and formally unecessary, so -- for now -- we bottle out"
	#CUtodo.

	"we can probably do it reliably for ghost classes, though.  If we do then we should
	also make ghost classes wrap abstract methods unconditionally, since that'd reduce
	the amount of generated code"
	#CUtodo.!

resolveNameClashesBetween: aCollection
	"private -- given a collection of generators that all generate the same selector, try to force them
	to use longer forms to avoid ambiguity"

	| names |

	"first pass, make them use long typenames in the selector keywords"
	aCollection do: [:each | each generateLongKeywords: true].

	"second pass, make any that remain ambiguous include the type as well"
	names := Bag withAll: (aCollection collect: [:each | each selector]).
	(aCollection select: [:each | (names occurrencesOf: each selector) > 1])
		do: [:each | each includeTypeInSelector: true].

	"of course, it's still not *guaranteed* that there's no ambiguity left, but that's the best we can do"!

resolveNameClashesIn: aCollection
	"private -- attempt to ensure that aCollection contains no two generators that will produce methods with the
	same name.
	NB: updates the collection in place"

	| map dummies |

	#CUtodo.  "this should be dummy-setter-aware"
	#CUtodo. "if we cannot resolve ambiguity then should not generate that wrapper at all"

	map := IdentityDictionary new.
	aCollection do: [:each | (map at: each selector ifAbsentPut: [OrderedCollection new]) add: each].

	dummies := OrderedCollection new.
	map keysAndValuesDo:
		[:selector :generators |
		generators size > 1 ifTrue:
			[self resolveNameClashesBetween: generators.
			dummies add: (JavaAmbiguousSelectorGenerator
						selector: selector
						entries: (generators collect: [:each | each selector])
						targetClass: targetClass)]].

	aCollection addAll: dummies.!

setters
	"answer a Collection of JavaLangReflectField objects for which we should generate setter wrappers"

	^ self settersFromGetters: self getters.!

settersFromGetters: aCollectionOfGetters
	"private -- given the collection of getters, answer a Collection of JavaLangReflectField objects for
	which we should generate setter wrappers"

	"we want the ones that are not final, or for which the target class inherits an implementation of
	the corresponding selector"
	^ aCollectionOfGetters reject: [:each | each isFinal and: [(self targetInheritsSetterFor: each) not]].!

targetClass
	"answer the Smalltalk class we are going to populate"

	^ targetClass.!

targetClass: aClass
	"private -- set the Smalltalk class we are going to populate"

	targetClass := aClass.
!

targetInheritsSetterFor: aJavaField
	"private -- answer whether the taget class inherits an implementation of the
	method that would be generated if we generated a setter for the given field"

	| tmp |

	#CUtodo.  "ugh!!"
	tmp := installer setterGeneratorFor: aJavaField.
	^ targetClass superclass canUnderstand: tmp selector.!

useJniObjectFor: aClass
	"private -- answer whether we should use the jniObject directly by default"

	^ false.!

wrappers
	"answer a Collection of JavaLangReflectMethod objects for which we should generate wrappers"

	self subclassResponsibility.
! !
!JavaClassWrapperGenerator categoriesFor: #addListingMethodGeneratorsTo:!generating!private! !
!JavaClassWrapperGenerator categoriesFor: #addSNIConstructorGeneratorsTo:!generating!private! !
!JavaClassWrapperGenerator categoriesFor: #addStandardMethodGeneratorsTo:!generating!private! !
!JavaClassWrapperGenerator categoriesFor: #classStatic!accessing!public! !
!JavaClassWrapperGenerator categoriesFor: #classStatic:!initializing!private! !
!JavaClassWrapperGenerator categoriesFor: #constructors!accessing!public! !
!JavaClassWrapperGenerator categoriesFor: #getters!accessing!public! !
!JavaClassWrapperGenerator categoriesFor: #installer:!initializing!private! !
!JavaClassWrapperGenerator categoriesFor: #memberMethodGenerators!generating!private! !
!JavaClassWrapperGenerator categoriesFor: #methodGenerators!generating!public! !
!JavaClassWrapperGenerator categoriesFor: #printOn:!printing!public! !
!JavaClassWrapperGenerator categoriesFor: #removeHiddenFieldsFrom:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #removeOverridesFrom:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #resolveNameClashesBetween:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #resolveNameClashesIn:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #setters!accessing!public! !
!JavaClassWrapperGenerator categoriesFor: #settersFromGetters:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #targetClass!accessing!public! !
!JavaClassWrapperGenerator categoriesFor: #targetClass:!initializing!private! !
!JavaClassWrapperGenerator categoriesFor: #targetInheritsSetterFor:!helpers!private! !
!JavaClassWrapperGenerator categoriesFor: #useJniObjectFor:!initializing!private! !
!JavaClassWrapperGenerator categoriesFor: #wrappers!accessing!public! !

!JavaClassWrapperGenerator class methodsFor!

for: aJavaStatic target: aClass installer: aJavaClassWrapperInstaller
	"answer a new instance which will generate code to wrap the Java class represented by aJavaStatic
	in the Smalltalk class, aClass, and which is 'driven' by aJavaClassWrapperInstaller"

	^ (self new)
		classStatic: aJavaStatic;
		targetClass: aClass;
		installer: aJavaClassWrapperInstaller
		yourself.!

new
	"private -- use #for:target:installer:"

	^ (super new)
		initialize;
		yourself.! !
!JavaClassWrapperGenerator class categoriesFor: #for:target:installer:!instance creation!public! !
!JavaClassWrapperGenerator class categoriesFor: #new!instance creation!private! !

JavaClassWrapperInstaller guid: (GUID fromString: '{1E948D8E-0386-4728-B219-02F221F2F840}')!
JavaClassWrapperInstaller comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

This class exist mainly in order to reduce the number of subclasses of JavaClassWrapperGenerator and JavaMethodGenerator, and duplication amongst those subclasses.  The problem being that we would need a ghost subclass of each "leaf" class, and that those subclasses would contain a great deal of duplicated code.  By factoring some of the logic into this class, which is designed to have a subclass that knows how to install ghost classes, we can eliminate a useful amount of that duplication.

However it is also nice to have a split of resposnibility between the classes that generate method sources, and those that actually install the compiled methods, so this class (and its subclasses) aren''t *just* workarounds for the lack of multiple-inheritance (or mixins, or whatever).'!
!JavaClassWrapperInstaller categoriesForClass!Unclassified! !
!JavaClassWrapperInstaller methodsFor!

addExtraMethodGeneratorsTo: aCollection
	"private -- invoked by the class generator as it constructs its list of method generators.
	This gives us the chance to add extra methods before the class generator does name
	reolution, etc.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	"nothing to do"!

autoCategory
	"private -- answer the method category used to mark automatically-generated
	methods"

	^ MethodCategory name: self autoCategoryName.!

autoCategoryName
	"answer the name of the method category used to mark automatically-generated
	methods"

	^ '**auto generated**'.!

canRegisterDestination
	"answer whether it is possibly and fruitful to register our destination
	class"

	| class |

	class := isForClassSide
			ifTrue: [source class]
			ifFalse: [source instanceClass].

	^ destination inheritsFrom: class ghostClassRoot.!

classGenerator
	"answer the JavaClassWrapperGenerator that will actually generate our wrappers"

	classGeneratorCache isNil ifTrue: [classGeneratorCache := self makeClassGenerator].

	^ classGeneratorCache.!

constructorGeneratorFor: aJavaLangConstructor
	"private -- answer a JavaConstructorWrapperGenerator for the given Java constructor
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaConstructorWrapperGenerator for: aJavaLangConstructor targetClass: destination.!

destination
	"answer the Smalltalk class that we will generate wrapper methods in"

	^ destination.!

destination: aClass
	"set the Smalltalk class to which we will add wrapper methods"

	destination := aClass.
	classGeneratorCache := nil.!

getterGeneratorFor: aJavaLangField
	"private -- answer a JavaFieldGetterGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaFieldGetterGenerator for: aJavaLangField targetClass: destination.!

initialize
	"private -- establish a coherent initial state"

	installed := OrderedCollection new.
	problems := OrderedCollection new.!

install
	"generate wrapper methods as specified by our settings and install them in our
	destination class"

	self methodGenerators do:
		[:each |
		[installed add: (self installFromMethodGenerator: each)]
			on: Exception
			do: [:ex | problems add: ex.  ex toTrace]].
!

installed
	"answer an OrderedCollection of the methods generated during installation"

	^ installed.!

installFromMethodGenerator: aJavaMethodGenerator
	"cause aJavaMethodGenerator to install itself, answers the installed method"

	| categories method methodSource |

	(self isDangerousOverwrite: aJavaMethodGenerator) ifTrue:
		[Error
			signal: (aJavaMethodGenerator methodName , ' was not generated because the previous version has been removed from the ''**auto generated**'' category')
			with: aJavaMethodGenerator].

	categories := aJavaMethodGenerator categories.
	methodSource := aJavaMethodGenerator methodSource.

	"ensure the newly generated method will be marked **auto**"
	categories add: self autoCategory.

	method := aJavaMethodGenerator targetClass
			compile: methodSource
			categories: categories.

	method isNil ifTrue:
		[Error
			signal: (aJavaMethodGenerator methodName , ' not generated because it won''t compile')
			with: (aJavaMethodGenerator -> methodSource)].

	^ method.!

isAcceptableOverwrite: aJavaMethodGenerator
	"answer true iff executing aJavaMethodGenerator would cause it to overwrite
	an exisiting method that was marked 'OK to overwrite' by putting it in the
	*generated methods* category"

	| selector prev |

	prev := aJavaMethodGenerator targetClass
			compiledMethodAt: aJavaMethodGenerator selector
			ifAbsent: [^ false].

	^ (self autoCategory includesMethod: prev).
!

isDangerousOverwrite: aJavaMethodGenerator
	"answer true iff executing aJavaMethodGenerator would cause it to overwrite
	an exisiting method that was not marked 'OK to overwrite' by putting it in the
	*generated methods* category"

	| selector prev |

	prev := aJavaMethodGenerator targetClass
			compiledMethodAt: aJavaMethodGenerator selector
			ifAbsent: [^ false].

	^ (self autoCategory includesMethod: prev) not.
!

isForClassSide
	"answer whether we are to generate wrappers for the class-side or instance-side"

	^ isForClassSide.!

isForClassSide: aBool
	"private -- set whether we are to generate wrappers for the class-side or instance-side"

	isForClassSide := aBool.!

isForJava5
	"answer whether we are to generate wrappers for a class under Java 5"

	^ source jvm hasJava5Extensions.!

makeClassGenerator
	"private -- answer a JavaClassWrapperGenerator that has been initialised to
	generate the wrappers we will install"

	| generatorClass |

	generatorClass := isForClassSide
				ifTrue: [JavaStaticWrapperGenerator]
				ifFalse: [JavaInstanceClassWrapperGenerator].

	^ generatorClass for: source target: destination installer: self.!

methodFilter
	"answer our <monadicValuable> method filter, or nil if we don't have one
	set"

	^ methodFilter.!

methodFilter: a1Block
	"set our method filter to the <monadicValuable> a1Block.  If this is non-nil
	then the filter will be applied to each instance of JavaMemberWrapperGenerator
	and only those for which it answers true will actually be generated"

	#CUtodo. "make use of this"

	methodFilter := a1Block.!

methodGeneratorFor: aJavaLangMethod
	"private -- answer a JavaMethodWrapperGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaMethodWrapperGenerator for: aJavaLangMethod targetClass: destination.!

methodGenerators
	"answer a Collection of the methods generators that our settings require us
	to generate"

	^ self classGenerator methodGenerators.!

problems
	"answer an OrderedCollection of the Exceptions signalled during installation"

	^ problems.!

registerDestination
	"ensure that the destination class is registered"

	destination registerAsWrapperWithJVM: source jvm.!

setterGeneratorFor: aJavaLangField
	"private -- answer a JavaFieldSetterGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaFieldSetterGenerator for: aJavaLangField targetClass: destination.!

settings
	"answer the JavaWrapperGeneratorSettings that controls what members we will generate
	wrappers for"

	^ settings.!

settings: aJavaWrapperGeneratorSettings
	"set the JavaWrapperGeneratorSettings that controls what members we will generate
	wrappers for"

	settings := aJavaWrapperGeneratorSettings.
	classGeneratorCache := nil.!

source
	"answer the JavaStatic that we are generating wrapper methods for"

	^ source.!

source: aJavaStatic
	"private -- set the Java class whose members we will generate and install
	wrappers for"

	source := aJavaStatic.! !
!JavaClassWrapperInstaller categoriesFor: #addExtraMethodGeneratorsTo:!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #autoCategory!constants!private! !
!JavaClassWrapperInstaller categoriesFor: #autoCategoryName!constants!public! !
!JavaClassWrapperInstaller categoriesFor: #canRegisterDestination!public!testing! !
!JavaClassWrapperInstaller categoriesFor: #classGenerator!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #constructorGeneratorFor:!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #destination!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #destination:!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #getterGeneratorFor:!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #initialize!initializing!private! !
!JavaClassWrapperInstaller categoriesFor: #install!operations!public! !
!JavaClassWrapperInstaller categoriesFor: #installed!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #installFromMethodGenerator:!operations!private! !
!JavaClassWrapperInstaller categoriesFor: #isAcceptableOverwrite:!public!testing! !
!JavaClassWrapperInstaller categoriesFor: #isDangerousOverwrite:!public!testing! !
!JavaClassWrapperInstaller categoriesFor: #isForClassSide!public!testing! !
!JavaClassWrapperInstaller categoriesFor: #isForClassSide:!initialization!private! !
!JavaClassWrapperInstaller categoriesFor: #isForJava5!public!testing! !
!JavaClassWrapperInstaller categoriesFor: #makeClassGenerator!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #methodFilter!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #methodFilter:!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #methodGeneratorFor:!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #methodGenerators!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #problems!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #registerDestination!operations!public! !
!JavaClassWrapperInstaller categoriesFor: #setterGeneratorFor:!helpers!private! !
!JavaClassWrapperInstaller categoriesFor: #settings!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #settings:!accessing!initialization!public! !
!JavaClassWrapperInstaller categoriesFor: #source!accessing!public! !
!JavaClassWrapperInstaller categoriesFor: #source:!initialization!private! !

!JavaClassWrapperInstaller class methodsFor!

forClassSideOf: aJavaStatic destination: aClass settings: aJVMWrapperGeneratorSettings
	"answer a new instance which will generate methods to wrap the class-side members and
	constructors of the Java class represented by aJavaStatic, and install them in the given Smalltalk
	class under the control of the given settings.
	NB: aClass should normally be a subclass of JavaStatic"

	^ (self new)
		source: aJavaStatic;
		destination: aClass;
		isForClassSide: true;
		settings: aJVMWrapperGeneratorSettings;
		yourself.
!

forInstanceSideOf: aJavaStatic destination: aClass settings: aJVMWrapperGeneratorSettings
	"answer a new instance which will generate mehtods to wrap the instance-side members
	of the Java class represented by aJavaStatic, and install them in the given Smalltalk class
	under the control of the given settings.
	NB: aClass should normally be a subclass of JavaInstance"

	^ (self new)
		source: aJavaStatic;
		destination: aClass;
		isForClassSide: false;
		settings: aJVMWrapperGeneratorSettings;
		yourself.
!

new
	"private -- use #for:[instanceClass:][staticClass:]"

	^ (super new)
		initialize;
		yourself.!

suggestedDestinationForClassSideOf: aJavaStatic
	"answer a Smalltalk class that looks like a good initial bet for generating
	class-side wrappers for the given Java class.  If there is no such class then
	answer the most appropriate superclass"

	^ aJavaStatic class ghostClassRoot.
!

suggestedDestinationForInstanceSideOf: aJavaStatic
	"answer a Smalltalk class that looks like a good initial bet for generating
	instance-side wrappers for the given Java class.  If there is no such class then
	answer the most appropriate superclass"

	^ aJavaStatic instanceClass ghostClassRoot.
! !
!JavaClassWrapperInstaller class categoriesFor: #forClassSideOf:destination:settings:!instance creation!public! !
!JavaClassWrapperInstaller class categoriesFor: #forInstanceSideOf:destination:settings:!instance creation!public! !
!JavaClassWrapperInstaller class categoriesFor: #new!instance creation!private! !
!JavaClassWrapperInstaller class categoriesFor: #suggestedDestinationForClassSideOf:!helpers!public! !
!JavaClassWrapperInstaller class categoriesFor: #suggestedDestinationForInstanceSideOf:!helpers!public! !

JavaMethodGenerator guid: (GUID fromString: '{A29916DA-EEE7-4F63-9869-311FF0BF15E7}')!
JavaMethodGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaMethodGenerator categoriesForClass!Unclassified! !
!JavaMethodGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ OrderedCollection new.
!

generateComment
	"answer whether we will generate a method comment"

	^ generateComment.
!

generateComment: aBool
	"set whether we will generate a method comment"

	generateComment := aBool.
!

isConstructor
	"answer true iff we are a constuctor method generator"

	^ false.!

isGetter
	"answer true iff we are a getter method generator"

	^ false.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	^ false.!

isSetter
	"answer true iff we are a setter method generator"

	^ false.!

isWrapper
	"answer true iff we are a wrapper method generator"

	^ false.!

jniObjectString
	"answer the String which we use for accessing the underlying JNIObject from a method"

	^ useJniObject
		ifTrue: ['jniObject']
		ifFalse: ['self jniObject'].

!

jvm
	"answer the JVM instance for which we are generating methods"

	self subclassResponsibility.!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ 'self jvm'.!

methodName
	"answer the 'Class>>selector' style name of the method we are to generate"

	^ targetClass name , '>>' , self selector.!

methodSource
	"answer the source to a Smalltalk method provides access to the member (which may be
	public or private) we target"

	| answer |

	output := String writeStream.

	self writeMethodPattern.
	generateComment ifTrue: [self writeMethodComment].
	self writeMethodBody.

	answer := output contents.

	output := nil.	"finished with it, nil it to prevent accidents"

	^ answer.!

printOn: aStream
	"append a developer-centric description of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' for: ';
		print: self methodName.!

selector
	"answer the selector of the method we are going to generate"

	self subclassResponsibility.!

targetClass
	"answer the Smalltalk class we are going to populate"

	^ targetClass!

targetClass: aClass
	"private -- set the Smalltalk class we are going to populate"

	targetClass := aClass.
	useJniObject := self useJniObjectFor: aClass.
	generateComment := true.
!

useJniObject
	"answer whether we will generate wrapper methods which refer directly to the receiver's jniObject
	instvar"

	^ useJniObject.
!

useJniObject: aBool
	"set whether we will generate wrapper methods which refer directly to the receiver's jniObject
	instvar, rather than using the #jniObject method"

	useJniObject := aBool.
!

useJniObjectFor: aClass
	"private -- answer whether we should use the jniObject directly by default"

	"if we are generating a subclass of JavaClassInstance, then we can access
	the instance's jniObject field directly"
	^ aClass includesBehavior: JavaClassInstance.!

writeMethodBody
	"private -- write a method body to our output stream"

	self subclassResponsibility.
!

writeMethodComment
	"private -- write a method comment to our output stream"
#subclassResponsibility.
	
	"default is just to leave a blank line"
	output cr.!

writeMethodPattern
	"private -- write a 'method pattern' (header) to our output stream"

	| i |

	self selector argumentCount < 1 ifTrue:
		[output
			nextPutAll: self selector;
			cr.
		^ self].

	i := 0.
	self selector keywords
		do: [:each | output
				nextPutAll: each;
				nextPutAll: ' arg';
				print: (i := i + 1)]
		separatedBy: [output space].

	output cr.! !
!JavaMethodGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaMethodGenerator categoriesFor: #generateComment!accessing!public! !
!JavaMethodGenerator categoriesFor: #generateComment:!accessing!public! !
!JavaMethodGenerator categoriesFor: #isConstructor!public!testing! !
!JavaMethodGenerator categoriesFor: #isGetter!public!testing! !
!JavaMethodGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaMethodGenerator categoriesFor: #isSetter!public!testing! !
!JavaMethodGenerator categoriesFor: #isWrapper!public!testing! !
!JavaMethodGenerator categoriesFor: #jniObjectString!constants!generating!public! !
!JavaMethodGenerator categoriesFor: #jvm!accessing!public! !
!JavaMethodGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaMethodGenerator categoriesFor: #methodName!accessing!public! !
!JavaMethodGenerator categoriesFor: #methodSource!accessing!generating!public! !
!JavaMethodGenerator categoriesFor: #printOn:!printing!public! !
!JavaMethodGenerator categoriesFor: #selector!accessing!public! !
!JavaMethodGenerator categoriesFor: #targetClass!accessing!public! !
!JavaMethodGenerator categoriesFor: #targetClass:!initializing!private! !
!JavaMethodGenerator categoriesFor: #useJniObject!accessing!public! !
!JavaMethodGenerator categoriesFor: #useJniObject:!accessing!public! !
!JavaMethodGenerator categoriesFor: #useJniObjectFor:!initializing!private! !
!JavaMethodGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaMethodGenerator categoriesFor: #writeMethodComment!generating!private! !
!JavaMethodGenerator categoriesFor: #writeMethodPattern!generating!private! !

!JavaMethodGenerator class methodsFor!

new
	"private -- use #targetClass: instead"

	^ (super new)
		initialize;
		yourself.!

targetClass: aClass
	"answer a new instance which will generate code and add it to aClass"

	^ (self new)
		targetClass: aClass;
		yourself.! !
!JavaMethodGenerator class categoriesFor: #new!instance creation!private! !
!JavaMethodGenerator class categoriesFor: #targetClass:!instance creation!public! !

AmbiguousJavaMethodError guid: (GUID fromString: '{7B0A1B87-4B32-4B78-8162-642B9764666B}')!
AmbiguousJavaMethodError comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Thrown from automatically generated methods, if the "normal" (short) form of the method''s selector corresponds to two (or more) Java members.  If that is the case then JNPort should have generated two (or more) other methods with long-form names that are not ambiguous.  The #alternatives list should name them'!
!AmbiguousJavaMethodError categoriesForClass!Unclassified! !
!AmbiguousJavaMethodError methodsFor!

alternatives
	"answer the collection of non-ambiguous method selectors that should be
	preferred to the one (our tag) that caused this error"

	^ alternatives.!

alternatives: aCollection
	"private -- set the collection of non-ambiguous method selectors that should be
	preferred to the one (our tag) that caused this error"

	alternatives := aCollection.! !
!AmbiguousJavaMethodError categoriesFor: #alternatives!accessing!public! !
!AmbiguousJavaMethodError categoriesFor: #alternatives:!accessing!private! !

!AmbiguousJavaMethodError class methodsFor!

signal: aString selector: aSelector alternatives: aCollection
	"Raise a new instance of the receiver, with the given Sting message"

	^ (self new)
		messageText: aString;
		tag: aSelector;
		alternatives: aCollection;
		signal.! !
!AmbiguousJavaMethodError class categoriesFor: #signal:selector:alternatives:!instance creation!public! !

JavaInstanceClassWrapperGenerator guid: (GUID fromString: '{3C2DC6BC-94CF-4D19-BAE3-9528783547D0}')!
JavaInstanceClassWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaInstanceClassWrapperGenerator categoriesForClass!Unclassified! !
!JavaInstanceClassWrapperGenerator methodsFor!

constructors
	"answer a Collection of JavaLangReflectConstructor objects for which we should generate wrappers"

	^ #().!

getters
	"answer a Collection of JavaLangReflectField objects for which we should generate getter wrappers"

	^ installer settings includeAllSuperclasses
				ifTrue: [classStatic allInstanceFields]
				ifFalse: [classStatic instanceFields].!

wrappers
	"answer a Collection of JavaLangReflectMethod objects for which we should generate wrappers"

	| settings methods abstract |

	settings := installer settings.

	"start with just the concrete methods"
	methods := settings includeAllSuperclasses
				ifTrue: [classStatic allInstanceMethods]
				ifFalse: [classStatic instanceMethods].

	settings includeAbstractMethods ifFalse: [^ methods].

	"the rest of this method is diabolically slow, fortunately its only used for
	interactive generation (not ghosts) so that doesn't really matter"

	"we pick up (abstract) methods inherited from directly implemented interfaces,
	even if 'includeAllSuperclasses is false"
	abstract := settings includeAllSuperclasses
			ifTrue: [classStatic allAbstractMethods]
			ifFalse: [classStatic abstractMethodsPlusInterfaceMethods].

	"only add in the ones that are not already defined concretely"
	methods addAll: (abstract reject: [:each | methods anySatisfy: [:other | other overrides: each]]).

	^ methods.! !
!JavaInstanceClassWrapperGenerator categoriesFor: #constructors!accessing!public! !
!JavaInstanceClassWrapperGenerator categoriesFor: #getters!accessing!public! !
!JavaInstanceClassWrapperGenerator categoriesFor: #wrappers!accessing!public! !

JavaStaticWrapperGenerator guid: (GUID fromString: '{3C39CD50-91F7-4488-8881-1A0B2289D2E8}')!
JavaStaticWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaStaticWrapperGenerator categoriesForClass!Unclassified! !
!JavaStaticWrapperGenerator methodsFor!

constructors
	"answer a Collection of JavaLangReflectConstructor objects for which we should generate wrappers"

	"we don't bother with constructors for abstract classes, since they can only every be used
	by subclasses -- which we aren't ever going to be..."

	^ classStatic isAbstract
		ifTrue: [#()]
		ifFalse: [classStatic constructors].!

getters
	"answer a Collection of JavaLangReflectField objects for which we should generate getter wrappers"

	"note that we always pick up static fields inherited from directly implemented interfaces, even if
	'includeAllSuperclasses is false"
	^ installer settings includeAllSuperclasses
				ifTrue: [classStatic allFields]
				ifFalse: [classStatic fieldsPlusInterfaceFields]
!

wrappers
	"answer a Collection of JavaLangReflectMethod objects for which we should generate wrappers"

	^ installer settings includeAllSuperclasses
				ifTrue: [classStatic allMethods]
				ifFalse: [classStatic methods].! !
!JavaStaticWrapperGenerator categoriesFor: #constructors!accessing!public! !
!JavaStaticWrapperGenerator categoriesFor: #getters!accessing!public! !
!JavaStaticWrapperGenerator categoriesFor: #wrappers!accessing!public! !

JavaAmbiguousSelectorGenerator guid: (GUID fromString: '{FBE77E2D-C4C4-4B25-BBD4-7274A9D86C88}')!
JavaAmbiguousSelectorGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaAmbiguousSelectorGenerator categoriesForClass!Unclassified! !
!JavaAmbiguousSelectorGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'Java dummy methods');
		yourself.

!

description
	"answer a short String description of the member we wrap"

	^ 'Generate an error since this selector is ambiguous'.!

entries
	"answer the Collection of entries that our generated method will answer"

	^ entries.!

entries: aCollection
	"private -- set the Collection of entries that our generated method will answer"

	entries := aCollection.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	"arguably, we should instead be responding true to whichever of #isConstructor, #isWrapper, etc"
	^ true.!

selector
	"answer the selector of the method we are going to generate"

	^ selector.!

selector: aSelector
	"private -- set the selector of the method we are going to generate"

	selector := aSelector.!

writeMethodBody
	"private -- write a method body to our output stream"

	output
		tab;
		nextPutAll: '^ self';
		cr; tab: 2;
		nextPutAll: 'ambiguousJavaMethod: #';
		nextPutAll: self selector;
		cr; tab: 2;
		nextPutAll: 'alternatives: #('.

	entries asSortedCollection
		do: [:each | output nextPut: $#; nextPutAll: each]
		separatedBy: [output space].

	output
		nextPutAll: ').';
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'trigger an error because more than one Java method maps to the same selector';
		nextPut: $";
		cr; cr.
! !
!JavaAmbiguousSelectorGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaAmbiguousSelectorGenerator categoriesFor: #description!displaying!public! !
!JavaAmbiguousSelectorGenerator categoriesFor: #entries!accessing!public! !
!JavaAmbiguousSelectorGenerator categoriesFor: #entries:!initializing!private! !
!JavaAmbiguousSelectorGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaAmbiguousSelectorGenerator categoriesFor: #selector!accessing!public! !
!JavaAmbiguousSelectorGenerator categoriesFor: #selector:!initializing!private! !
!JavaAmbiguousSelectorGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaAmbiguousSelectorGenerator categoriesFor: #writeMethodComment!generating!private! !

!JavaAmbiguousSelectorGenerator class methodsFor!

selector: aString entries: aCollection targetClass: aClass
	"answer a new instance which will generate code for a method named by aString
	that will answer aCollection"

	^ (super targetClass: aClass)
		selector: aString;
		entries: aCollection;
		yourself.! !
!JavaAmbiguousSelectorGenerator class categoriesFor: #selector:entries:targetClass:!instance creation!public! !

JavaClassNameMethodGenerator guid: (GUID fromString: '{DC732B38-3776-4A83-BD1B-66EADA2ECEF4}')!
JavaClassNameMethodGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaClassNameMethodGenerator categoriesForClass!Unclassified! !
!JavaClassNameMethodGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'accessing');
		add: (MethodCategory name: 'constants');
		yourself.

!

className
	"answer the Java class name that our generated method will answer"

	^ className.
!

className: aString
	"private -- set the Java class name that our generated method will answer"

	className := aString.!

description
	"answer a short String description of the member we wrap"

	^ 'Answer the name of the wrapped Java class'.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	^ true.!

selector
	"answer the selector of the method we are going to generate"

	^ #javaClassName.!

writeMethodBody
	"private -- write a method body to our output stream"

	output
		tab;
		nextPutAll: '^ #''';
		nextPutAll: className;
		nextPutAll: '''.';
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'answer the Symbol name of the Java class we stand for';
		nextPut: $";
		cr; cr.
! !
!JavaClassNameMethodGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaClassNameMethodGenerator categoriesFor: #className!accessing!public! !
!JavaClassNameMethodGenerator categoriesFor: #className:!initializing!private! !
!JavaClassNameMethodGenerator categoriesFor: #description!displaying!public! !
!JavaClassNameMethodGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaClassNameMethodGenerator categoriesFor: #selector!accessing!public! !
!JavaClassNameMethodGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaClassNameMethodGenerator categoriesFor: #writeMethodComment!generating!private! !

!JavaClassNameMethodGenerator class methodsFor!

className: aString targetClass: aClass
	"answer a new instance which will generate code for a #javaClassName method that will
	answer aString"

	^ (super targetClass: aClass)
		className: aString;
		yourself.! !
!JavaClassNameMethodGenerator class categoriesFor: #className:targetClass:!instance creation!public! !

JavaListingMethodGenerator guid: (GUID fromString: '{1F0451F3-CEDD-4548-8EB6-C6DE51A5D3A5}')!
JavaListingMethodGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaListingMethodGenerator categoriesForClass!Unclassified! !
!JavaListingMethodGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'listing wrapper methods');
		add: (MethodCategory name: 'constants');
		yourself.

!

description
	"answer a short String description of the member we wrap"

	^ 'List the generated methods'.!

entries
	"answer the Collection of entries that our generated method will answer"

	^ entries.!

entries: aCollection
	"private -- set the Collection of entries that our generated method will answer"

	entries := aCollection.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	^ true.!

selector
	"answer the selector of the method we are going to generate"

	^ selector.!

selector: aSelector
	"private -- set the selector of the method we are going to generate"

	selector := aSelector.!

writeMethodBody
	"private -- write a method body to our output stream"

	| selectors |

	"add selectors from the existing implementation, if any"
	selectors := (targetClass includesSelector: selector)
				ifTrue: [(IdentitySet withAll: entries)
						addAll: (targetClass instanceClass perform: selector);
						yourself]
				ifFalse: [entries].

	output
		tab;
		nextPutAll: '^ #(';
		cr.

	selectors asSortedCollection do:
		[:each | output
				tab: 2;
				nextPut: $#;
				nextPutAll: each;
				cr].

	output
		tab;
		nextPutAll: ').';
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'answer an Array of the selectors of automatically generated methods.';
		cr;
		tab;
		nextPutAll: 'Note that this does not include inherited selectors';
		nextPut: $";
		cr; cr.
! !
!JavaListingMethodGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaListingMethodGenerator categoriesFor: #description!displaying!public! !
!JavaListingMethodGenerator categoriesFor: #entries!accessing!public! !
!JavaListingMethodGenerator categoriesFor: #entries:!initializing!private! !
!JavaListingMethodGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaListingMethodGenerator categoriesFor: #selector!accessing!public! !
!JavaListingMethodGenerator categoriesFor: #selector:!initializing!private! !
!JavaListingMethodGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaListingMethodGenerator categoriesFor: #writeMethodComment!generating!private! !

!JavaListingMethodGenerator class methodsFor!

selector: aSelector entries: aCollection targetClass: aClass
	"answer a new instance which will generate code for a method named by aString
	that will answer aCollection"

	^ (super targetClass: aClass)
		selector: aSelector;
		entries: aCollection;
		yourself.! !
!JavaListingMethodGenerator class categoriesFor: #selector:entries:targetClass:!instance creation!public! !

JavaMemberWrapperGenerator guid: (GUID fromString: '{AFE691EC-E8CC-4E2F-BF71-519AF7BC6E50}')!
JavaMemberWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaMemberWrapperGenerator categoriesForClass!Unclassified! !
!JavaMemberWrapperGenerator methodsFor!

accessFlags
	"private -- answer the bit flags, from JavaWrapperConstants, corresponding to the type of this member"

	^ reflection isPublic
		ifTrue: [WRAP_ANY_PUBLIC_MASK]
		ifFalse: [reflection isProtected
			ifTrue: [WRAP_ANY_PROTECTED_MASK]
			ifFalse: [reflection isPrivate
				ifTrue: [WRAP_ANY_PRIVATE_MASK]
				ifFalse: [WRAP_ANY_DEFAULT_ACCESS_MASK]]].!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	| cats |

	cats := super categories.

	reflection isPublic
		ifTrue: [cats add: MethodCategory public]
		ifFalse: [cats add: MethodCategory private].

	reflection isAbstract ifTrue: [cats add: (MethodCategory name: 'Java-abstract')].
	reflection isFinal ifTrue: [cats add: (MethodCategory name: 'Java-final')].
	reflection isNative ifTrue: [cats add: (MethodCategory name: 'Java-native')].
	reflection isPrivate ifTrue: [cats add: (MethodCategory name: 'Java-private')].
	reflection isProtected ifTrue: [cats add: (MethodCategory name: 'Java-protected')].
	reflection isPublic ifTrue: [cats add: (MethodCategory name: 'Java-public')].
	reflection isStatic ifTrue: [cats add: (MethodCategory name: 'Java-static')].
	reflection isStrict ifTrue: [cats add: (MethodCategory name: 'Java-fpstrict')].
	reflection isSynchronized ifTrue: [cats add: (MethodCategory name: 'Java-synchronized')].
	reflection isTransient ifTrue: [cats add: (MethodCategory name: 'Java-transient')].
	reflection isVolatile ifTrue: [cats add: (MethodCategory name: 'Java-volatile')].

	^ cats.!

computeParameters
	"private -- answer a list #(keyword parameterName parameterType) triples for the method"

	self subclassResponsibility.!

computeSelector
	"private -- compute the proper selector to use for the method.  Answers a String not a Symbol"

	| str |

	self isUnary ifTrue: [^ self computeUnarySelector].

	str := String writeStream.
	self parametersDo: [:each | str nextPutAll: each first].

	^ str contents.!

computeUnarySelector
	"private -- compute the proper selector to use for the method.  Answers a String not a Symbol"

	self subclassResponsibility.!

convertToSmalltalkString: aString
	"answer aString, which is an expression decorated with code to
	coerce it into Smalltalk"

	^ self type conversionToSmalltalkString
			formatWith: aString
			with: self wrapperFactoryString.!

description
	"answer a short String description of the member we wrap"

	^ reflection description.!

generateLongKeywords
	"answer whether we generate long keywords (using the full type name, not just it's last segment)"

	^ generateLongKeywords.!

generateLongKeywords: aBool
	"set whether we generate long keywords (using the full type name, not just its last segment)"

	generateLongKeywords = aBool ifTrue: [^ self].

	generateLongKeywords := aBool.

	"discard any cached info that depends on this setting"
	selectorCache := parametersCache := nil.
!

genericTypeName
	"answer one of Boolean, Byte, ... Object, Short, or, Void according to our type"

	genericTypeNameCache isNil ifTrue: [genericTypeNameCache := self type genericTypeName].

	^ genericTypeNameCache.!

includeTypeInSelector
	"answer whether we mention the type of this member in the generated selector"

	^ includeTypeInSelector.!

includeTypeInSelector: aBool
	"set whether we mention the type of this member in the generated selector"

	includeTypeInSelector = aBool ifTrue: [^ self].

	includeTypeInSelector := aBool.

	"discard any cached info that depends on this setting"
	selectorCache := parametersCache := nil.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.
	generateLongKeywords := false.
	includeTypeInSelector := false.
!

isSelectedBy: aGeneratorSettings
	"private -- answer whether we satisfy the criteria defined by aGeneratorSettings"

	| mask |

	"default is just to filter on access flags"
	mask := self accessFlags bitAnd: self typeFlags.	"we want the intersection of the two bitsets"
	^ aGeneratorSettings allFlagsSet: mask.
!

isUnary
	"answer whether the generated method will take no arguments"

	^ self parameterCount = 0.!

jniSignature
	"answer the JNI signature of the member we will generate method to wrap"

	jniSignatureCache isNil ifTrue: [jniSignatureCache := reflection jniSignature].

	^ jniSignatureCache.!

jvm
	"answer the JVM instance for which we are generating methods"

	^ reflection jvm.!

keyword: anInteger
	"answer the name of anInteger-th keyword of the method we will generate.
	NB: assumes we aren't unary"

	^ (self parameters at: anInteger) first.!

name
	"answer the name of the target object (which is also our own name)"

	^ name.!

parameter: anInteger
	"answer the anInteger-th parameter of the method we will generate"

	^ self parameters at: anInteger.!

parameterCount
	"answer how many paramers the generated method will take"

	^ self parameters size.!

parameterName: anInteger
	"answer the name of anInteger-th parameter of the method we will generate"

	^ (self parameter: anInteger) second.!

parameters
	"answer an Array of #(keyword name type) triples for the method that will be generated"

	parametersCache isNil ifTrue: [parametersCache := self computeParameters].

	^ parametersCache.!

parametersDo: a1Block
	"private -- evaluate a1Block for each parameter of our method"

	^ self parameters do: [:each | a1Block value: each].!

parametersDo: a1Block separatedBy: a0Block
	"private -- evaluate a1Block for each parameter of our method"

	^ self parameters
		do: [:each | a1Block value: each]
		separatedBy: a0Block.!

parameterType: anInteger
	"answer the type of anInteger-th parameter of the method we will generate"

	^ (self parameters at: anInteger) third.!

printOn: aStream
	"append a developer-centric description of ourselves to aStream"

	aStream
		basicPrint: self;
		nextPutAll: ' for: ';
		print: reflection.!

reflection: aJavaReflection
	"private -- set the reflection object we will use"

	reflection := aJavaReflection.
	name := aJavaReflection name.
!

selector
	"answer the selector for the generated method"

	selectorCache isNil ifTrue: [selectorCache := self computeSelector asSymbol].

	^ selectorCache.!

type
	"answer the Static for the type of our target"

	^ reflection type.!

typeFlags
	"private -- answer the bit flags, from JavaWrapperConstants, corresponding to the type of this member"

	self subclassResponsibility.!

wrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject"

	"we could answer something like:
		(self jvm findClass: #'...classname...')
	for non-substitutable classes, but I don't think it's worth it"
	^ self jvmString.!

writeExceptionCheck
	"private -- write a raw JNI exception check to our output stream"

	"since this will be generated code, and hardly ever seen (except in the debugger)
	we can drop right to the lowest level to gain speed.  Another small gain is that
	the JVM and JNIEnv will be embedded directly in the generated method's literal
	frame"

	output
		nextPutAll: 'self jniEnv ExceptionCheck == 1 ifTrue:';
		cr; tab: 2;
		nextPutAll: '[| jex |';
		cr; tab: 2;
		nextPutAll: 'jex := self jniEnv ExceptionOccurred.';
		cr; tab: 2;
		nextPutAll: 'self jniEnv ExceptionClear.';
		cr; tab: 2;
		nextPutAll: self jvmString;
		nextPutAll: ' throwJavaException: jex]';
		nextPut: $.;
		cr.
!

writeMethodPattern
	"private -- write a 'method pattern' (header) to our output stream"

	self isUnary ifTrue: [output nextPutAll: self selector; cr. ^ self].

	self
		parametersDo: [:each | output nextPutAll: each first; space; nextPutAll: each second]
		separatedBy: [output space].

	output cr.! !
!JavaMemberWrapperGenerator categoriesFor: #accessFlags!constants!private! !
!JavaMemberWrapperGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaMemberWrapperGenerator categoriesFor: #computeParameters!helpers!private! !
!JavaMemberWrapperGenerator categoriesFor: #computeSelector!helpers!private! !
!JavaMemberWrapperGenerator categoriesFor: #computeUnarySelector!helpers!private! !
!JavaMemberWrapperGenerator categoriesFor: #convertToSmalltalkString:!constants!private! !
!JavaMemberWrapperGenerator categoriesFor: #description!displaying!public! !
!JavaMemberWrapperGenerator categoriesFor: #generateLongKeywords!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #generateLongKeywords:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #genericTypeName!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #includeTypeInSelector!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #includeTypeInSelector:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #initialize!initializing!private! !
!JavaMemberWrapperGenerator categoriesFor: #isSelectedBy:!private!testing! !
!JavaMemberWrapperGenerator categoriesFor: #isUnary!public!testing! !
!JavaMemberWrapperGenerator categoriesFor: #jniSignature!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #jvm!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #keyword:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #name!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #parameter:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #parameterCount!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #parameterName:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #parameters!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #parametersDo:!enumerating!private! !
!JavaMemberWrapperGenerator categoriesFor: #parametersDo:separatedBy:!enumerating!private! !
!JavaMemberWrapperGenerator categoriesFor: #parameterType:!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #printOn:!printing!public! !
!JavaMemberWrapperGenerator categoriesFor: #reflection:!initializing!private! !
!JavaMemberWrapperGenerator categoriesFor: #selector!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #type!accessing!public! !
!JavaMemberWrapperGenerator categoriesFor: #typeFlags!constants!private! !
!JavaMemberWrapperGenerator categoriesFor: #wrapperFactoryString!constants!private! !
!JavaMemberWrapperGenerator categoriesFor: #writeExceptionCheck!generating!private! !
!JavaMemberWrapperGenerator categoriesFor: #writeMethodPattern!generating!private! !

!JavaMemberWrapperGenerator class methodsFor!

for: aJavaReflection targetClass: aClass
	"answer a new instance which will generate code to wrap the given Java
	reflection object (a java.lang.Class or java.lang.reflect.AccessibleObject)
	and add it to aClass"

	^ (super targetClass: aClass)
		reflection: aJavaReflection;
		yourself.! !
!JavaMemberWrapperGenerator class categoriesFor: #for:targetClass:!instance creation!public! !

JavaSNIConstructorGenerator guid: (GUID fromString: '{9F34565F-BEB4-476D-83A9-49D8D37AF6D4}')!
JavaSNIConstructorGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaSNIConstructorGenerator categoriesForClass!Unclassified! !
!JavaSNIConstructorGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'Java-dummy methods');
		yourself.

!

description
	"answer a short String description of the member we wrap"

	^ 'Override incorrectly inherited constructor wrapper to #shouldNotImplement'.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	"arguably, we should instead be responding true to #isConstructor"
	^ true.!

selector
	"answer the selector of the method we are going to generate"

	^ selector.!

selector: aSelector
	"private -- set the selector of the method we are going to generate"

	selector := aSelector.!

writeMethodBody
	"private -- write a method body to our output stream"

	output
		tab;
		nextPutAll: '^ self shouldNotImplement.';
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'overridden because Java constructors are not inherited, and so we must not inherit #';
		nextPutAll: selector;
		crtab;
		nextPutAll: 'from our Smalltalk superclass';
		nextPut: $";
		cr; cr.
! !
!JavaSNIConstructorGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaSNIConstructorGenerator categoriesFor: #description!displaying!public! !
!JavaSNIConstructorGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaSNIConstructorGenerator categoriesFor: #selector!accessing!public! !
!JavaSNIConstructorGenerator categoriesFor: #selector:!initializing!private! !
!JavaSNIConstructorGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaSNIConstructorGenerator categoriesFor: #writeMethodComment!generating!private! !

!JavaSNIConstructorGenerator class methodsFor!

selector: aString targetClass: aClass
	"answer a new instance which will generate code for a 'should not implement' constructor
	method named by aString"

	^ (super targetClass: aClass)
		selector: aString;
		yourself.! !
!JavaSNIConstructorGenerator class categoriesFor: #selector:targetClass:!instance creation!public! !

JavaCodeWrapperGenerator guid: (GUID fromString: '{426694DB-4080-4024-93B5-F1DA8F670A82}')!
JavaCodeWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaCodeWrapperGenerator categoriesForClass!Unclassified! !
!JavaCodeWrapperGenerator methodsFor!

computeParameters
	"private -- answer a list #(keyword parameterName parameterType) triples for the method"

	| bag pfx params answer |

	params := reflection parameterTypes.
	answer := OrderedCollection new: params size.

	bag := Bag new.
	pfx := self expandedName , '_'.
	params do:
		[:each || triple |
		bag add: each.
		triple := Array
				with: (pfx , (each keywordName: generateLongKeywords) , ':')
				with: (each parameterName: (bag occurrencesOf: each))
				with: each.
		answer addLast: triple.
		pfx := ''].

	^ answer.!

computeUnarySelector
	"private -- compute the proper selector to use for the method.  Answers a String not a Symbol"

	^ self expandedName , self noArgsPostfix.!

expandedName
	"private -- answer the base part of the name to use for the method"

	^ includeTypeInSelector
		ifTrue: [self name , '_' , (self type keywordName: generateLongKeywords)]
		ifFalse: [self name].!

noArgsPostfix
	"private -- answer the String to be appended to a method name to signify that it takes no arguments"

	^ '_null'.!

writeParameterAssembly
	"private -- write a the declaration and assembly of a variable holding our parameters"

	self isUnary ifFalse: [self writeParameterAssembly: ''].
!

writeParameterAssembly: aString
	"private -- write a the declaration and assembly of a variable holding our parameters.
	This is complicated by the need to assign temporary java.lang.String objects to variables
	before putting them in the JNIValueArray"

	output
		tab;
		nextPutAll: '| ';
		nextPutAll: aString;
		nextPutAll: (self isUnary ifTrue: [''] ifFalse: ['args ']).
	1 to: self parameterCount do:
		[:i | (self parameterType: i) isJavaLangString ifTrue:
			[output
				nextPutAll: (self parameterName: i);
				nextPutAll: 'Ref ']].
	output
		nextPutAll: '|';
		cr; cr.

	self isUnary ifTrue: [^ self].

	1 to: self parameterCount do:
		[:i | (self parameterType: i) isJavaLangString ifTrue:
			[output
				tab;
				nextPutAll: (self parameterName: i);
				nextPutAll: 'Ref := ';
				nextPutAll: (self parameterName: i);
				nextPutAll: ' asJavaString: ';
				nextPutAll: self jvmString;
				nextPutAll: '.';
				cr]].

	output
		tab;
		nextPutAll: 'args := (JNIValueArray new: ';
		display: self parameterCount;
		nextPut: $).

	1 to: self parameterCount do:
		[:i |
		output
			cr; tab: 3;
			nextPutAll: (self parameterType: i) genericTypeName asLowercase;
			nextPutAll: 'At: ';
			display: i;
			nextPutAll: ' put: ';
			nextPutAll: (self parameterName: i);
			nextPutAll: ((self parameterType: i) isJavaLangString ifTrue: ['Ref;'] ifFalse: [';'])].

	output
		cr; tab: 3;
		nextPutAll: 'yourself.';
		cr; cr.
!

writeParameterDeclarationJavaStyle
	"private -- write a Java-style list of our parameter types to our output stream"

	output nextPut: $(.

	self
		parametersDo: [:each | output nextPutAll: each third name]
		separatedBy: [output nextPutAll: ', '].

	output nextPut: $).! !
!JavaCodeWrapperGenerator categoriesFor: #computeParameters!helpers!private! !
!JavaCodeWrapperGenerator categoriesFor: #computeUnarySelector!helpers!private! !
!JavaCodeWrapperGenerator categoriesFor: #expandedName!helpers!private! !
!JavaCodeWrapperGenerator categoriesFor: #noArgsPostfix!constants!private! !
!JavaCodeWrapperGenerator categoriesFor: #writeParameterAssembly!generating!private! !
!JavaCodeWrapperGenerator categoriesFor: #writeParameterAssembly:!generating!private! !
!JavaCodeWrapperGenerator categoriesFor: #writeParameterDeclarationJavaStyle!generating!private! !

JavaFieldAccessorGenerator guid: (GUID fromString: '{5AF158B6-28EB-4BA2-AC42-7E7296487289}')!
JavaFieldAccessorGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaFieldAccessorGenerator categoriesForClass!Unclassified! !
!JavaFieldAccessorGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'Java-fields');
		yourself.

!

expandedName
	"private -- answer the base part of the name to use for the method"

	^ includeTypeInSelector
		ifTrue: [(self type keywordName: generateLongKeywords) , '_' , self name]
		ifFalse: [self name].!

typeFlags
	"private -- answer the bit flags, from JavaWrapperConstants, corresponding to the type of this member"

	^ WRAP_ANY_FIELDS_MASK.! !
!JavaFieldAccessorGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaFieldAccessorGenerator categoriesFor: #expandedName!helpers!private! !
!JavaFieldAccessorGenerator categoriesFor: #typeFlags!constants!private! !

JavaConstructorWrapperGenerator guid: (GUID fromString: '{9E15A8BC-0227-4093-A0E8-54F56D2A64E5}')!
JavaConstructorWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaConstructorWrapperGenerator categoriesForClass!Unclassified! !
!JavaConstructorWrapperGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'Java-constructors');
		yourself.

!

isConstructor
	"answer true iff we are a constuctor method generator"

	^ true.!

name
	"answer the name of the target object (which is also our own name)"

	^ 'new'.!

typeFlags
	"private -- answer the bit flags, from JavaWrapperConstants, corresponding to the type of this member"

	^ WRAP_ANY_CONSTRUCTORS_MASK.!

writeMethodBody
	"private -- write a method body to our output stream"

	self writeParameterAssembly.

	output
		tab;
		nextPutAll: '^ self callConstructor'.

	self isUnary ifFalse:
		[output
			nextPutAll: 'Signature: ';
			nextPut: $';
			nextPutAll: self jniSignature;
			nextPut: $';
			nextPutAll: ' withArguments: args'].
	output
		nextPut: $.;
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'answer the result of calling the receiver''s ';
		nextPutAll: reflection modifiersString;
		space.
	self isUnary
		ifTrue: [output nextPutAll: 'default']
		ifFalse: [output nextPutAll: 'new'. self writeParameterDeclarationJavaStyle].
	output
		nextPutAll: ' Java constructor';
		nextPut: $";
		cr; cr.
! !
!JavaConstructorWrapperGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaConstructorWrapperGenerator categoriesFor: #isConstructor!public!testing! !
!JavaConstructorWrapperGenerator categoriesFor: #name!accessing!public! !
!JavaConstructorWrapperGenerator categoriesFor: #typeFlags!constants!private! !
!JavaConstructorWrapperGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaConstructorWrapperGenerator categoriesFor: #writeMethodComment!generating!private! !

JavaMethodWrapperGenerator guid: (GUID fromString: '{A38D95CD-A3E7-4625-9D32-4E80FF9A557C}')!
JavaMethodWrapperGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaMethodWrapperGenerator categoriesForClass!Unclassified! !
!JavaMethodWrapperGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'Java-methods');
		yourself.

!

isSelectedBy: aGeneratorSettings
	"private -- answer whether we satisfy the criteria defined by aGeneratorSettings"

	"overriden to check for abstract methods too"
	^ (super isSelectedBy: aGeneratorSettings)
		and: [(reflection isAbstract not or: [aGeneratorSettings includeAbstractMethods])
			and: [reflection isBridge not or: [aGeneratorSettings includeBridgeMethods]]].
!

isVoid
	"answer whether the method we generate a wrapper for returns void"

	^ reflection isVoidReturn.!

isWrapper
	"answer true iff we are a wrapper method generator"

	^ true.!

typeFlags
	"private -- answer the bit flags, from JavaWrapperConstants, corresponding to the type of this member"

	^ WRAP_ANY_METHODS_MASK.!

writeMethodBody
	"private -- write a method body to our output stream"

	self writeParameterAssembly.

	output
		tab;
		nextPutAll: (self isVoid ifTrue: [''] ifFalse: ['^ ']);
		nextPutAll: 'self call';
		nextPutAll: self genericTypeName;
		nextPutAll: 'Method: ';
		nextPut: $';
		nextPutAll: self name;
		nextPut: $'.

	(self type isPrimitive and: [self isUnary]) ifFalse:
		[output
			nextPutAll: ' signature: ';
			nextPut: $';
			nextPutAll: self jniSignature;
			nextPut: $'].

	self isUnary ifFalse:
		[output nextPutAll: ' withArguments: args'].

	output
		nextPut: $.;
		cr.!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: (self isVoid ifTrue: ['invoke'] ifFalse: ['answer the result of calling']);
		nextPutAll: ' the receiver''s ';
		nextPutAll: reflection modifiersString;
		space;
		nextPutAll: self name.
	self writeParameterDeclarationJavaStyle.
	output
		nextPutAll: ' Java method';
		nextPut: $";
		cr; cr.
! !
!JavaMethodWrapperGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaMethodWrapperGenerator categoriesFor: #isSelectedBy:!private!testing! !
!JavaMethodWrapperGenerator categoriesFor: #isVoid!public!testing! !
!JavaMethodWrapperGenerator categoriesFor: #isWrapper!public!testing! !
!JavaMethodWrapperGenerator categoriesFor: #typeFlags!constants!private! !
!JavaMethodWrapperGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaMethodWrapperGenerator categoriesFor: #writeMethodComment!generating!private! !

JavaFieldGetterGenerator guid: (GUID fromString: '{2256A615-1AB7-4D1C-9642-D8C8508059E3}')!
JavaFieldGetterGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaFieldGetterGenerator categoriesForClass!Unclassified! !
!JavaFieldGetterGenerator methodsFor!

computeParameters
	"private -- answer a list #(keyword parameterName parameterType) triples for the method"

	^ #().!

computeUnarySelector
	"private -- compute the proper selector to use for the method.  Answers a String not a Symbol"

	^ self getterPrefix , self expandedName.!

getterPrefix
	"private -- answer the prefix to use for getter methods"

	^ 'get_'.!

isGetter
	"answer true iff we are a getter method generator"

	^ true.!

writeMethodBody
	"private -- write a method body to our output stream"

	| noSignature typeName |

	self type isJavaLangString
		ifTrue:
			[noSignature := true.
			typeName := 'String']
		ifFalse:
			[noSignature := self type isPrimitive.
			typeName := self genericTypeName].

	output
		tab;
		nextPutAll: '^ self get';
		nextPutAll: typeName;
		nextPutAll: 'Field: ';
		nextPut: $';
		nextPutAll: self name;
		nextPut: $'.

	noSignature ifFalse:
		[output
			nextPutAll: ' signature: ';
			nextPut: $';
			nextPutAll: self type jniSignature;
			nextPut: $'].

	output
		nextPut: $.;
		cr.!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'answer the value of the receiver''s ';
		nextPutAll: reflection modifiersString;
		space;
		nextPutAll: self name;
		nextPutAll: ' Java field';
		nextPut: $";
		cr; cr.
! !
!JavaFieldGetterGenerator categoriesFor: #computeParameters!helpers!private! !
!JavaFieldGetterGenerator categoriesFor: #computeUnarySelector!helpers!private! !
!JavaFieldGetterGenerator categoriesFor: #getterPrefix!constants!generating!private! !
!JavaFieldGetterGenerator categoriesFor: #isGetter!public!testing! !
!JavaFieldGetterGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaFieldGetterGenerator categoriesFor: #writeMethodComment!generating!private! !

JavaFieldSetterGenerator guid: (GUID fromString: '{62F9CFDF-3C48-4E2F-80DD-88914CD1BA53}')!
JavaFieldSetterGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaFieldSetterGenerator categoriesForClass!Unclassified! !
!JavaFieldSetterGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	| categories |

	categories := super categories.

	self isDummy ifTrue: [categories add: (MethodCategory name: 'Java-dummy methods')].

	^ categories.!

computeParameters
	"private -- answer a list #(keyword parameterName parameterType) triples for the method"

	| triple |

	triple := Array
		with: (self setterPrefix , self expandedName , ':')
		with: (self type parameterName: 0)
		with: (self type).

	^ Array with: triple.!

isDummy
	"private -- answer whether we will generate dummy, #shouldNotImplement, setter
	method.  This is used whenever we are asked to generate a setter for a final field
	(NB: the class generation will attempt to filter our final fields, so that dummy setters
	will not be generated unless they are needed to hide a superclass implementation)"

	^ reflection isFinal.!

isSetter
	"answer true iff we are a setter method generator"

	^ true.!

parameter
	"answer the name of the single parameter our method will take"

	^ self parameter: 1.!

parameterName
	"answer the name of the single parameter our method will take"

	^ self parameterName: 1.!

setterPrefix
	"private -- answer the prefix to use for setter methods"

	^ 'set_'.!

writeDummyMethodBody
	"private -- write a method body to our output stream that will trigger an error
	becuase the corresponding Java field is final"

	output
		tab;
		nextPutAll: '^ self shouldNotImplement.';
		cr.!

writeDummyMethodComment
	"private -- write a method comment to our output stream explaining that it is a
	dummy method"

	output
		tab;
		nextPut: $";
		nextPutAll: 'trigger a ShouldNotImplement error because the receiver''s ';
		nextPutAll: reflection modifiersString;
		space;
		nextPutAll: self name;
		nextPutAll: ' Java field is final';
		nextPut: $";
		cr; cr.
!

writeMethodBody
	"private -- write a method body to our output stream"

	| noSignature typeName |

	self isDummy ifTrue: [^ self writeDummyMethodBody].

	self type isJavaLangString
		ifTrue:
			[noSignature := true.
			typeName := 'String']
		ifFalse:
			[noSignature := self type isPrimitive.
			typeName := self genericTypeName].

	output
		tab;
		nextPutAll: '^ self set';
		nextPutAll: typeName;
		nextPutAll: 'Field: ';
		nextPut: $';
		nextPutAll: self name;
		nextPut: $'.

	noSignature ifFalse:
		[output
			nextPutAll: ' signature: ';
			nextPut: $';
			nextPutAll: self type jniSignature;
			nextPut: $'].

	output
		nextPutAll: ' to: ';
		nextPutAll: (self parameterName: 1);
		nextPut: $.;
		cr!

writeMethodComment
	"private -- write a method comment to our output stream"

	self isDummy ifTrue: [^ self writeDummyMethodComment].

	output
		tab;
		nextPut: $";
		nextPutAll: 'set the value of the receiver''s ';
		nextPutAll: reflection modifiersString;
		space;
		nextPutAll: self name;
		nextPutAll: ' Java field to ';
		nextPutAll: (self parameterName: 1);
		nextPut: $";
		cr; cr.
! !
!JavaFieldSetterGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaFieldSetterGenerator categoriesFor: #computeParameters!helpers!private! !
!JavaFieldSetterGenerator categoriesFor: #isDummy!private!testing! !
!JavaFieldSetterGenerator categoriesFor: #isSetter!public!testing! !
!JavaFieldSetterGenerator categoriesFor: #parameter!accessing!public! !
!JavaFieldSetterGenerator categoriesFor: #parameterName!accessing!public! !
!JavaFieldSetterGenerator categoriesFor: #setterPrefix!constants!generating!private! !
!JavaFieldSetterGenerator categoriesFor: #writeDummyMethodBody!generating!private! !
!JavaFieldSetterGenerator categoriesFor: #writeDummyMethodComment!generating!private! !
!JavaFieldSetterGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaFieldSetterGenerator categoriesFor: #writeMethodComment!generating!private! !

JavaWrapperGeneratorSettings guid: (GUID fromString: '{93D8ABAF-0FD1-4AF7-B695-01B2D67DA7FB}')!
JavaWrapperGeneratorSettings comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

One of these holds the options for controlling how new wrapper classes are generated.

The system-wide one in the current JVM settings is used to set the used by the wrapper generation wizard.'!
!JavaWrapperGeneratorSettings categoriesForClass!Unclassified! !
!JavaWrapperGeneratorSettings methodsFor!

generateSNIConstructors
	"answer whether we generate #shouldNotImplement wrappers for constructors that are 'inherited'
	in Smalltalk, but would not be in Java (and would therefore actually answer an instance of the
	superclass)"

	^ self allFlagsSet: GENERATE_SNI_CONSTRUCTORS_MASK.!

generateSNIConstructors: aBool
	"set whether we generate #shouldNotImplement wrappers for constructors that are 'inherited'
	in Smalltalk, but would not be in Java (and would therefore actually answer an instance of the
	superclass)"

	self setFlags: GENERATE_SNI_CONSTRUCTORS_MASK to: aBool.
!

includeAbstractMethods
	"answer whether we will generate wrapper methods for all of our target class's abstract methods"

	^ self allFlagsSet: WRAP_ABSTRACT_METHODS_MASK.!

includeAbstractMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's abstract methods"

	self setFlags: WRAP_ABSTRACT_METHODS_MASK to: aBool.!

includeAllSuperclasses
	"answer whether we will generate wrapper methods for our target class's inherited members (by
	default we only do the directly declared ones, plus public static fields picked up from interfaces)"

	^ self allFlagsSet: WRAP_ALL_SUPERCLASSES_MASK.
!

includeAllSuperclasses: aBool
	"set whether we will generate wrapper methods for our target class's inherited members (by
	default we only do the directly declared ones, plus public static fields picked up from interfaces)"

	self setFlags: WRAP_ALL_SUPERCLASSES_MASK to: aBool.!

includeBridgeMethods
	"answer whether we will generate wrapper methods for all of our target class's bridge methods.
	These are methods that the bloody Java compiler generates (in huge numbers) to implement
	the disgusting 'covariant returns' feature.  This is off by default because they provide zero
	value"

	^ self allFlagsSet: WRAP_BRIDGE_METHODS_MASK.!

includeBridgeMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's bridge methods"

	self setFlags: WRAP_BRIDGE_METHODS_MASK to: aBool.!

includeConstructors
	"answer whether we generate wrappers for any constructors"

	^ self anyFlagsSet: WRAP_ANY_CONSTRUCTORS_MASK.!

includeDefaultAccessConstructors
	"answer whether we will generate wrapper methods for all of our target class's 'default access' constructors"

	^ self allFlagsSet: WRAP_DEFAULT_ACCESS_CONSTRUCTORS_MASK.!

includeDefaultAccessConstructors: aBool
	"set whether we will generate wrapper methods for all of our target class's 'default access' constructors"

	self setFlags: WRAP_DEFAULT_ACCESS_CONSTRUCTORS_MASK to: aBool.!

includeDefaultAccessFields
	"answer whether we will generate wrapper methods for all of our target class's 'default access' fields"

	^ self allFlagsSet: WRAP_DEFAULT_ACCESS_FIELDS_MASK.!

includeDefaultAccessFields: aBool
	"set whether we will generate wrapper methods for all of our target class's 'default access' fields"

	self setFlags: WRAP_DEFAULT_ACCESS_FIELDS_MASK to: aBool.!

includeDefaultAccessMembers
	"answer whether we will generate wrapper methods for any of our target class's 'default access' members"

	^ self anyFlagsSet: WRAP_ANY_DEFAULT_MASK.!

includeDefaultAccessMembers: aBool
	"set whether we will generate wrapper methods for all of our target class's 'default access' members
	(i.e. it turns contructor, methods, and fields all on/off together)"

	self setFlags: WRAP_ANY_DEFAULT_MASK to: aBool.!

includeDefaultAccessMethods
	"answer whether we will generate wrapper methods for all of our target class's 'default access' methods"

	^ self allFlagsSet: WRAP_DEFAULT_ACCESS_METHODS_MASK.!

includeDefaultAccessMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's 'default access' methods"

	self setFlags: WRAP_DEFAULT_ACCESS_METHODS_MASK to: aBool.!

includeFields
	"answer whether we generate wrappers for any fields"

	^ self anyFlagsSet: WRAP_ANY_FIELDS_MASK.!

includeMethods
	"answer whether we generate wrappers for any methods"

	^ self anyFlagsSet: WRAP_ANY_METHODS_MASK.!

includeNonPrivateMembers
	"answer whether we will generate wrapper methods for any of our target class's non-private members"

	^ self anyFlagsSet: WRAP_NONPRIVATE_MEMBERS_MASK.!

includeNonPrivateMembers: aBool
	"set whether we will generate wrapper methods for all of our target class's non-private members
	(i.e. it turns public, private, and 'default access', contructor, methods, and fields all on/off together)"

	self setFlags: WRAP_NONPRIVATE_MEMBERS_MASK to: aBool.!

includePrivateConstructors
	"answer whether we will generate wrapper methods for all of our target class's private constructors"

	^ self allFlagsSet: WRAP_PRIVATE_CONSTRUCTORS_MASK.!

includePrivateConstructors: aBool
	"set whether we will generate wrapper methods for all of our target class's private constructors"

	self setFlags: WRAP_PRIVATE_CONSTRUCTORS_MASK to: aBool.!

includePrivateFields
	"answer whether we will generate wrapper methods for all of our target class's private fields"

	^ self allFlagsSet: WRAP_PRIVATE_FIELDS_MASK.!

includePrivateFields: aBool
	"set whether we will generate wrapper methods for all of our target class's private fields"

	self setFlags: WRAP_PRIVATE_FIELDS_MASK to: aBool.!

includePrivateMembers
	"answer whether we will generate wrapper methods for any of our target class's private members"

	^ self anyFlagsSet: WRAP_ANY_PRIVATE_MASK.!

includePrivateMembers: aBool
	"set whether we will generate wrapper methods for all of our target class's private members
	(i.e. it turns contructor, methods, and fields all on/off together)"

	self setFlags: WRAP_ANY_PRIVATE_MASK to: aBool.!

includePrivateMethods
	"answer whether we will generate wrapper methods for all of our target class's private methods"

	^ self allFlagsSet: WRAP_PRIVATE_METHODS_MASK.!

includePrivateMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's private methods"

	self setFlags: WRAP_PRIVATE_METHODS_MASK to: aBool.!

includeProtectedConstructors
	"answer whether we will generate wrapper methods for all of our target class's protected constructors"

	^ self allFlagsSet: WRAP_PROTECTED_CONSTRUCTORS_MASK.!

includeProtectedConstructors: aBool
	"set whether we will generate wrapper methods for all of our target class's protected constructors"

	self setFlags: WRAP_PROTECTED_CONSTRUCTORS_MASK to: aBool.!

includeProtectedFields
	"answer whether we will generate wrapper methods for all of our target class's protected fields"

	^ self allFlagsSet: WRAP_PROTECTED_FIELDS_MASK.!

includeProtectedFields: aBool
	"set whether we will generate wrapper methods for all of our target class's protected fields"

	self setFlags: WRAP_PROTECTED_FIELDS_MASK to: aBool.!

includeProtectedMembers
	"answer whether we will generate wrapper methods for any of our target class's protected members"

	^ self anyFlagsSet: WRAP_ANY_PROTECTED_MASK.!

includeProtectedMembers: aBool
	"set whether we will generate wrapper methods for all of our target class's protected members
	(i.e. it turns contructor, methods, and fields all on/off together)"

	self setFlags: WRAP_ANY_PROTECTED_MASK to: aBool.!

includeProtectedMethods
	"answer whether we will generate wrapper methods for all of our target class's protected methods"

	^ self allFlagsSet: WRAP_PROTECTED_METHODS_MASK.!

includeProtectedMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's protected methods"

	self setFlags: WRAP_PROTECTED_METHODS_MASK to: aBool.!

includePublicConstructors
	"answer whether we will generate wrapper methods for all of our target class's public constructors"

	^ self allFlagsSet: WRAP_PUBLIC_CONSTRUCTORS_MASK.!

includePublicConstructors: aBool
	"set whether we will generate wrapper methods for all of our target class's public constructors"

	self setFlags: WRAP_PUBLIC_CONSTRUCTORS_MASK to: aBool.!

includePublicFields
	"answer whether we will generate wrapper methods for all of our target class's public fields"

	^ self allFlagsSet: WRAP_PUBLIC_FIELDS_MASK.!

includePublicFields: aBool
	"set whether we will generate wrapper methods for all of our target class's public fields"

	self setFlags: WRAP_PUBLIC_FIELDS_MASK to: aBool.!

includePublicMembers
	"answer whether we will generate wrapper methods for any of our target class's public members"

	^ self anyFlagsSet: WRAP_ANY_PUBLIC_MASK.!

includePublicMembers: aBool
	"set whether we will generate wrapper methods for all of our target class's public members
	(i.e. it turns contructor, methods, and fields all on/off together)"

	self setFlags: WRAP_ANY_PUBLIC_MASK to: aBool.!

includePublicMethods
	"answer whether we will generate wrapper methods for all of our target class's public methods"

	^ self allFlagsSet: WRAP_PUBLIC_METHODS_MASK.!

includePublicMethods: aBool
	"set whether we will generate wrapper methods for all of our target class's public methods"

	self setFlags: WRAP_PUBLIC_METHODS_MASK to: aBool.!

selectGeneratorsFrom: aCollectionOfMemberGenerators
	"given the collection of JavaLangReflect{Field/Method} objects, answer a copy without any
	that don't match our accessibility criteria"

	^ aCollectionOfMemberGenerators select: [:each | each isSelectedBy: self].! !
!JavaWrapperGeneratorSettings categoriesFor: #generateSNIConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #generateSNIConstructors:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeAbstractMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeAbstractMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeAllSuperclasses!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeAllSuperclasses:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeBridgeMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeBridgeMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessConstructors:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessFields!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessFields:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessMembers!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessMembers:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeDefaultAccessMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeFields!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeNonPrivateMembers!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeNonPrivateMembers:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateConstructors:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateFields!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateFields:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateMembers!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateMembers:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePrivateMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedConstructors:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedFields!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedFields:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedMembers!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedMembers:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includeProtectedMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicConstructors!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicConstructors:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicFields!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicFields:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicMembers!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicMembers:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicMethods!accessing!public!testing! !
!JavaWrapperGeneratorSettings categoriesFor: #includePublicMethods:!accessing!public! !
!JavaWrapperGeneratorSettings categoriesFor: #selectGeneratorsFrom:!helpers!operations!public! !

!JavaWrapperGeneratorSettings class methodsFor!

booleanAspectNames
	"private -- answer an Array of the names of boolean aspects of instances"

	^ super booleanAspectNames
		, #( 
			#includeAllSuperclasses
			#includeAbstractMethods
			#includePublicFields
			#includeDefaultAccessFields
			#includeProtectedFields
			#includePrivateFields
			#includePublicMethods
			#includeDefaultAccessMethods
			#includeProtectedMethods
			#includePrivateMethods
			#includePublicConstructors
			#includeDefaultAccessConstructors
			#includeProtectedConstructors
			#includePrivateConstructors
			#generateSNIConstructors
			#includeBridgeMethods
		).

!

defaultFlags
	"answer the collection of flags that are set by default"

	^ WRAP_PUBLIC_MEMBERS_MASK | WRAP_ABSTRACT_METHODS_MASK.!

initialize
	"private -- class-side intialisation.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #wrapperGeneratorSettings.
!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"

	(Smalltalk at: #JavaWrapperConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'WRAP_PUBLIC_METHODS_MASK'						put: 16r0001;
		at: 'WRAP_DEFAULT_ACCESS_METHODS_MASK'		put: 16r0002;
		at: 'WRAP_PROTECTED_METHODS_MASK'				put: 16r0004;
		at: 'WRAP_PRIVATE_METHODS_MASK'					put: 16r0008;
		at: 'WRAP_ANY_METHODS_MASK'						put: 16r000F;

		at: 'WRAP_PUBLIC_FIELDS_MASK'						put: 16r0010;
		at: 'WRAP_DEFAULT_ACCESS_FIELDS_MASK'			put: 16r0020;
		at: 'WRAP_PROTECTED_FIELDS_MASK'					put: 16r0040;
		at: 'WRAP_PRIVATE_FIELDS_MASK'						put: 16r0080;
		at: 'WRAP_ANY_FIELDS_MASK'							put: 16r00F0;

		at: 'WRAP_PUBLIC_CONSTRUCTORS_MASK'			put: 16r0100;
		at: 'WRAP_DEFAULT_ACCESS_CONSTRUCTORS_MASK' put: 16r0200;
		at: 'WRAP_PROTECTED_CONSTRUCTORS_MASK'	put: 16r0400;
		at: 'WRAP_PRIVATE_CONSTRUCTORS_MASK'			put: 16r0800;
		at: 'WRAP_ANY_CONSTRUCTORS_MASK'				put: 16r0F00;

		at: 'WRAP_ANY_PUBLIC_MASK'							put: 16r0111;
		at: 'WRAP_ANY_DEFAULT_ACCESS_MASK'				put: 16r0222;
		at: 'WRAP_ANY_PROTECTED_MASK'						put: 16r0444;
		at: 'WRAP_ANY_PRIVATE_MASK'						put: 16r0888;
		at: 'WRAP_ALL_MEMBERS_MASK'						put: 16r0FFF;
		at: 'WRAP_PUBLIC_MEMBERS_MASK'						put: 16r0111;
		at: 'WRAP_NONPRIVATE_MEMBERS_MASK'			put: 16r0777;

		at: 'WRAP_ABSTRACT_METHODS_MASK'				put: 16r1000;
		at: 'WRAP_ALL_SUPERCLASSES_MASK'					put: 16r2000;
		at: 'GENERATE_SNI_CONSTRUCTORS_MASK'			put: 16r4000;
		at: 'WRAP_BRIDGE_METHODS_MASK'						put: 16r8000;

		shrink.
!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #wrapperGeneratorSettings.
! !
!JavaWrapperGeneratorSettings class categoriesFor: #booleanAspectNames!constants!development!must strip!private! !
!JavaWrapperGeneratorSettings class categoriesFor: #defaultFlags!constants!public! !
!JavaWrapperGeneratorSettings class categoriesFor: #initialize!initializing!private! !
!JavaWrapperGeneratorSettings class categoriesFor: #rebuildPoolConstants!initializing!private! !
!JavaWrapperGeneratorSettings class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

