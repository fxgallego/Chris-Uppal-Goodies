| package |
package := Package name: 'CU Java Ghost Classes'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

"Ghost classes" are dynamically-generated, ephemeral, wrapper classes that provide a reasonably natural way of interacting with Java objects without either having to generate the wrappers manually, or being forced to drop to the lower-level features of the JNIPort Java base.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.10'.


package classNames
	add: #JavaGhostClassGeneratorSettings;
	add: #JavaGhostClassInstaller;
	add: #JavaGhostConstructorGenerator;
	add: #JavaGhostFieldGetterGenerator;
	add: #JavaGhostFieldSetterGenerator;
	add: #JavaGhostJVMMethodGenerator;
	add: #JavaGhostMethodGenerator;
	add: #JVMGhostClassMaker;
	add: #JVMLazyClassStatic;
	add: #JVMLazyGhostClassMaker;
	yourself.

package methodNames
	add: #JavaArrayClassStatic -> #shouldHaveGhostInstanceClass;
	add: #JavaArrayClassStatic -> #shouldHaveGhostStaticClass;
	add: #JavaClassStatic -> #shouldHaveGhostInstanceClass;
	add: #JavaClassStatic -> #shouldHaveGhostStaticClass;
	add: #JavaCodeWrapperGenerator -> #ghostMethodIDString;
	add: #JavaCodeWrapperGenerator -> #literalsMap;
	add: #JavaFieldAccessorGenerator -> #ghostFieldIDString;
	add: #JavaFieldAccessorGenerator -> #literalsMap;
	add: #JavaInterfaceStatic -> #makeGhostInstanceClassInstaller;
	add: #JavaInterfaceStatic -> #shouldHaveGhostInstanceClass;
	add: #JavaInterfaceStatic -> #shouldHaveGhostStaticClass;
	add: #JavaMemberWrapperGenerator -> #ghostWrapperFactoryString;
	add: #JavaMemberWrapperGenerator -> #literalsMap;
	add: #JavaMethodGenerator -> #ghostJVMString;
	add: #JavaMethodGenerator -> #literalsMap;
	add: #JavaPrimitiveStatic -> #shouldHaveGhostInstanceClass;
	add: #JavaPrimitiveStatic -> #shouldHaveGhostStaticClass;
	add: #JavaStatic -> #generateGhostClasses;
	add: #JavaStatic -> #ghostClassSettings;
	add: #JavaStatic -> #hasGhostInstanceClass;
	add: #JavaStatic -> #hasGhostStaticClass;
	add: #JavaStatic -> #hasUnpopulatedGhostInstanceClass;
	add: #JavaStatic -> #hasUnpopulatedGhostStaticClass;
	add: #JavaStatic -> #isUnresolved;
	add: #JavaStatic -> #makeGhostInstanceClass;
	add: #JavaStatic -> #makeGhostInstanceClassInstaller;
	add: #JavaStatic -> #makeGhostStaticClass;
	add: #JavaStatic -> #makeGhostStaticClassInstaller;
	add: #JavaStatic -> #populateGhostClasses;
	add: #JavaStatic -> #shouldHaveGhostInstanceClass;
	add: #JavaStatic -> #shouldHaveGhostStaticClass;
	add: #JavaStatic -> #useGhostClasses;
	add: #JavaStatic -> #useLazyGhostClasses;
	add: #JVMSettings -> #ghostClassSettings;
	add: #JVMSettings -> #usesGhosts;
	add: #JVMSettings -> #usesGhosts:;
	yourself.

package globalNames
	add: #JavaGhostClassConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU Java Wrapper Generation';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

JavaClassWrapperInstaller subclass: #JavaGhostClassInstaller
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodGenerator subclass: #JavaGhostJVMMethodGenerator
	instanceVariableNames: 'jvm'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaConstructorWrapperGenerator subclass: #JavaGhostConstructorGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaMethodWrapperGenerator subclass: #JavaGhostMethodGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaFieldGetterGenerator subclass: #JavaGhostFieldGetterGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaFieldSetterGenerator subclass: #JavaGhostFieldSetterGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaWrapperGeneratorSettings subclass: #JavaGhostClassGeneratorSettings
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JavaGhostClassConstants'
	classInstanceVariableNames: ''!
JVMWatcher subclass: #JVMGhostClassMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMWatcher subclass: #JVMLazyGhostClassMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProtoObject subclass: #JVMLazyClassStatic
	instanceVariableNames: 'subject sharedMutex'
	classVariableNames: 'Why'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaArrayClassStatic methodsFor!

shouldHaveGhostInstanceClass 
	"private -- answer whether we should use a ghost class for our instances"

	^ self ghostClassSettings useGhostInstancesForArrays.
!

shouldHaveGhostStaticClass 
	"private -- answer whether we should use a ghost class static"

	^ self ghostClassSettings useGhostStaticsForArrays.! !
!JavaArrayClassStatic categoriesFor: #shouldHaveGhostInstanceClass!ghost classes!private!testing! !
!JavaArrayClassStatic categoriesFor: #shouldHaveGhostStaticClass!ghost classes!private!testing! !

!JavaClassStatic methodsFor!

shouldHaveGhostInstanceClass 
	"private -- answer whether we should use a ghost class for our instances"

	^ self ghostClassSettings useGhostInstances.
!

shouldHaveGhostStaticClass 
	"private -- answer whether we should use a ghost class static"

	^ self ghostClassSettings useGhostStatics.
! !
!JavaClassStatic categoriesFor: #shouldHaveGhostInstanceClass!ghost classes!private!testing! !
!JavaClassStatic categoriesFor: #shouldHaveGhostStaticClass!ghost classes!private!testing! !

!JavaCodeWrapperGenerator methodsFor!

ghostMethodIDString
	"answer the String which we use for accessing the methodID from a ghost class method"

	"the resulting literal will be replaced in the CompliledMethod's literal frame by a direct reference to
	the appropriate JNIMethodID"
	^ '#MethodID'.


!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	^ (super literalsMap)
		at: #MethodID put: reflection methodID;
		yourself.! !
!JavaCodeWrapperGenerator categoriesFor: #ghostMethodIDString!constants!generating!ghost classes!private! !
!JavaCodeWrapperGenerator categoriesFor: #literalsMap!accessing!public! !

!JavaFieldAccessorGenerator methodsFor!

ghostFieldIDString
	"answer the String which we use for accessing the fieldID from a ghost class method"

	"the resulting literal will be replaced in the CompliledMethod's literal frame by a direct reference to
	the appropriate JNIFieldID"
	^ '#FieldID'.


!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	^ (super literalsMap)
		at: #FieldID put: reflection fieldID;
		yourself.! !
!JavaFieldAccessorGenerator categoriesFor: #ghostFieldIDString!constants!generating!ghost classes!private! !
!JavaFieldAccessorGenerator categoriesFor: #literalsMap!accessing!public! !

!JavaInterfaceStatic methodsFor!

makeGhostInstanceClassInstaller
	"private -- answer a new JavaGhostClassInstaller that will populate our ghost instance class.
	Should be overriden by subclasses that wish to modify the default settings"

	| installer |

	installer := super makeGhostInstanceClassInstaller.

	"there's normally no point in wrapping abstract methods in ghost classes, since the concrete
	subclass will also be wrapped.  However that doesn't apply to interface wrappers"
	installer settings includeAbstractMethods: true.

	^ installer.!

shouldHaveGhostInstanceClass 
	"private -- answer whether we should use a ghost class for our instances"

	^ self ghostClassSettings useGhostInstancesForInterfaces.!

shouldHaveGhostStaticClass 
	"private -- answer whether we should use a ghost class static"

	^ self ghostClassSettings useGhostStaticsForInterfaces.! !
!JavaInterfaceStatic categoriesFor: #makeGhostInstanceClassInstaller!generating!ghost classes!private! !
!JavaInterfaceStatic categoriesFor: #shouldHaveGhostInstanceClass!ghost classes!private!testing! !
!JavaInterfaceStatic categoriesFor: #shouldHaveGhostStaticClass!ghost classes!private!testing! !

!JavaMemberWrapperGenerator methodsFor!

ghostWrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject.
	All ghost generator subclasses will override #wrapperFactoryString to answer this"

	"the resulting literal will be replaced in the CompliledMethod's literal frame by a direct reference to
	the object's JVM or class static"
	^ self type couldBeSubstituted
		ifTrue: [self ghostJVMString]
		ifFalse: ['#WrapperFactory'].

!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	^ (IdentityDictionary new)
		at: #JVM put: self jvm;
		at: #WrapperFactory put: self type;
		yourself.! !
!JavaMemberWrapperGenerator categoriesFor: #ghostWrapperFactoryString!constants!ghost classes!private! !
!JavaMemberWrapperGenerator categoriesFor: #literalsMap!accessing!ghost classes!public! !

!JavaMethodGenerator methodsFor!

ghostJVMString
	"answer the String which we use for accessing the JVM from a ghost class method.
	All ghost generator subclasses will override #jvmString to answer this"

	"the resulting literal will be replaced in the CompliledMethod's literal frame by a direct reference to
	the object's JVM"
	^ '#JVM'.


!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	"we don't use a literals map"
	^ nil.! !
!JavaMethodGenerator categoriesFor: #ghostJVMString!constants!generating!ghost classes!public! !
!JavaMethodGenerator categoriesFor: #literalsMap!accessing!public! !

!JavaPrimitiveStatic methodsFor!

shouldHaveGhostInstanceClass 
	"private -- answer whether we should use a ghost class for our instances"

	^ false.!

shouldHaveGhostStaticClass 
	"private -- answer whether we should use a ghost class static"

	^ false.! !
!JavaPrimitiveStatic categoriesFor: #shouldHaveGhostInstanceClass!ghost classes!private!testing! !
!JavaPrimitiveStatic categoriesFor: #shouldHaveGhostStaticClass!ghost classes!private!testing! !

!JavaStatic methodsFor!

generateGhostClasses
	"private -- construct, but don't populate, ghost class wrappers as allowed by our owning JVM's
	settings"

	"ensure our Java superclass has already generated any ghosts it's going to.  This must come first because
	we need the Java superclass wrapper classes (if any) to be in place before we can subclass them"
	self javaSuperclassDo: [:it | it generateGhostClasses].

	"NB: we are assuming that the following code is idempotent or else this is not threadsafe"

	"generate and record the instance class if we're going to use one"
	(self shouldHaveGhostInstanceClass and: [self hasGhostInstanceClass not])
		ifTrue: [self changeInstanceClassTo: self makeGhostInstanceClass].

	"generate and change class to the class static class if we are going to use one"
	(self shouldHaveGhostStaticClass and: [self hasGhostStaticClass not])
		ifTrue: [self changeStaticClassTo: self makeGhostStaticClass].!

ghostClassSettings
	"answer the subcollection of settings for ghost classes"

	^ self jvmSettings ghostClassSettings.!

hasGhostInstanceClass
	"answer whether we are using a ghost class for instances.
	NB: this is more subtle than just asking if the instance class
	is a ghost class, since we may just be inheriting a ghost instance
	class from our Java superclass without yet having a proper
	ghost instance class dedicated to the Java class that we wrap"

	instanceClass isGhostClass ifFalse: [^ false].
	self javaSuperclassDo: [:it | it instanceClass = instanceClass ifTrue: [^ false]].

	^ true.!

hasGhostStaticClass
	"answer whether we are an instance of a ghost static class.
	NB: this is more subtle than just checking whether we're ghost
	since we may just be inheriting a ghost static class from our Java
	superclass without yet having a proper ghost class dedicated to
	the Java class that we wrap"

	self isGhost ifFalse: [^ false].
	self javaSuperclassDo: [:it | it class = self class ifTrue: [^ false]].

	^ true.!

hasUnpopulatedGhostInstanceClass
	"private answer whether we are using a ghost class for instances, but that
	class is still unpopulated (a transient condition during the construction of
	ghost classes)"

	^ self hasGhostInstanceClass and: [(instanceClass class includesSelector: #javaClassName) not].!

hasUnpopulatedGhostStaticClass
	"private answer whether we are a ghost static, but that our class is still
	unpopulated (a transient condition during the construction of
	ghost classes)"

	^ self hasGhostStaticClass and: [(self class class includesSelector: #javaClassName) not].!

isUnresolved
	"answer whether we are an unresolved proxy for a class static"

	^ false.!

makeGhostInstanceClass
	"private -- answer a new class to use as our ghost instance class"

	^ instanceClass makeGhostClass: self name.!

makeGhostInstanceClassInstaller
	"private -- answer a new JavaGhostClassInstaller that will populate our ghost instance class.
	Should be overriden by subclasses that wish to modify the default settings"

	| settings |

	settings := self ghostClassSettings deepCopy.

	"if the our superclass is not a ghost class, then we'll have to generate
	all the wrappers up to java.lang.Object, since they will not be inherited"
	settings includeAllSuperclasses: (instanceClass superclass isGhostClass not).

	^ JavaGhostClassInstaller
		forInstanceSideOf: self
		destination: instanceClass
		settings: settings.!

makeGhostStaticClass
	"private -- answer a new class to use as our ghost static class"

	^ self class makeGhostClass: (self name , '.static').!

makeGhostStaticClassInstaller
	"private -- answer a new JavaGhostClassInstaller that will populate our own ghost static class.
	Should be overriden by subclasses that wish to modify the default settings"

	| settings |

	settings := self ghostClassSettings deepCopy.

	"if the our superclass is not a ghost class, then we'll have to generate
	all the wrappers up to java.lang.Object, since they will not be inherited"
	settings includeAllSuperclasses: (self class superclass isGhostClass not).

	"make sure we inherit no constructors"
	settings generateSNIConstructors: true.

	^ JavaGhostClassInstaller
		forClassSideOf: self
		destination: self class
		settings: settings.!

populateGhostClasses
	"private -- populate the already generated ghost class wrappers"

	"ensure our Java superclass has already generated any wrapper methods it's going to.  This must come
	first because we need to know which methods we override, etc"
	self javaSuperclassDo: [:it | it populateGhostClasses].

	"finally, actually generate some methods!!"
	self hasUnpopulatedGhostInstanceClass ifTrue:
		[self makeGhostInstanceClassInstaller install].
	self hasUnpopulatedGhostStaticClass ifTrue:
		[self makeGhostStaticClassInstaller install].
!

shouldHaveGhostInstanceClass 
	"private -- answer whether we should use a ghost class for our instances"

	self subclassResponsibility.
!

shouldHaveGhostStaticClass 
	"private -- answer whether we should use a ghost class static"

	self subclassResponsibility.!

useGhostClasses
	"if we aren't already using ghost class wrappers, and can usefully do so, then
	construct them"

	self
		generateGhostClasses;
		populateGhostClasses.!

useLazyGhostClasses
	"if we aren't already using ghost class wrappers, and can usefully do so, then
	construct them, but...
		...lazily"

	"it's easier if we ensure that all the necessary classes are in place, although they
	may be populated lazily -- it's the population that we are trying to postpone"	
	self generateGhostClasses.

	"and turn into a stub that will populate-on-demand"
	JVMLazyClassStatic installLazyStubFor: self.! !
!JavaStatic categoriesFor: #generateGhostClasses!ghost classes!private! !
!JavaStatic categoriesFor: #ghostClassSettings!accessing!ghost classes!public! !
!JavaStatic categoriesFor: #hasGhostInstanceClass!ghost classes!public!testing! !
!JavaStatic categoriesFor: #hasGhostStaticClass!ghost classes!public!testing! !
!JavaStatic categoriesFor: #hasUnpopulatedGhostInstanceClass!ghost classes!public!testing! !
!JavaStatic categoriesFor: #hasUnpopulatedGhostStaticClass!ghost classes!public!testing! !
!JavaStatic categoriesFor: #isUnresolved!ghost classes!public!testing! !
!JavaStatic categoriesFor: #makeGhostInstanceClass!ghost classes!private! !
!JavaStatic categoriesFor: #makeGhostInstanceClassInstaller!ghost classes!private! !
!JavaStatic categoriesFor: #makeGhostStaticClass!ghost classes!private! !
!JavaStatic categoriesFor: #makeGhostStaticClassInstaller!ghost classes!private! !
!JavaStatic categoriesFor: #populateGhostClasses!ghost classes!private! !
!JavaStatic categoriesFor: #shouldHaveGhostInstanceClass!ghost classes!private!testing! !
!JavaStatic categoriesFor: #shouldHaveGhostStaticClass!ghost classes!private!testing! !
!JavaStatic categoriesFor: #useGhostClasses!ghost classes!public! !
!JavaStatic categoriesFor: #useLazyGhostClasses!ghost classes!public! !

!JVMSettings methodsFor!

ghostClassSettings
	"answer the subcollection of settings for the ghost classes"

	^ self subSettings: #ghostClassSettings.!

usesGhosts
	"answer whether these settings are such that a JVM configured from
	them would use ghost classes.
	This intended just as a convenience method"

	^ self jniPortSettings watcherClasses includes: JVMGhostClassMaker.
!

usesGhosts: aBoolean
	"set whether these settings are such that a JVM configured from
	them would use ghost classes.
	This intended just as a convenience method"

	| watchers |

	watchers := self jniPortSettings watcherClasses.

	[watchers includes: JVMGhostClassMaker]  = aBoolean ifTrue: [^ self].

	aBoolean
		ifTrue: [watchers add: JVMGhostClassMaker]
		ifFalse: [watchers remove: JVMGhostClassMaker].
! !
!JVMSettings categoriesFor: #ghostClassSettings!accessing!public! !
!JVMSettings categoriesFor: #usesGhosts!accessing!public! !
!JVMSettings categoriesFor: #usesGhosts:!accessing!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #JavaGhostClassConstants put: (PoolConstantsDictionary named: #JavaGhostClassConstants)!
JavaGhostClassConstants at: 'GHOST_ANY_INSTANCES_MASK' put: 16r150000!
JavaGhostClassConstants at: 'GHOST_ANY_MASK' put: 16r3F0000!
JavaGhostClassConstants at: 'GHOST_ANY_STATICS_MASK' put: 16r2A0000!
JavaGhostClassConstants at: 'GHOST_INSTANCES_FOR_ARRAYS_MASK' put: 16r100000!
JavaGhostClassConstants at: 'GHOST_INSTANCES_FOR_CLASSES_MASK' put: 16r1!
JavaGhostClassConstants at: 'GHOST_INSTANCES_FOR_INTERFACES_MASK' put: 16r40000!
JavaGhostClassConstants at: 'GHOST_INSTANCES_MASK' put: 16r10000!
JavaGhostClassConstants at: 'GHOST_STATICS_FOR_ARRAYS_MASK' put: 16r200000!
JavaGhostClassConstants at: 'GHOST_STATICS_FOR_CLASSES_MASK' put: 16r2!
JavaGhostClassConstants at: 'GHOST_STATICS_FOR_INTERFACES_MASK' put: 16r80000!
JavaGhostClassConstants at: 'GHOST_STATICS_MASK' put: 16r20000!
JavaGhostClassConstants at: 'RETAIN_GHOST_METHOD_SOURCE_MASK' put: 16r1000000!
JavaGhostClassConstants shrink!

"Classes"!

JavaGhostClassInstaller guid: (GUID fromString: '{C9B7E6B6-C69F-4112-BE1A-22E93D729772}')!
JavaGhostClassInstaller comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostClassInstaller categoriesForClass!Unclassified! !
!JavaGhostClassInstaller methodsFor!

addExtraMethodGeneratorsTo: aCollection
	"private -- invoked by the class generator as it constructs its list of method generators.
	This gives us the chance to add extra methods before the class generator does name
	reolution, etc.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	"if the target class doesn't already inherits a ghost implementation of #jvm then
	add one now"
	(destination superclass isGhostClass) ifFalse:
		[aCollection add: (JavaGhostJVMMethodGenerator
					jvm: source jvm
					targetClass: destination)].
!

constructorGeneratorFor: aJavaLangConstructor
	"private -- answer a JavaConstructorWrapperGenerator for the given Java constructor
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaGhostConstructorGenerator for: aJavaLangConstructor targetClass: destination.!

getterGeneratorFor: aJavaLangField
	"private -- answer a JavaFieldGetterGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaGhostFieldGetterGenerator for: aJavaLangField targetClass: destination.!

installFromMethodGenerator: aJavaMethodGenerator
	"cause aJavaMethodGenerator to install itself, answers the installed method or
	throws an Error exception if there is some problem"

	| method methodSource keepSource |

	self assert: [aJavaMethodGenerator targetClass isGhostClass].

	keepSource := self settings retainMethodSource and: [SessionManager current isRuntime not].
	aJavaMethodGenerator generateComment: keepSource.

	methodSource := aJavaMethodGenerator methodSource.
	method := aJavaMethodGenerator targetClass
			compileGhostMethod: methodSource
			map: aJavaMethodGenerator literalsMap.

	method ifNil: [Error
				signal: (aJavaMethodGenerator methodName , ' not generated because it won''t compile')
				with: (aJavaMethodGenerator -> methodSource)].

	keepSource ifTrue: [method sourceDescriptor: methodSource].

	^ method.!

methodGeneratorFor: aJavaLangMethod
	"private -- answer a JavaMethodWrapperGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaGhostMethodGenerator for: aJavaLangMethod targetClass: destination.!

setterGeneratorFor: aJavaLangField
	"private -- answer a JavaFieldSetterGenerator for the given Java method
	that will generate a method to live in our destination class.
	This is factored out from JavaClassWrapperGenerator to eliminate the need
	for duplicate ghost subclasses of Java{InstanceClass/Static}WrapperGenerator"

	^ JavaGhostFieldSetterGenerator for: aJavaLangField targetClass: destination.! !
!JavaGhostClassInstaller categoriesFor: #addExtraMethodGeneratorsTo:!helpers!private! !
!JavaGhostClassInstaller categoriesFor: #constructorGeneratorFor:!helpers!private! !
!JavaGhostClassInstaller categoriesFor: #getterGeneratorFor:!helpers!private! !
!JavaGhostClassInstaller categoriesFor: #installFromMethodGenerator:!operations!private! !
!JavaGhostClassInstaller categoriesFor: #methodGeneratorFor:!helpers!private! !
!JavaGhostClassInstaller categoriesFor: #setterGeneratorFor:!helpers!private! !

JavaGhostJVMMethodGenerator guid: (GUID fromString: '{C6A1427E-EB0A-456F-9168-EF57E488C129}')!
JavaGhostJVMMethodGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostJVMMethodGenerator categoriesForClass!Unclassified! !
!JavaGhostJVMMethodGenerator methodsFor!

categories
	"answer an OrderedCollection of Categories which are appropriate to the
	object we target"

	^ (super categories)
		add: (MethodCategory name: 'accessing');
		yourself.

!

description
	"answer a short String description of the member we wrap"

	^ 'Answer the owning JVM'.!

isHousekeeping
	"answer true iff we are a housekeeping method generator"

	^ true.!

jvm
	"answer the JVM instance for which we are generating methods"

	^ jvm.!

jvm: aJVM
	"private -- set the JVM instance for which we are generating methods"

	jvm := aJVM.!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ self ghostJVMString.!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	^ (IdentityDictionary new)
		at: #JVM put: self jvm;
		yourself.!

selector
	"answer the selector of the method we are going to generate"

	^ #jvm.!

writeMethodBody
	"private -- write a method body to our output stream"

	output
		tab;
		nextPutAll: '^ ';
		nextPutAll: self jvmString;
		nextPut: $.;
		cr.
!

writeMethodComment
	"private -- write a method comment to our output stream"

	output
		tab;
		nextPut: $";
		nextPutAll: 'answer the receiver''s JVM';
		nextPut: $";
		cr; cr.
! !
!JavaGhostJVMMethodGenerator categoriesFor: #categories!accessing!categories!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #description!displaying!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #isHousekeeping!public!testing! !
!JavaGhostJVMMethodGenerator categoriesFor: #jvm!accessing!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #jvm:!initializing!private! !
!JavaGhostJVMMethodGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #literalsMap!accessing!ghost classes!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #selector!accessing!public! !
!JavaGhostJVMMethodGenerator categoriesFor: #writeMethodBody!generating!private! !
!JavaGhostJVMMethodGenerator categoriesFor: #writeMethodComment!generating!private! !

!JavaGhostJVMMethodGenerator class methodsFor!

jvm: aJVM targetClass: aClass
	"answer a new instance which will generate code to answer the given JVM"

	^ (super targetClass: aClass)
		jvm: aJVM;
		yourself.! !
!JavaGhostJVMMethodGenerator class categoriesFor: #jvm:targetClass:!instance creation!public! !

JavaGhostConstructorGenerator guid: (GUID fromString: '{DE9F09CD-68D7-443C-8F72-C9DC6369DAB4}')!
JavaGhostConstructorGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostConstructorGenerator categoriesForClass!Unclassified! !
!JavaGhostConstructorGenerator methodsFor!

jniObjectString
	"answer the String which we use for accessing the underlying JNIObject from a method"

	"overridden so as to hardwire the class object we are talking about, and thus
	prevent mis-inheritance of constructors"
	^ '#JavaClass'.
!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ self ghostJVMString.!

literalsMap
	"answer a Dictionary mapping literals in a generated method to replacement values.
	(Only used for ghost methods)"

	^ (super literalsMap)
		at: #JavaClass put: self type jniObject;
		yourself.!

wrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject"

	"since we always know *exactly* the type of the returned object, we can always use the optimised form"
	^'#WrapperFactory'.!

writeMethodBody
	"private -- write a method body to our output stream"

	"since this will be generated code, and hardly ever seen (except in the debugger),
	we can drop right to the lowest level to gain speed.  However the big gain is that
	the JVM, and especially the JNIMethodID will be embedded directly in the generated
	method's literal frame"

	self writeParameterAssembly: 'answer '.

	output 
		tab;
		nextPutAll: 'answer := self jniEnv';
		cr; tab: 3;
		nextPutAll: 'NewObjectA_class: ';
		nextPutAll: self jniObjectString;
		cr; tab: 3;
		nextPutAll: 'methodID: ';
		nextPutAll: self ghostMethodIDString;
		cr; tab: 3;
		nextPutAll: 'args: ';
		nextPutAll: (self isUnary ifTrue: ['nil'] ifFalse: ['args']);
		nextPut: $.;
		crtab.

	self writeExceptionCheck.

	output
		cr; tab;
		nextPutAll: '^ ';
		nextPutAll: (self convertToSmalltalkString: 'answer');
		nextPut: $.;
		cr.! !
!JavaGhostConstructorGenerator categoriesFor: #jniObjectString!constants!generating!private! !
!JavaGhostConstructorGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaGhostConstructorGenerator categoriesFor: #literalsMap!accessing!public! !
!JavaGhostConstructorGenerator categoriesFor: #wrapperFactoryString!constants!private! !
!JavaGhostConstructorGenerator categoriesFor: #writeMethodBody!generating!private! !

JavaGhostMethodGenerator guid: (GUID fromString: '{A909D58E-1BA5-4F98-9EAF-2735DC5E9E98}')!
JavaGhostMethodGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostMethodGenerator categoriesForClass!Unclassified! !
!JavaGhostMethodGenerator methodsFor!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ self ghostJVMString.!

wrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject"

	^ self ghostWrapperFactoryString.!

writeMethodBody
	"private -- write a method body to our output stream"

	"since this will be generated code, and hardly ever seen (except in the debugger),
	we can drop right to the lowest level to gain speed.  However the big gain is that
	the JVM, and especially the JNIMethodID will be embedded directly in the generated
	method's literal frame"

	| isStatic tabs |

	isStatic := reflection isStatic.

	self isVoid
		ifTrue: [self writeParameterAssembly]
		ifFalse: [self writeParameterAssembly: 'answer '].

	output 	tab.
	tabs := 2.

	self isVoid ifFalse:
		[output nextPutAll: 'answer := '.
		tabs := tabs + 1].

	output
		nextPutAll: 'self jniEnv';
		crtab: tabs;
		nextPutAll: (isStatic ifTrue: ['CallStatic'] ifFalse: ['Call']);
		nextPutAll: self genericTypeName;
		nextPutAll: (isStatic ifTrue: ['MethodA_class: '] ifFalse: ['MethodA_obj: ']);
		nextPutAll: self jniObjectString;
		crtab: tabs;
		nextPutAll: 'methodID: ';
		nextPutAll: self ghostMethodIDString;
		crtab: tabs;
		nextPutAll: 'args: ';
		nextPutAll: (self isUnary ifTrue: ['nil'] ifFalse: ['args']);
		nextPut: $.;
		crtab.

	self writeExceptionCheck.

	self isVoid ifTrue: [^ self].

	output
		cr; tab;
		nextPutAll: '^ ';
		nextPutAll: (self convertToSmalltalkString: 'answer');
		nextPut: $.;
		cr.! !
!JavaGhostMethodGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaGhostMethodGenerator categoriesFor: #wrapperFactoryString!constants!private! !
!JavaGhostMethodGenerator categoriesFor: #writeMethodBody!generating!private! !

JavaGhostFieldGetterGenerator guid: (GUID fromString: '{29148764-9228-4B10-8D57-D08D51B26DB7}')!
JavaGhostFieldGetterGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostFieldGetterGenerator categoriesForClass!Unclassified! !
!JavaGhostFieldGetterGenerator methodsFor!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ self ghostJVMString.!

wrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject"

	^ self ghostWrapperFactoryString.!

writeMethodBody
	"private -- write a method body to our output stream"

	"since this will be generated code, and hardly ever seen (except in the debugger),
	we can drop right to the lowest level to gain speed.  However the big gain is that
	the JVM, and especially the JNIFieldID, can be embedded directly in the generated
	method's literal frame"

	| isStatic |

	isStatic := reflection isStatic.

	output
		tab;
		nextPutAll: '| answer |';
		cr; cr; tab;
		nextPutAll: 'answer := self jniEnv';
		space;
		nextPutAll: (isStatic ifTrue: ['GetStatic'] ifFalse: ['Get']);
		nextPutAll: self genericTypeName;
		nextPutAll: (isStatic ifTrue: ['Field_class: '] ifFalse: ['Field_obj: ']);
		nextPutAll: self jniObjectString;
		nextPutAll: ' fieldID: ';
		nextPutAll: self ghostFieldIDString;
		nextPut: $.;
		crtab.

	self writeExceptionCheck.

	output
		cr; tab;
		nextPutAll: '^ ';
		nextPutAll: (self convertToSmalltalkString: 'answer');
		nextPut: $.;
		cr.! !
!JavaGhostFieldGetterGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaGhostFieldGetterGenerator categoriesFor: #wrapperFactoryString!constants!private! !
!JavaGhostFieldGetterGenerator categoriesFor: #writeMethodBody!generating!private! !

JavaGhostFieldSetterGenerator guid: (GUID fromString: '{19EF87D5-35F7-48FC-A52D-63A552CBF08F}')!
JavaGhostFieldSetterGenerator comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JavaGhostFieldSetterGenerator categoriesForClass!Unclassified! !
!JavaGhostFieldSetterGenerator methodsFor!

jvmString
	"answer the String which we use for accessing the JMV from a method"

	^ self ghostJVMString.!

wrapperFactoryString
	"private -- answer the String which we use for indicating the best object to wrap the returned JNIObject"

	^ self ghostWrapperFactoryString.!

writeMethodBody
	"private -- write a method body to our output stream"

	"since this will be generated code, and hardly ever seen (except in the debugger),
	we can drop right to the lowest level to gain speed.  However the big gain is that
	the JVM. and especially the JNIFieldID, can be embedded directly in the
	generated method's literal frame"

	| isStatic valueString |

	self isDummy ifTrue: [^ self writeDummyMethodBody].

	isStatic := reflection isStatic.
	valueString := (self type conversionToJavaString)
				formatWith: (self parameterName: 1)
				with: self ghostJVMString.

	output
		tab;
		nextPutAll: 'self jniEnv';
		cr; tab: 2;
		nextPutAll: (isStatic ifTrue: ['SetStatic'] ifFalse: ['Set']);
		nextPutAll: self genericTypeName;
		nextPutAll: (isStatic ifTrue: ['Field_class: '] ifFalse: ['Field_obj: ']);
		nextPutAll: self jniObjectString;
		cr; tab: 2;
		nextPutAll: 'fieldID: ';
		nextPutAll: self ghostFieldIDString;
		cr; tab: 2;
		nextPutAll: 'val: ';
		nextPutAll: valueString;
		nextPutAll: ' asParameter.';
		crtab.

	self writeExceptionCheck.! !
!JavaGhostFieldSetterGenerator categoriesFor: #jvmString!constants!generating!public! !
!JavaGhostFieldSetterGenerator categoriesFor: #wrapperFactoryString!constants!private! !
!JavaGhostFieldSetterGenerator categoriesFor: #writeMethodBody!generating!private! !

JavaGhostClassGeneratorSettings guid: (GUID fromString: '{60CDF058-BAAA-4F2C-A502-D7E44B749C69}')!
JavaGhostClassGeneratorSettings comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

One of these holds the options to control how ghost classes are generated.  The system-wide instance in the current JVM-settings is used for every ghost class generated.'!
!JavaGhostClassGeneratorSettings categoriesForClass!Unclassified! !
!JavaGhostClassGeneratorSettings methodsFor!

retainMethodSource
	"answer whether we should keep the source of ghost methods"

	^ self allFlagsSet: RETAIN_GHOST_METHOD_SOURCE_MASK.
!

retainMethodSource: aBool
	"set whether we should keep the source of ghost methods"

	self setFlags: RETAIN_GHOST_METHOD_SOURCE_MASK to: aBool.
!

useGhostInstances
	"answer whether we should generate ghost classes for Java instances"

	^ self anyFlagsSet: GHOST_INSTANCES_MASK.
!

useGhostInstances: aBool
	"set whether we should generate ghost classes for Java instances"

	self setFlags: GHOST_INSTANCES_MASK to: aBool.
!

useGhostInstancesForArrays
	"answer whether we should generate ghost classes for Java arrays"

	^ self anyFlagsSet: GHOST_INSTANCES_FOR_ARRAYS_MASK.
!

useGhostInstancesForArrays: aBool
	"set whether we should generate ghost classes for Java arrays"

	self setFlags: GHOST_INSTANCES_FOR_ARRAYS_MASK to: aBool.
!

useGhostInstancesForInterfaces
	"answer whether we should generate ghost classes for Java interfaces (i.e. object known to
	implement that interface)"

	^ self anyFlagsSet: GHOST_INSTANCES_FOR_INTERFACES_MASK.
!

useGhostInstancesForInterfaces: aBool
	"set whether we should generate ghost classes for Java interfaces (i.e. object known to
	implement that interface)"

	self setFlags: GHOST_INSTANCES_FOR_INTERFACES_MASK to: aBool.
!

useGhosts
	"answer whether the receiver is set to require the use of Ghost classes at all"

	^ self anyFlagsSet: GHOST_ANY_MASK.!

useGhostStatics
	"answer whether we should generate ghost classes for Java classes (i.e. statics)"

	^ self anyFlagsSet: GHOST_STATICS_MASK.
!

useGhostStatics: aBool
	"set whether we should generate ghost classes for Java classes (i.e. statics)"

	self setFlags: GHOST_STATICS_MASK to: aBool.
!

useGhostStaticsForArrays
	"answer whether we should generate ghost classes for Java Array classes (i.e. statics)"

	^ self anyFlagsSet: GHOST_STATICS_FOR_ARRAYS_MASK.!

useGhostStaticsForArrays: aBool
	"set whether we should generate ghost classes for Java Array classes (i.e. statics)"

	self setFlags: GHOST_STATICS_FOR_ARRAYS_MASK to: aBool.

!

useGhostStaticsForInterfaces
	"answer whether we should generate ghost classes for Java interfaces (i.e. statics)"

	^ self anyFlagsSet: GHOST_STATICS_FOR_INTERFACES_MASK.!

useGhostStaticsForInterfaces: aBool
	"set whether we should generate ghost classes for Java interfaces (i.e. statics)"

	self setFlags: GHOST_STATICS_FOR_INTERFACES_MASK to: aBool.

! !
!JavaGhostClassGeneratorSettings categoriesFor: #retainMethodSource!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #retainMethodSource:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstances!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstances:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstancesForArrays!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstancesForArrays:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstancesForInterfaces!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostInstancesForInterfaces:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhosts!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStatics!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStatics:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStaticsForArrays!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStaticsForArrays:!accessing!public! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStaticsForInterfaces!accessing!public!testing! !
!JavaGhostClassGeneratorSettings categoriesFor: #useGhostStaticsForInterfaces:!accessing!public! !

!JavaGhostClassGeneratorSettings class methodsFor!

booleanAspectNames
	"private -- answer an Array of the names of boolean aspects of instances"

	| suppress |

	"we remove some aspects because they are determined by the runtime, rather than depending on the user's choice"
	suppress :=  #( #includeAbstractMethods #includeAllSuperclasses #generateSNIConstructors ).
	^ (super booleanAspectNames copyWithoutAll: suppress)
		, #( 
			#useGhostInstances
			#useGhostStatics
			#useGhostInstancesForArrays
			#useGhostStaticsForArrays
			#useGhostInstancesForInterfaces
			#useGhostStaticsForInterfaces
			#retainMethodSource
		).

!

defaultFlags
	"answer the collection of flags that are set by default"

	^ super defaultFlags
			| GHOST_INSTANCES_MASK
			| GHOST_STATICS_MASK
			| GHOST_STATICS_FOR_INTERFACES_MASK.!

initialize
	"private -- class-side intialisation.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #ghostClassSettings.
!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"

	"NB: flags must not clash with superclass bitpatterns"
	(Smalltalk at: #JavaGhostClassConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'GHOST_INSTANCES_MASK'							put: 16r0010000;
		at: 'GHOST_STATICS_MASK'							put: 16r0020000;

		at: 'GHOST_INSTANCES_FOR_INTERFACES_MASK'	put: 16r0040000;
		at: 'GHOST_STATICS_FOR_INTERFACES_MASK'		put: 16r0080000;

		at: 'GHOST_INSTANCES_FOR_ARRAYS_MASK'		put: 16r0100000;
		at: 'GHOST_STATICS_FOR_ARRAYS_MASK'			put: 16r0200000;

		at: 'GHOST_ANY_INSTANCES_MASK'						put: 16r0150000;
		at: 'GHOST_ANY_STATICS_MASK'						put: 16r02A0000;

		at: 'GHOST_ANY_MASK'								put: 16r03F0000;

		at: 'RETAIN_GHOST_METHOD_SOURCE_MASK'		put: 16r1000000;	"only used in dev. env."

		shrink.
!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #ghostClassSettings.
! !
!JavaGhostClassGeneratorSettings class categoriesFor: #booleanAspectNames!constants!development!must strip!private! !
!JavaGhostClassGeneratorSettings class categoriesFor: #defaultFlags!constants!public! !
!JavaGhostClassGeneratorSettings class categoriesFor: #initialize!initializing!private! !
!JavaGhostClassGeneratorSettings class categoriesFor: #rebuildPoolConstants!ghost classes!initializing!private! !
!JavaGhostClassGeneratorSettings class categoriesFor: #uninitialize!initializing!private! !

JVMGhostClassMaker guid: (GUID fromString: '{959615E1-9A2C-41F4-BAEF-A2DF377CD1F3}')!
JVMGhostClassMaker comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

One of these sits watching the JVM and waiting for new Java classes to be registered.  When they are it converts them to ghosts.'!
!JVMGhostClassMaker categoriesForClass!Unclassified! !
!JVMGhostClassMaker methodsFor!

enghost: aJavaStatic
	"ensure that aJavaStatic has appropriate ghost classes generated for it"

	"you might expect that, since this class is responsible for making ghost classes (hence
	its name!!), we would now create a JavaGhostClass installer, and start using it, rather than
	passing everything back onto the target class.  Indeed I have tried several times to make
	it work that way, but although the *reponsibility* for setting up the ghost class clearly lies
	here, the actual *code* wants to live in JavaStatic.  What's more, it insists on it so strongly
	that I've had to give in to it (not without a fight).  So in the end this class doesn't actually *do*
	very much"

	"NB: for threadsafetly we require that ghost class creation is idempotent"
	aJavaStatic useGhostClasses.
!

enghostAll: aJVM
	"ensure that all currently registered Java classes are ghosts (to the degree
	determined by our settings)"

	aJVM classRegistry allClasses do: [:each | self enghost: each].!

onClassRegistered: aJavaStatic
	"notification recieved when a class has been added to the JVM's
	registry (the owning JVM can be found via the class object).
	Note that this will be called for each wrapper class during JVM
	bootstrap, *before* we recieve notification that the initialisation
	is complete"

	self jvmIsInitialized ifTrue: [self enghost: aJavaStatic].!

onJmvInitialized: aJVM
	"notification recieved when the given JVM has finished initialisation"

	super onJmvInitialized: aJVM.

	self enghostAll: aJVM.!

usesGhostClasses
	"answer whether we use ghost class wrappers"

	^ true.! !
!JVMGhostClassMaker categoriesFor: #enghost:!ghost classes!operations!public! !
!JVMGhostClassMaker categoriesFor: #enghostAll:!operations!public! !
!JVMGhostClassMaker categoriesFor: #onClassRegistered:!event handling!public! !
!JVMGhostClassMaker categoriesFor: #onJmvInitialized:!event handling!public! !
!JVMGhostClassMaker categoriesFor: #usesGhostClasses!public!testing! !

!JVMGhostClassMaker class methodsFor!

onJvmStartup: aJVM
	"this is called whenever a JVM is started with settngs that include this class on
	its list of watcher classes"

	"check that the current system includes enough stuff"
	self assert: [Smalltalk includesKey: #Compiler].
	self assert: [Smalltalk includesKey: #JavaClassWrapperGenerator].

	super onJvmStartup: aJVM.! !
!JVMGhostClassMaker class categoriesFor: #onJvmStartup:!event handling!public! !

JVMLazyGhostClassMaker guid: (GUID fromString: '{384173F7-3E86-4BC7-A852-24737926CADE}')!
JVMLazyGhostClassMaker comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these sits watching the JVM and waiting for new Java classes to be registered.  When they are it converts them to "lazy ghosts".

A lazy ghost is a proxy for a real ghost class that will only go to the effort of generating the actual ghost class methods when it is needed.  The idea is that this reduces significantly the nuimber of classes that are ever loaded or enghosted, thus allowing the system to come up faster and work in a smaller footprint.'!
!JVMLazyGhostClassMaker categoriesForClass!Unclassified! !
!JVMLazyGhostClassMaker methodsFor!

enghost: aJavaStatic
	"ensure that aJavaStatic has appropriate ghost classes generated for it"

	aJavaStatic sharedMutex critical: [aJavaStatic useLazyGhostClasses].
!

enghostAll: aJVM
	"ensure that all currently registered Java classes are ghosts (to the degree
	determined by our settings)"

	aJVM classRegistry allClasses do: [:each | self enghost: each].!

onClassRegistered: aJavaStatic
	"notification recieved when a class has been added to the JVM's
	registry (the owning JVM can be found via the class object).
	Note that this will be called for each wrapper class during JVM
	bootstrap, *before* we recieve notification that the initialisation
	is complete"

	self jvmIsInitialized ifTrue: [self enghost: aJavaStatic].!

onJmvInitialized: aJVM
	"notification recieved when the given JVM has finished initialisation"

	super onJmvInitialized: aJVM.

	self enghostAll: aJVM.!

onJvmShutdown: aJVM
	"notification recieved when the given JVM is about to shutdown.  There's not much that
	can safely be assumed about its state at this point"

	super onJvmShutdown: aJVM.

	"ensure that any lazy classes are taken out of the loop, we
	don't want them to try to resolve themselves now..."
	JVMLazyClassStatic removeLazyStubs.!

usesGhostClasses
	"answer whether we use ghost class wrappers"

	^ true.!

usesLazyGhostClasses
	"answer whether we use lazy ghost class wrappers"

	^ true.! !
!JVMLazyGhostClassMaker categoriesFor: #enghost:!ghost classes!operations!public! !
!JVMLazyGhostClassMaker categoriesFor: #enghostAll:!operations!public! !
!JVMLazyGhostClassMaker categoriesFor: #onClassRegistered:!event handling!public! !
!JVMLazyGhostClassMaker categoriesFor: #onJmvInitialized:!event handling!public! !
!JVMLazyGhostClassMaker categoriesFor: #onJvmShutdown:!event handling!public! !
!JVMLazyGhostClassMaker categoriesFor: #usesGhostClasses!public!testing! !
!JVMLazyGhostClassMaker categoriesFor: #usesLazyGhostClasses!public!testing! !

!JVMLazyGhostClassMaker class methodsFor!

onJvmStartup: aJVM
	"this is called whenever a JVM is started with settngs that include this class on
	its list of watcher classes"

	"check that the current system includes enough stuff"
	self assert: [Smalltalk includesKey: #Compiler].
	self assert: [Smalltalk includesKey: #JavaClassWrapperGenerator].

	super onJvmStartup: aJVM.! !
!JVMLazyGhostClassMaker class categoriesFor: #onJvmStartup:!event handling!public! !

JVMLazyClassStatic guid: (GUID fromString: '{D76F6F19-FF56-4BFE-9943-0C6755380417}')!
JVMLazyClassStatic comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these is a stub that holds a reference to a not-yet-enghosted ClassStatic, and converts it into a ghost on first use.

We implement several methods simply in order to stay lazy.'!
!JVMLazyClassStatic categoriesForClass!Unclassified! !
!JVMLazyClassStatic methodsFor!

__initialize: aJavaClassStatic
	"private -- establish a state in which we are prepared to be #become:-ed into our subject"

	subject := self.	"self and subject will be interchanged just after this method returns so we don't actually record aJavaClassStatic"
	sharedMutex := aJavaClassStatic sharedMutex.!

= aJavaStatic
	"answer whether the receiver is = (in Java terms, this is an identity comparison) to aJavaStatic"

	"implemented in order to avoid premature resolution"

	^ subject = aJavaStatic.!

asParameter
	"implemented in order to avoid premature resolution"

	^ subject asParameter.!

changeInstanceClassTo: aClass

	"if we get here then something is very wrong"
	self assert: [false].!

changeStaticClassTo: aClass

	"if we get here then something is very wrong"
	self assert: [false].!

class
	"implemented in order to avoid premature resolution"

	^ subject class.!

classloader
	"implemented in order to avoid premature resolution"

	^ subject classloader.!

classObject
	"implemented in order to avoid premature resolution"

	^ subject classObject.!

debugPrintString
	"answer the print string to use in a debugger"

	| stream |

	stream := String writeStream.

	stream
		display: self basicClass;
		display: ' for: ';
		display: subject debugPrintString.

	^ stream contents.!

displayOn: aStream
	"implemented in order to avoid premature resolution"

	subject displayOn: aStream
!

displayString
	"implemented in order to avoid premature resolution"

	^ subject displayString.!

doesNotUnderstand: aMessage
	"called by th system when we don't understand some message.  Since we are a
	resolve-on-demand stub, we first resolve, then forward the message to our new self"

	| selector |

	selector := aMessage selector.

	"can we safely pass this message through to our subject without resolving first ?"
	(self basicClass instancesShouldPass: selector) ifTrue:
		[^ subject perform: selector withArguments: aMessage arguments].

	"logging of what is causing us to resolve. Not turned on by default"
	self basicClass recordWhy: selector.

	^ self resolve perform: selector withArguments: aMessage arguments.!

fastResolve
	"called during JVM shutdown to ensure that no lazy stubs are left that might attempt
	to resolve themselves"

	"NB: this depends on several assumptions, as follows.
	We are not finalisable, hence cannot appear on the finalisation queue.  Therefore #primAllInstances
	must also 'see' our subject, which is therefore also not on the finalisation queue.  Hence we will not
	be buggering up the finalisation queue by using #become: here.
	We also assume that #become: is itself thread-safe.
	Another reason for assuming that our subject is not (yet) reclaimable is that this is called early
	in the JVM shutdown sequence, so the class registry has not yet been cleared"

	^ self become: subject.!

generateGhostClasses
	"private -- construct, but don't populate, ghost class wrappers as allowed by our owning JVM's
	settings"

	"since we only ever wrap class statics that have already generated any necessary ghosts,
	we should ignore this"!

hash
	"answer the underlying javaObject's identity hash"

	"implemented in order to avoid premature resolution and also to ensure that
	hashed collections don't need to be re-hashed when we do resolve"

	^ subject hash.
!

inspect
	"we override this because the inherited implementation opens
	a basic inspector that seems to be looking at our subject (because
	it uses #class rather than #basicClass) which is not what we want.
	Ideally we want a basic inpsector that shows /our/ instvars, but
	as a fallback we force a resolve and then open"

	#CUtodo.  "remove this once the basic inpector works right"
	^ self resolve inspect.!

instanceClass
	"implemented in order to avoid premature resolution"

	^ subject instanceClass.!

isDead
	"implemented in order to avoid premature resolution"

	^ subject isDead.!

isKindOf: aClass
	"overridden because the superclass provides the wrong implementation"

	^ subject isKindOf: aClass.!

isUnresolved
	"answer whether we are an unresolved proxy for a class static"

	^ true.!

javaSuperclass
	"implemented in order to avoid premature resolution"

	^ subject javaSuperclass.!

javaSuperclassIfNone: a0Block
	"implemented in order to avoid premature resolution"

	^ subject javaSuperclassIfNone: a0Block.!

jniEnv
	"implemented in order to avoid premature resolution"

	^ subject jniEnv.!

jniObject
	"implemented in order to avoid premature resolution"

	^ subject jniObject.!

jvm
	"implemented in order to avoid premature resolution"

	^ subject jvm.!

managedInstance
	"implemented in order to avoid premature resolution"

	^ subject managedInstance.!

notifyRegistered
	"we implement this so as to prevent it reaching the subject class; we'll resend it
	when we resolve ourself"!

printOn: aStream
	"write a developer oriented representation of the receiver to aStream"

	aStream
		display: self basicClass;
		display: ' for: ';
		print: subject.!

printString
	"Answer a <readableString> whose characters are a description of the receiver 
	as a developer would want to see it."

	| stream |
	stream := String writeStream: 32.
	self printOn: stream.
	^stream contents!

purge
	"implemented so that purging does not cause us to build a
	ghost class as we resolve ourself"

	self
		fastResolve;
		purge.!

resolve
	"if we are some sort of lazy stub (such as are used in lazy ghost classes) then
	resolve and replace ourself with the real thing.
	Answer the resolved reciever (which may not be the original reciever!!)"

	| newMe |

	"slightly tricky logic to prevent two simultaneous #resolves
	resulting in the same class enghosting itself twice"
	sharedMutex critical:
		[newMe := subject ifNil: [^ self].
		subject := nil].

	^ self
		become: newMe;
		populateGhostClasses;
		notifyRegistered;
		yourself.
!

shallowCopy
	"overridden since instances have no properly mutable state, hence a copy would be
	pointless"

	"or actually, to avoid premature resolution"

	^ self.! !
!JVMLazyClassStatic categoriesFor: #__initialize:!accessing!private! !
!JVMLazyClassStatic categoriesFor: #=!comparing!public! !
!JVMLazyClassStatic categoriesFor: #asParameter!converting!public! !
!JVMLazyClassStatic categoriesFor: #changeInstanceClassTo:!mutating!public! !
!JVMLazyClassStatic categoriesFor: #changeStaticClassTo:!mutating!public! !
!JVMLazyClassStatic categoriesFor: #class!accessing!public! !
!JVMLazyClassStatic categoriesFor: #classloader!accessing!public! !
!JVMLazyClassStatic categoriesFor: #classObject!accessing!public! !
!JVMLazyClassStatic categoriesFor: #debugPrintString!development!printing!public! !
!JVMLazyClassStatic categoriesFor: #displayOn:!displaying!public! !
!JVMLazyClassStatic categoriesFor: #displayString!displaying!public! !
!JVMLazyClassStatic categoriesFor: #doesNotUnderstand:!exceptions!public! !
!JVMLazyClassStatic categoriesFor: #fastResolve!mutating!public! !
!JVMLazyClassStatic categoriesFor: #generateGhostClasses!ghost classes!private! !
!JVMLazyClassStatic categoriesFor: #hash!comparing!public! !
!JVMLazyClassStatic categoriesFor: #inspect!development!public! !
!JVMLazyClassStatic categoriesFor: #instanceClass!accessing!public! !
!JVMLazyClassStatic categoriesFor: #isDead!public!testing! !
!JVMLazyClassStatic categoriesFor: #isKindOf:!public!testing! !
!JVMLazyClassStatic categoriesFor: #isUnresolved!public!testing! !
!JVMLazyClassStatic categoriesFor: #javaSuperclass!accessing!Java class hierarchy!public! !
!JVMLazyClassStatic categoriesFor: #javaSuperclassIfNone:!accessing!Java class hierarchy!public! !
!JVMLazyClassStatic categoriesFor: #jniEnv!accessing!public! !
!JVMLazyClassStatic categoriesFor: #jniObject!accessing!public! !
!JVMLazyClassStatic categoriesFor: #jvm!accessing!managed objects!public! !
!JVMLazyClassStatic categoriesFor: #managedInstance!accessing!managed objects!public! !
!JVMLazyClassStatic categoriesFor: #notifyRegistered!initializing!public! !
!JVMLazyClassStatic categoriesFor: #printOn:!printing!public! !
!JVMLazyClassStatic categoriesFor: #printString!printing!public! !
!JVMLazyClassStatic categoriesFor: #purge!public!purging! !
!JVMLazyClassStatic categoriesFor: #resolve!mutating!public! !
!JVMLazyClassStatic categoriesFor: #shallowCopy!copying!public! !

!JVMLazyClassStatic class methodsFor!

installLazyStubFor: aJavaClassStatic
	"create a new instance that uses DNU handling to populate aJavaClassStatic
	on demand, and use #become: to swap it into place"

	| old new |

	old := aJavaClassStatic.
	new := (self basicNew)
			__initialize: aJavaClassStatic;
			yourself.

	old become: new.
		!

instancesShouldPass: aSelector
	"private -- answer whether our instances should pass aSelector directly through to their subjects without
	resolving themselves first"

	"these messages are safe to forward, and are called often during the generation
	of ghost classes that refer to this one, so we don't take them as triggers to resolve
	the stubs.
	It's tempting just to pass through /any/ message that doesn't contain a $_, but then
	we'd have to use #oneWayBecome: when we did resolve since references to the
	subject would certainly leak out.
	NB: this list was constructed by using the #why stuff, rather than by actually thinking...."
	#CUtodo.  "review this stuff, then convert the selectors into forwarding methods"
	^ ##(IdentitySet withAll: #(
		#abstractMethodsPlusInterfaceMethods
		#allFields
		#conversionToSmalltalkString
		#conversionToJavaString
		#couldBeSubstituted
		#genericTypeName
		#icon
		#isArray
		#isDerivedFrom:
		#isInterface
		#isJavaLangString
		#isPrimitive
		#isSubclassOf:
		#isVoid
		#keywordBase
		#keywordName:
		#parameterBase
		#parameterName:
		#name
	)) includes: aSelector.!

recordWhy: aSelector
	"private -- add to our list of selectors that record why an instance was resolved"

	Why ifNotNil: [:it | it at: aSelector put: (it at: aSelector ifAbsent: [0]) + 1].!

removeLazyStubs
	"ensure that no instances are left as lazy stubs"

	self primAllInstances do: [:each | each fastResolve].!

resetWhy: aBool
	"private -- discard/restart our list of selectors that record why an instance was resolved

		self resetWhy: true.
		self resetWhy: false.
	"

	Why := aBool
			ifTrue: [SharedIdentityDictionary new]
			ifFalse: [nil].
!

why
	"private -- answer our list of selectors that record why an instance was resolved"

	^ Why.
! !
!JVMLazyClassStatic class categoriesFor: #installLazyStubFor:!instance creation!public! !
!JVMLazyClassStatic class categoriesFor: #instancesShouldPass:!helpers!private! !
!JVMLazyClassStatic class categoriesFor: #recordWhy:!accessing!private! !
!JVMLazyClassStatic class categoriesFor: #removeLazyStubs!public! !
!JVMLazyClassStatic class categoriesFor: #resetWhy:!accessing!private! !
!JVMLazyClassStatic class categoriesFor: #why!accessing!private! !

"Binary Globals"!

"Resources"!

