| package |
package := Package name: 'CU Java Base'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

The base level of JNIPort.  Adds Smalltalk-flavoured "proxies" that wrap JNI references to Java objects; also "class statics" that provide parallel access to Java classes''s static members as if they were objects too.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.31'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Java Base'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Resources\JavaInstance.ico''
		''Resources\JavaInstance-Ghost.ico''
		''Resources\JavaObject.ico''
		''Resources\JavaStatic.ico''
		''Resources\JavaStatic-Ghost.ico''
		''Resources\JavaStatic-GhostInstanceClass.ico''
		''Resources\JavaStatic-GhostStatic.ico''
		''Resources\JVM.ico''

		''Extras\JNIPort.jar''
		''Extras\JNIPort.zip''
	).
!!'.

package classNames
	add: #JavaAggregate;
	add: #JavaArrayClassStatic;
	add: #JavaBooleanArray;
	add: #JavaByteArray;
	add: #JavaCharArray;
	add: #JavaClassIndex;
	add: #JavaClassInstance;
	add: #JavaClassRegistry;
	add: #JavaClassStatic;
	add: #JavaDoubleArray;
	add: #JavaEqualitySearchPolicy;
	add: #JavaException;
	add: #JavaFloatArray;
	add: #JavaInstance;
	add: #JavaIntArray;
	add: #JavaInterfaceInstance;
	add: #JavaInterfaceStatic;
	add: #JavaLangClass;
	add: #JavaLangClassLoader;
	add: #JavaLangObject;
	add: #JavaLangReflectAccessibleObject;
	add: #JavaLangReflectConstructor;
	add: #JavaLangReflectField;
	add: #JavaLangReflectMethod;
	add: #JavaLangString;
	add: #JavaLangThread;
	add: #JavaLangThrowable;
	add: #JavaLongArray;
	add: #JavaMainClassIndex;
	add: #JavaNetURLClassLoader;
	add: #JavaNonvirtual;
	add: #JavaObject;
	add: #JavaObjectArray;
	add: #JavaObjectRegistry;
	add: #JavaPrimitiveArray;
	add: #JavaPrimitiveBooleanStatic;
	add: #JavaPrimitiveByteStatic;
	add: #JavaPrimitiveCharStatic;
	add: #JavaPrimitiveDoubleStatic;
	add: #JavaPrimitiveFloatStatic;
	add: #JavaPrimitiveInstance;
	add: #JavaPrimitiveIntStatic;
	add: #JavaPrimitiveLongStatic;
	add: #JavaPrimitivesShortStatic;
	add: #JavaPrimitiveStatic;
	add: #JavaPrimitiveVoidStatic;
	add: #JavaRuntimeSettings;
	add: #JavaShortArray;
	add: #JavaStatic;
	add: #JNIPortSettings;
	add: #JVM;
	add: #JVMSettings;
	add: #JVMSubSettings;
	add: #JVMWatcher;
	add: #JVMWithoutCallbacks;
	add: #NoJavaSuperclassException;
	add: #StaticJavaLangClass;
	add: #StaticJavaLangClassLoader;
	add: #StaticJavaLangObject;
	add: #StaticJavaLangReflectModifier;
	add: #StaticJavaLangReflectProxy;
	add: #StaticJavaLangSystem;
	add: #StaticJavaLangThread;
	add: #StaticJavaLangThrowable;
	add: #StaticJavaNetURLClassLoader;
	add: #SupplementaryClassloader;
	add: #SupplementaryClassloadersSettings;
	add: #SupplementaryClassloaderTree;
	yourself.

package methodNames
	add: #BYTEArray -> #allSatisfy:;
	add: #ByteArray -> #asJavaByteArray:;
	add: #ByteArray -> #asJavaCharArray:;
	add: #ByteArray -> #asJavaString:;
	add: #BYTEArray -> #asString;
	add: #BYTEArray -> #readStream;
	add: #Collection -> #asJavaArrayOf:;
	add: #JNIClass -> #asJavaObject:;
	add: #JNIClassG -> #asJavaObject:;
	add: #JNIObject -> #asJavaObject:;
	add: #JNIObjectG -> #asJavaObject:;
	add: #JNIReference -> #asJavaObject:;
	add: #String -> #asJavaByteArray:;
	add: #String -> #asJavaCharArray:;
	add: #String -> #asJavaString:;
	add: #String -> #asJNIClassName;
	add: #String -> #asJNISignature;
	add: #UndefinedObject -> #asJavaByteArray:;
	add: #UndefinedObject -> #asJavaCharArray:;
	add: #UndefinedObject -> #asJavaObject:;
	add: #UndefinedObject -> #asJavaString:;
	add: #UnicodeString -> #asJavaString:;
	yourself.

package globalNames
	add: #JavaBaseConstants;
	add: #JavaReflectionConstants;
	add: #SupplementaryClassloaderConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Binary Arrays';
	add: 'CU Collection Adaptors';
	add: 'CU Ghost Classes';
	add: 'CU Java Quasi-UTF8';
	add: 'CU JNI';
	add: 'CU Package-relative File Locator';
	add: 'CU PluggableWeakSet';
	add: 'CU Varargs';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Registry\Dolphin Registry Access';
	yourself).

package setManualPrerequisites: #(
	'CU Ghost Classes'
	'CU Java Quasi-UTF8'
	'CU Varargs').

package!

"Class Definitions"!

Object subclass: #JavaClassIndex
	instanceVariableNames: 'index sharedMutex classFinder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JavaObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JavaObjectRegistry
	instanceVariableNames: 'jvm registry sharedMutex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JVM
	instanceVariableNames: 'name status javaVM jniEnv library jniHelper sharedMutex settings classRegistry classIndex rawJavaLangSystem rawJavaLangObject rawJavaLangClass isArrayMethodID isInterfaceMethodID identityHashCodeMethodID hashCodeMethodID equalsMethodID hasJava5Extensions externalCallbacks localCapacity localCount globalCount objectsCreated objectsReleased watchers supplementaryClassloaders'
	classVariableNames: 'InstancesStarted RunningInstances'
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
Object subclass: #JVMSettings
	instanceVariableNames: 'name categories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'template predefined'!
Object subclass: #JVMSubSettings
	instanceVariableNames: 'flags'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JVMWatcher
	instanceVariableNames: 'jvmIsInitialized'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SupplementaryClassloader
	instanceVariableNames: 'name path parentName classloaderClass classloader isEnabled containingTree'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SupplementaryClassloaderTree
	instanceVariableNames: 'jvm entriesByName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #JavaException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #NoJavaSuperclassException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaClassIndex subclass: #JavaMainClassIndex
	instanceVariableNames: 'jvm'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaObject subclass: #JavaInstance
	instanceVariableNames: 'static'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaObject subclass: #JavaStatic
	instanceVariableNames: 'jvm classObject javaSuperclass instanceClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInstance subclass: #JavaClassInstance
	instanceVariableNames: 'jniObject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInstance subclass: #JavaInterfaceInstance
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInstance subclass: #JavaNonvirtual
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInstance subclass: #JavaPrimitiveInstance
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaClassInstance subclass: #JavaLangObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaAggregate
	instanceVariableNames: 'adaptorCache sizeCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaLangClassLoader
	instanceVariableNames: 'classIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaLangReflectAccessibleObject
	instanceVariableNames: 'nameCache declaredInCache declaredInCacheIsValid modifiersCache'
	classVariableNames: ''
	poolDictionaries: 'JavaReflectionConstants'
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaLangThread
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaLangThrowable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaAggregate subclass: #JavaLangString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaAggregate subclass: #JavaObjectArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaAggregate subclass: #JavaPrimitiveArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'JNIConstants'
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaBooleanArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaCharArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaDoubleArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaFloatArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaIntArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaLongArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveArray subclass: #JavaShortArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangClassLoader subclass: #JavaNetURLClassLoader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangReflectAccessibleObject subclass: #JavaLangClass
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangReflectAccessibleObject subclass: #JavaLangReflectConstructor
	instanceVariableNames: 'parameterTypesCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangReflectAccessibleObject subclass: #JavaLangReflectField
	instanceVariableNames: 'typeCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangReflectAccessibleObject subclass: #JavaLangReflectMethod
	instanceVariableNames: 'typeCache parameterTypesCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaStatic subclass: #JavaClassStatic
	instanceVariableNames: 'registry allInstancesAreCanonical'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaStatic subclass: #JavaInterfaceStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaStatic subclass: #JavaPrimitiveStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaClassStatic subclass: #StaticJavaLangObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #JavaArrayClassStatic
	instanceVariableNames: 'elementClassCache instanceElementWrapperCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangClassLoader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangReflectModifier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangReflectProxy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangSystem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangThread
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangThrowable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangClassLoader subclass: #StaticJavaNetURLClassLoader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveBooleanStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveByteStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveCharStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveDoubleStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveFloatStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveIntStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveLongStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitivesShortStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaPrimitiveStatic subclass: #JavaPrimitiveVoidStatic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaObjectRegistry subclass: #JavaClassRegistry
	instanceVariableNames: 'javaLangClass javaLangObject javaLangString notificationQueue'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVM subclass: #JVMWithoutCallbacks
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMSubSettings subclass: #JavaRuntimeSettings
	instanceVariableNames: 'options'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVMSubSettings subclass: #JNIPortSettings
	instanceVariableNames: 'jvmClass jniLibraryClass jniLibraryFilename jniLibraryVersion jniLibraryJVMType watcherClasses'
	classVariableNames: ''
	poolDictionaries: 'JavaBaseConstants'
	classInstanceVariableNames: ''!
JVMSubSettings subclass: #SupplementaryClassloadersSettings
	instanceVariableNames: 'entries'
	classVariableNames: ''
	poolDictionaries: 'SupplementaryClassloaderConstants'
	classInstanceVariableNames: ''!
SearchPolicy subclass: #JavaEqualitySearchPolicy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ByteArray methodsFor!

asJavaByteArray: aJVM
	"Answer a JavaByteArray object which is owned by aJVM."
#CUadded.

	^ JavaByteArray fromByteArray: self jvm: aJVM.!

asJavaCharArray: aJVM
	"Answer a JavaCharArray object which is owned by aJVM."
#CUadded.

	^ JavaCharArray fromByteArray: self jvm: aJVM.!

asJavaString: aJVM
	"Answer a JavaLangString object which is owned by aJVM."
#CUadded.

	^ JavaLangString fromByteArray: self jvm: aJVM.! !
!ByteArray categoriesFor: #asJavaByteArray:!converting!public! !
!ByteArray categoriesFor: #asJavaCharArray:!converting!public! !
!ByteArray categoriesFor: #asJavaString:!converting!public! !

!BYTEArray methodsFor!

allSatisfy: a1Block
	"see Collection>>allSatisfy"

	1 to: self size do: [:i | (a1Block value: (self at: i)) ifFalse: [^ false]].
	^ true.!

asString
	"see ByteArray>>asString"

	^ (String new: length)
		replaceFrom: 1 to: length
		with: self startingAt: 1.!

readStream
	"see ByteArray>>readStream"

	^ ReadStream on: self
! !
!BYTEArray categoriesFor: #allSatisfy:!enumerating!public! !
!BYTEArray categoriesFor: #asString!converting!public! !
!BYTEArray categoriesFor: #readStream!public!streaming! !

!Collection methodsFor!

asJavaArrayOf: aJavaStatic
	"Answer a Java array object with element type defined by aJavaStatic and holding
	the elements of the receiver."
#CUadded.

	^ aJavaStatic newArrayWithAll: self.
! !
!Collection categoriesFor: #asJavaArrayOf:!converting!public! !

!JNIClass methodsFor!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM.
	Note that it is an error for a managed object to be owned by more than one
	JVM or to change JVMs"

	^ aJVM wrapJNIClass: self.! !
!JNIClass categoriesFor: #asJavaObject:!converting!public! !

!JNIClassG methodsFor!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM.
	Note that it is an error for a managed object to be owned by more than one
	JVM or to change JVMs"

	^ aJVM wrapJNIClass: self.! !
!JNIClassG categoriesFor: #asJavaObject:!converting!public! !

!JNIObject methodsFor!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM.
	Note that it is an error for a managed object to be owned by more than one
	JVM or to change JVMs"

	^ aJVM wrapJNIObject: self.! !
!JNIObject categoriesFor: #asJavaObject:!converting!public! !

!JNIObjectG methodsFor!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM.
	Note that it is an error for a managed object to be owned by more than one
	JVM or to change JVMs"

	^ aJVM wrapJNIObject: self.! !
!JNIObjectG categoriesFor: #asJavaObject:!converting!public! !

!JNIReference methodsFor!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM.
	Note that it is an error for a managed object to be owned by more than one
	JVM or to change JVMs"

	self subclassResponsibility.
! !
!JNIReference categoriesFor: #asJavaObject:!converting!public! !

!String methodsFor!

asJavaByteArray: aJVM
	"Answer a JavaByteArray object which is owned by aJVM."
#CUadded.

	^ JavaByteArray fromString: self jvm: aJVM.!

asJavaCharArray: aJVM
	"Answer a JavaCharArray object which is owned by aJVM."
#CUadded.

	^ JavaCharArray fromString: self jvm: aJVM.!

asJavaString: aJVM
	"Answer a JavaLangString object which is owned by aJVM"
#CUadded.

	^ JavaLangString fromString: self jvm: aJVM.!

asJNIClassName
	"On the assumption that the receiver is the name of a Java class in JNI format (xxx/yyy/zzz/Class),
	or Java format (xxx.yyy.zzz.Class), answer the equvalent String name in JNI format."
#CUadded.

	"try to identify cases where we're OK already quickly"
	self isEmpty ifTrue: [^ self].
	(self identityIncludes: $/) ifTrue: [^ self].

	"check for array sufixes, [] -- arrays are given the JNI signature name (for some reason)"
	(self endsWith: '[]') ifTrue: [^ self asJNISignature].

	"there is nothing we can do about the names of primitives that are not in arrays, since JNI won't
	let you look them up by name at all"
	
	"note that this won't handle nested class names properly -- you have to use $ not ."
	^ self copyReplacing: $. withObject: $/.!

asJNISignature
	"On the assumption that the receiver is the name of a Java class in JNI format (xxx/yyy/zzz/Class),
	or Java format (xxx.yyy.zzz.Class), or a JNI signature string, then answer the equvalent String in JNI
	signature format."
	| stem depth stream map |
#CUadded.

	"try to identify the cases where it's already in JNI signature format quickly"
	self isEmpty ifTrue: [^ self].
	self last = $; ifTrue: [^ self].
	self first = $[ ifTrue: [^ self].
	(self size = 1 and: ['BCDFIJLSVZ' identityIncludes: self first]) ifTrue: [^ self].

	stream := String writeStream: self size + 2.

	"check for strings in JNI classname but not JNI signature format"
	(self identityIncludes: $/) ifTrue:
		[^ stream
			nextPut: $L;
			nextPutAll: self;
			nextPut: $;;
			contents].

	"handle as many layers of array nesting as are needed"
	stem := self.
	depth := self occurrencesOf: $[.
	depth > 0 ifTrue:
		[stream next: depth put: $[.
		stem := stem upTo: $[].

	"check for primitive types"
	map := ##((LookupTable new: 10)
			at: 'byte' put: $B;
			at: 'char' put: $C;
			at: 'double' put: $D;
			at: 'float' put: $F;
			at: 'int' put: $I;
			at: 'long' put: $J;
			at: 'short' put: $S;
			at: 'void' put: $V;
			at: 'boolean' put: $Z;
			yourself).
	map at: stem ifPresent: [:code | stream nextPut: code. ^ stream contents].

	stream nextPut: $L.
	stem do: [:each | each = $. ifTrue: [stream nextPut: $/] ifFalse: [stream nextPut: each]].
	stream nextPut: $;.

	^ stream contents.! !
!String categoriesFor: #asJavaByteArray:!converting!public! !
!String categoriesFor: #asJavaCharArray:!converting!public! !
!String categoriesFor: #asJavaString:!converting!public! !
!String categoriesFor: #asJNIClassName!converting!public! !
!String categoriesFor: #asJNISignature!converting!public! !

!UndefinedObject methodsFor!

asJavaByteArray: aJVM
	"Answer a JavaByteArray object which is owned by aJVM."
#CUadded.

	"nil is a valid java object corresponding to Java null, and since null is of type byte[],
	it follows that nil is a valid Java byte[]"
	^ self.!

asJavaCharArray: aJVM
	"Answer a JavaCharArray object which is owned by aJVM."
#CUadded.

	"nil is a valid java object corresponding to Java null, and since null is of type char[],
	it follows that nil is a valid Java char[]"
	^ self.!

asJavaObject: aJVM
	"answer a managed JavaObject which corresponds to the same underlying
	object as the receiver and which is managed by aJVM"
#CUadded.

	"nil is a valid java object corresponding to Java null"
	^ self.!

asJavaString: aJVM
	"Answer a JavaLangString object which is owned by aJVM."
#CUadded.

	"nil is a valid java object corresponding to Java null, and since null is of type java.lang.String,
	it follows that nil is a valid Java string"
	^ self.
! !
!UndefinedObject categoriesFor: #asJavaByteArray:!converting!public! !
!UndefinedObject categoriesFor: #asJavaCharArray:!converting!public! !
!UndefinedObject categoriesFor: #asJavaObject:!converting!public! !
!UndefinedObject categoriesFor: #asJavaString:!converting!public! !

!UnicodeString methodsFor!

asJavaString: aJVM
	"Answer a JavaLangString object which is owned by aJVM."
#CUadded.

	^ JavaLangString fromUnicodeString: self jvm: aJVM.! !
!UnicodeString categoriesFor: #asJavaString:!converting!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #JavaBaseConstants put: (PoolConstantsDictionary named: #JavaBaseConstants)!
JavaBaseConstants at: 'USE_ABORT_HOOK_MASK' put: 16r1!
JavaBaseConstants at: 'USE_EXIT_HOOK_MASK' put: 16r2!
JavaBaseConstants at: 'USE_JNI_HELPER_LIBRARY_MASK' put: 16r10!
JavaBaseConstants at: 'USE_VFPRINTF_HOOK_MASK' put: 16r4!
JavaBaseConstants at: 'USE_VFPRINTF_REDIRECT_MASK' put: 16r8!
JavaBaseConstants shrink!

Smalltalk at: #JavaReflectionConstants put: (PoolConstantsDictionary named: #JavaReflectionConstants)!
JavaReflectionConstants at: 'MODIFIER_ABSTRACT' put: 16r400!
JavaReflectionConstants at: 'MODIFIER_FINAL' put: 16r10!
JavaReflectionConstants at: 'MODIFIER_INTERFACE' put: 16r200!
JavaReflectionConstants at: 'MODIFIER_NATIVE' put: 16r100!
JavaReflectionConstants at: 'MODIFIER_PRIVATE' put: 16r2!
JavaReflectionConstants at: 'MODIFIER_PROTECTED' put: 16r4!
JavaReflectionConstants at: 'MODIFIER_PUBLIC' put: 16r1!
JavaReflectionConstants at: 'MODIFIER_STATIC' put: 16r8!
JavaReflectionConstants at: 'MODIFIER_STRICT' put: 16r800!
JavaReflectionConstants at: 'MODIFIER_SYNCHRONIZED' put: 16r20!
JavaReflectionConstants at: 'MODIFIER_TRANSIENT' put: 16r80!
JavaReflectionConstants at: 'MODIFIER_VOLATILE' put: 16r40!
JavaReflectionConstants shrink!

Smalltalk at: #SupplementaryClassloaderConstants put: (PoolConstantsDictionary named: #SupplementaryClassloaderConstants)!
SupplementaryClassloaderConstants at: 'USE_AT_STARTUP' put: 16r1!
SupplementaryClassloaderConstants shrink!

"Classes"!

JavaClassIndex guid: (GUID fromString: '{B42218A5-1DD6-4CCA-93B5-C9F8D9D8964C}')!
JavaClassIndex comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

This is a cache keyed by the Java name of a class (e.g. int, java.lang.String, double[][]) expressed as a Symbol and containing the relevant class static.

These are used as helpers by <javaClassFinder>s --  which is to say, either by the JVM object itself (though that actually uses an instance of a subclass) or by a java.lang.Classloader or a LocalJavaClasspathEntry.

Note that the same class may appear in this index several times under different names.'!
!JavaClassIndex categoriesForClass!Unclassified! !
!JavaClassIndex methodsFor!

addClass: aClassStatic
	"add aClassStatic to our index, note that the name used
	will be the class static's own idea of its name"

	| name |

	name := aClassStatic name asSymbol.

	sharedMutex critical: [index at: name put: aClassStatic].!

averageProbesPerElement
	"answer the <Float> average number of probes to find an item already in the index table"

	^ sharedMutex critical: [index averageProbesPerElement].!

classFinder
	"answer the <javaClassFinder> which which we are associated, and
	for which we act as a lookup cache"

	^ classFinder.!

classFinder: aClassFinder
	"private -- set the <javaClassFinder> we will use"

	sharedMutex := aClassFinder sharedMutex.
	classFinder := aClassFinder.
!

findClass: aClassName
	"answer a JavaStatic object corresponding to aClassName or throw a Java
	ClassNotFound exception if no such class can be loaded.

	This looks in the index first (if necessary converting aClassName to a Symbol), and
	only if the name is not found there does it go back to the class finder to lookup the name
	(converting it to a JNI class name if necessary).  Once discovered it'll be entered
	into the index under the orginal name (as a Symbol).

	This interface is only suitable for finding classes with no explicit class loader, and so
	there can only be one class found *through this interface* with any given name"

	| symbol classStatic |

	"NB: we've got to be a bit careful with the logic here, since we don't want ClassNotFound
	exceptions to be thrown while we've got either mutex locked"

	"see if we've already got it indexed"
	symbol := aClassName asSymbol.
	sharedMutex critical: [index at: symbol ifPresent: [:it | ^ it]].

	"no, have to go back to the class finder for it"
	classStatic := (classFinder findClassObject: aClassName asJNIClassName) classStatic.

	"OK, the class is now known, and must be in the registry.  We now add it to the index;
	since we've been running outside the index mutex, it is quite possible (albeit unlikely)
	that someone's already added it, but if so them we'll just overwrite the existing record
	with the same data"
	sharedMutex critical: [index at: symbol put: classStatic].

	^ classStatic.!

initialize: aCapacity
	"private -- establish a coherent initial state"

	index := self makeIndex: aCapacity.
!

jvm
	"answer the JVM which, ultimately, owns us"

	^ classFinder jvm.!

makeIndex: aCapacity
	"private -- answer a new index table of the stated capacity"

	^ IdentityDictionary new: aCapacity.!

names
	"answer an OrderedCollection of all the names in the index"

	^ sharedMutex critical: [index keys asOrderedCollection].!

namesAndClassesDo: a2Block
	"evaluate a2Block for each object in the index"

	^ sharedMutex critical: [index keysAndValuesDo: a2Block].!

purge: aJavaStatic
	"private -- do *NOT* call this!!"

	sharedMutex critical:
		[| keys |
		keys := index keys select: [:each | (index at: each) == aJavaStatic].
		index removeAllKeys: keys].!

shutdown
	"private -- called by our owning class finder when it shuts down"

	sharedMutex critical: [index := self makeIndex: 0].
!

tableLoad
	"answer the <Float> percentage load on the index table"

	^ sharedMutex critical: [index size * 100.0 / index basicSize].!

tableSize
	"answer how many class names have been indexed"

	^ sharedMutex critical: [index size].! !
!JavaClassIndex categoriesFor: #addClass:!adding!public! !
!JavaClassIndex categoriesFor: #averageProbesPerElement!measuring!public! !
!JavaClassIndex categoriesFor: #classFinder!accessing!public! !
!JavaClassIndex categoriesFor: #classFinder:!initializing!private! !
!JavaClassIndex categoriesFor: #findClass:!public!searching! !
!JavaClassIndex categoriesFor: #initialize:!initializing!private! !
!JavaClassIndex categoriesFor: #jvm!accessing!public! !
!JavaClassIndex categoriesFor: #makeIndex:!helpers!private! !
!JavaClassIndex categoriesFor: #names!accessing!public! !
!JavaClassIndex categoriesFor: #namesAndClassesDo:!enumerating!public! !
!JavaClassIndex categoriesFor: #purge:!private!removing! !
!JavaClassIndex categoriesFor: #shutdown!operations!private! !
!JavaClassIndex categoriesFor: #tableLoad!measuring!public! !
!JavaClassIndex categoriesFor: #tableSize!accessing!measuring!public! !

!JavaClassIndex class methodsFor!

defaultCapacity
	"private -- answer the default capacity to use for instances"

	^ 10.!

new
	"private -- use #newFor:"

	self shouldNotImplement.!

new: aCapacity
	"private -- use #new:for:"

	^ (self basicNew)
		initialize: aCapacity;
		yourself.!

new: aCapacity for: aClassFinder
	"answer a new instance which uses aClassFinder to find previously unknown classes,
	and which has the given initial capacity"

	^ (self basicNew)
		initialize: aCapacity;
		classFinder: aClassFinder;
		yourself.!

newFor: aClassFinder
	"answer a new instance which uses aJVM to find previously unknown classes"

	^ self new: self defaultCapacity for: aClassFinder.! !
!JavaClassIndex class categoriesFor: #defaultCapacity!constants!private! !
!JavaClassIndex class categoriesFor: #new!instance creation!private! !
!JavaClassIndex class categoriesFor: #new:!instance creation!private! !
!JavaClassIndex class categoriesFor: #new:for:!instance creation!public! !
!JavaClassIndex class categoriesFor: #newFor:!instance creation!public! !

JavaObject guid: (GUID fromString: '{E571B36E-C290-4495-BAF9-E9E87F320AC8}')!
JavaObject comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class that captures the commonality amongst a number of subclasses that all, in some way or other, act as "proxies" for entities in a Java runtime.  The principle subclasses are JavaInstance, which are proxies for Java objects, and JavaStatic, which are "proxies" for Java classes (the name proxy is misleading, "facade" might be better, because there is no such object as a Java class, however they act uniformly with the real proxies, so...).

The immediate subclasses (down a couple of levels) are the JNIPort framework.  Below that are various custom subclasses that wrap different Java classes.  E.g. JavaLangString corresponds to (the instance side of) java.lang.String.

The main features of this class are that instances all:
	- know what their owning JVM is (and therefore also know the JNIEnv to use to talk to the Java runtime).
	- have a variety of methods for calling Java methods and accessing Java fields.

Many of thoese methods require a JNI signature.  For more information on them see either the Java (JNI or JVM) documentation, or read Docs\JNIPort\jni-names.html.'!
!JavaObject categoriesForClass!Unclassified! !
!JavaObject methodsFor!

_debugPrintString

	#CUtodo. ^ self basicPrintString.
    !

allFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this includes fields inherited from superclasses.  Fields from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	self subclassResponsibility.!

allMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of our Java object's instance-side
	methods, this includes methods inherited from superclasses.  Methods from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	self subclassResponsibility.
!

allRealFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this includes fields *genuinely* inherited from superclasses.  Fields from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	self subclassResponsibility.
!

asParameter
	"answer the receiver in a form suitable for passing to an ExternalLibrary call,
	i.e. the ExternalAddress of our underlying Java object"

	^ self managedInstance jniObject.!

beNotAGhost
	"private -- used during clean-up.  Stop being an instance of a ghost class"

	self isGhost ifTrue: [self becomeA: self class ghostClassRoot].!

callBooleanMethod: aStringName
	"invoke the zero-argument, boolean-valued, method named by aStringName on our Java object"

	^ self callBooleanMID: (self findMethod: aStringName signature: '()Z').!

callBooleanMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the boolean-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callBooleanMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callBooleanMID: aMethodID
	"invoke the zero-argument, boolean-valued, method named by aMethodID on our Java object"

	^ self
		callBooleanMID: aMethodID
		withArguments: nil.!

callBooleanMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callByteMethod: aStringName
	"invoke the zero-argument, byte-valued, method named by aStringName on our Java object"

	^ self callByteMID: (self findMethod: aStringName signature: '()B').!

callByteMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the byte-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callByteMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callByteMID: aMethodID
	"invoke the zero-argument, byte-valued, method named by aMethodID on our Java object"

	^ self
		callByteMID: aMethodID
		withArguments: nil.!

callByteMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callCharMethod: aStringName
	"invoke the zero-argument, char-valued, method named by aStringName on our Java object.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	^ self callCharMID: (self findMethod: aStringName signature: '()C').!

callCharMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the char-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	^ self
		callCharMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callCharMID: aMethodID
	"invoke the zero-argument, char-valued, method named by aMethodID on our Java object.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	^ self
		callCharMID: aMethodID
		withArguments: nil.!

callCharMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	self subclassResponsibility.!

callDoubleMethod: aStringName
	"invoke the zero-argument, double-valued, method named by aStringName on our Java object"

	^ self callDoubleMID: (self findMethod: aStringName signature: '()D').!

callDoubleMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the double-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callDoubleMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callDoubleMID: aMethodID
	"invoke the zero-argument, double-valued, method named by aMethodID on our Java object"

	^ self
		callDoubleMID: aMethodID
		withArguments: nil.!

callDoubleMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callFloatMethod: aStringName
	"invoke the zero-argument, float-valued, method named by aStringName on our Java object"

	^ self callFloatMID: (self findMethod: aStringName signature: '()F').!

callFloatMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the float-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callFloatMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callFloatMID: aMethodID
	"invoke the zero-argument, float-valued, method named by aMethodID on our Java object"

	^ self
		callFloatMID: aMethodID
		withArguments: nil.!

callFloatMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callIntMethod: aStringName
	"invoke the zero-argument, int-valued, method named by aStringName on our Java object"

	^ self callIntMID: (self findMethod: aStringName signature: '()I').!

callIntMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the int-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callIntMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callIntMID: aMethodID
	"invoke the zero-argument, int-valued, method named by aMethodID on our Java object"

	^ self
		callIntMID: aMethodID
		withArguments: nil.!

callIntMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callLongMethod: aStringName
	"invoke the zero-argument, long-valued, method named by aStringName on our Java object"

	^ self callLongMID: (self findMethod: aStringName signature: '()J').!

callLongMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the long-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callLongMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callLongMID: aMethodID
	"invoke the zero-argument, long-valued, method named by aMethodID on our Java object"

	^ self
		callLongMID: aMethodID
		withArguments: nil.!

callLongMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callMethod: aStringName signature: aJNISignature
	"invoke the zero-argument method defined by aStringName and aJNISignature on our Java object,
	 with the given JNIValueArray of arguments.
	Note that we use the signature to induce which method to call"

	| type mid |

	type := aJNISignature last.
	mid := self findMethod: aStringName signature: aJNISignature.

	"Smalltalk has no need of a case-switch.  Yeah right..."
	type = $; ifTrue: [^ self callObjectMID: mid].
	type = $I ifTrue: [^ self callIntMID: mid].
	type = $V ifTrue: [^ self callVoidMID: mid].
	type = $Z ifTrue: [^ self callBooleanMID: mid].
	type = $C ifTrue: [^ self callCharMID: mid].
	type = $F ifTrue: [^ self callFloatMID: mid].
	type = $J ifTrue: [^ self callLongMID: mid].
	type = $D ifTrue: [^ self callDoubleMID: mid].
	type = $B ifTrue: [^ self callByteMID: mid].
	type = $S ifTrue: [^ self callShortMID: mid].

	Error signal: 'Unrecognised JNI signature' with: aJNISignature.
!

callMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the method defined by aStringName and aJNISignature on our Java object,
	 with the given JNIValueArray of arguments.
	Note that we use the signature to induce which method to call"

	| type mid |

	type := aJNISignature last.
	mid := self findMethod: aStringName signature: aJNISignature.

	"Smalltalk has no need of a case-switch.  Yeah right..."
	type = $; ifTrue: [^ self callObjectMID: mid withArguments: aJNIValueArray].
	type = $I ifTrue: [^ self callIntMID: mid withArguments: aJNIValueArray].
	type = $V ifTrue: [^ self callVoidMID: mid withArguments: aJNIValueArray].
	type = $Z ifTrue: [^ self callBooleanMID: mid withArguments: aJNIValueArray].
	type = $C ifTrue: [^ self callCharMID: mid withArguments: aJNIValueArray].
	type = $F ifTrue: [^ self callFloatMID: mid withArguments: aJNIValueArray].
	type = $J ifTrue: [^ self callLongMID: mid withArguments: aJNIValueArray].
	type = $D ifTrue: [^ self callDoubleMID: mid withArguments: aJNIValueArray].
	type = $B ifTrue: [^ self callByteMID: mid withArguments: aJNIValueArray].
	type = $S ifTrue: [^ self callShortMID: mid withArguments: aJNIValueArray].

	Error signal: 'Unrecognised JNI signature' with: aJNISignature.
!

callObjectMethod: aStringName
	"invoke the java.lang.Object-valued, zero-argument, method named by aStringName on our Java object"

	^ self callObjectMID: (self findMethod: aStringName signature: '()Ljava/lang/Object;').
!

callObjectMethod: aStringName signature: aJNISignature
	"invoke the zero-argument, object-valued, method named by aStringName on our Java object"

	^ self callObjectMID: (self findMethod: aStringName signature: aJNISignature).
!

callObjectMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the object-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callObjectMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.
!

callObjectMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the object-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	^ self
		callObjectMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray
		wrapperFactory: aJVMOrJavaStatic.
!

callObjectMethod: aStringName signature: aJNISignature wrapperFactory: aJVMOrJavaStatic
	"invoke the zero-argument, object-valued, method named by aStringName on our Java object.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	^ self
		callObjectMID: (self findMethod: aStringName signature: aJNISignature)
		wrapperFactory: aJVMOrJavaStatic.
!

callObjectMID: aMethodID
	"invoke the zero-argument, object-valued, method named by aMethodID on our Java object"

	^ self
		callObjectMID: aMethodID
		withArguments: nil.!

callObjectMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callObjectMID: aMethodID
		withArguments: aJNIValueArray
		wrapperFactory: self jvm.!

callObjectMID: aMethodID withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	self subclassResponsibility.!

callObjectMID: aMethodID wrapperFactory: aJVMOrJavaStatic
	"invoke the zero-argument, object-valued, method named by aMethodID on our Java object.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	^ self
		callObjectMID: aMethodID
		withArguments: nil
		 wrapperFactory: aJVMOrJavaStatic.!

callShortMethod: aStringName
	"invoke the zero-argument, short-valued, method named by aStringName on our Java object"

	^ self callShortMID: (self findMethod: aStringName signature: '()S').!

callShortMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the short-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callShortMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callShortMID: aMethodID
	"invoke the zero-argument, short-valued, method named by aMethodID on our Java object"

	^ self
		callShortMID: aMethodID
		withArguments: nil.!

callShortMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

callStringMethod: aStringName
	"invoke the java.lang.String-valued, zero-argument, method named by aStringName on our Java object"

	^ self callObjectMID: (self findMethod: aStringName  signature: '()Ljava/lang/String;').
!

callVoidMethod: aStringName
	"invoke the zero-argument, void-valued, method named by aStringName on our Java object"

	^ self callVoidMID: (self findMethod: aStringName signature: '()V').!

callVoidMethod: aStringName signature: aJNISignature withArguments: aJNIValueArray
	"invoke the void-valued method named by aStringName on our Java object with the given
	JNIValueArray of arguments"

	^ self
		callVoidMID: (self findMethod: aStringName signature: aJNISignature)
		withArguments: aJNIValueArray.!

callVoidMID: aMethodID
	"invoke the zero-argument, void-valued, method named by aMethodID on our Java object"

	^ self
		callVoidMID: aMethodID
		withArguments: nil.!

callVoidMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self subclassResponsibility.!

classRegistry
	"answer the JavaClassRegistry wherin the receiver's class is lodged"

	^ self jvm classRegistry.
!

fields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this does not includes fields inherited from superclasses.  They are in no special order"

	self subclassResponsibility.
!

findField: aStringName signature: aJNISignature
	"answer the JavaFieldID corresponding to our Java object's field named aStringName,
	with signature defined by aJNISignature"

	self subclassResponsibility.!

findMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java object's method named aString,
	and signature defined by aJNISignature"

	self subclassResponsibility.!

getBooleanFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getBooleanField: aStringName
	"answer the value of the boolean field of our Java object that is named by
	aStringName"

	^ self getBooleanFID: (self findField: aStringName signature: 'Z').!

getByteFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getByteField: aStringName
	"answer the value of the byte field of our Java object that is named by
	aStringName"

	^ self getByteFID: (self findField: aStringName signature: 'B').!

getCharFID: aFieldID
	"get the value of the field defined by aFieldID.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	self subclassResponsibility.!

getCharField: aStringName
	"answer the value of the char field of our Java object that is named by
	aStringName.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	^ self getCharFID: (self findField: aStringName signature: 'C').!

getDoubleFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getDoubleField: aStringName
	"answer the value of the double field of our Java object that is named by
	aStringName"

	^ self getDoubleFID: (self findField: aStringName signature: 'D').!

getField: aStringName signature: aJNISignature
	"get the value of the field named by aStringName and the given signature"

	| type fid |

	fid := self findField: aStringName signature: aJNISignature.
	type := aJNISignature last.

	"Smalltalk has no need of a case-switch.  Yeah right..."

	type = $; ifTrue: [^ self getObjectFID: fid].
	type = $I ifTrue: [^ self getIntFID: fid].
	type = $Z ifTrue: [^ self getBooleanFID: fid].
	type = $C ifTrue: [^ self getCharFID: fid].
	type = $F ifTrue: [^ self getFloatFID: fid].
	type = $J ifTrue: [^ self getLongFID: fid].
	type = $D ifTrue: [^ self getDoubleFID: fid].
	type = $B ifTrue: [^ self getByteFID: fid].
	type = $S ifTrue: [^ self getShortFID: fid].

	Error signal: 'Unrecognised JNI signature' with: aJNISignature.
!

getFloatFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getFloatField: aStringName
	"answer the value of the float field of our Java object that is named by
	aStringName"

	^ self getFloatFID: (self findField: aStringName signature: 'F').!

getIntFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getIntField: aStringName
	"answer the value of the int field of our Java object that is named by
	aStringName"

	^ self getIntFID: (self findField: aStringName signature: 'I').!

getLongFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getLongField: aStringName
	"answer the value of the long field of our Java object that is named by
	aStringName"

	^ self getLongFID: (self findField: aStringName signature: 'J').!

getObjectFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self
		getObjectFID: aFieldID
		wrapperFactory: self jvm.!

getObjectFID: aFieldID wrapperFactory: aJVMOrJavaStatic
	"get the value of the field defined by aFieldID.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	self subclassResponsibility.!

getObjectField: aStringName
	"get the value of the java.lang.Object-valued field of our Java object that is named by aStringName"

	^ self getObjectFID: (self findField: aStringName signature: 'Ljava/lang/Object;').!

getObjectField: aStringName signature: aJNISignature
	"get the value of the field of our Java object that is named by aStringName"

	^ self getObjectFID: (self findField: aStringName signature: aJNISignature).!

getShortFID: aFieldID
	"get the value of the field defined by aFieldID"

	self subclassResponsibility.!

getShortField: aStringName
	"answer the value of the short field of our Java object that is named by
	aStringName"

	^ self getShortFID: (self findField: aStringName signature: 'S').!

getStringField: aStringName
	"get the value of the java.lang.String-valued field of our Java object that is named by aStringName"

	^ self getObjectFID: (self findField: aStringName signature: 'Ljava/lang/String;').!

isDead
	"answer whether this instance no longer represents a live connection to an object in a Java Virtual Machine"

	^ self jniObject isNil.!

isGhost
	"answer whether this instance is a member of a 'ghost class'"

	^ self class isGhostClass.!

isGlobalRef
	"answer true if we are wrapping a JNI global ref"

	^ self isLive and: [self jniObject isGlobalRef].!

isLive
	"answer whether this instance still represents a live connection to a Java Virtual Machine"

	^ self jniObject notNil.!

isLocalRef
	"answer true if we are wrapping a JNI local ref"

	^ self isLive and: [self jniObject isLocalRef].!

isOwnedBy: aJVM
	"answer whether our owning JVM is that given"

	^ self jvm == aJVM.!

isSameAs: aJavaObjectOrJNIObject
	"answer whether the receiver is = (in Java terms, i.e. this is an identity comparison) to aJavaObjectOrJNIObject"

	^ self jvm is: self identicalTo: aJavaObjectOrJNIObject.
!

javaVM
	"answer the receiver's JavaVM"

	^ self jvm javaVM.!

jniEnv
	"answer the receiver's (current) JNIEnv"

	^ self jvm jniEnv.!

jniObject
	"answer the JNIObject which is our real handle on the underlying Java object"

	^ self managedInstance jniObject.!

jvm
	"answer the receiver's owning JVM"

	self subclassResponsibility.!

jvmSettings
	"answer the receiver's JVM's settings"

	^ self jvm settings.!

managedInstance
	"answer the managed object (a JavaClassInstance) which really owns the javaObject we wrap"

	self subclassResponsibility.!

methods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of our Java object's instance-side
	methods, this does not includes methods inherited from superclasses.  They are in no special order"

	self subclassResponsibility.
!

monitorEnter
	"acquire on the current object.  Note it is better -- by far -- to use
	the #synchronized: method, since that will look after unlocking automatically"

	self jniEnv
		MonitorEnter_obj: self jniObject asParameter
		onException: [:jex | self throwJavaException: jex].!

monitorExit
	"remove any lock on the current object.  Note it is better -- by far -- to use
	the #synchronized: method, since that will look after unlocking automatically"

	self jniEnv
		MonitorExit_obj: self jniObject asParameter
		onException: [:jex | self throwJavaException: jex].
!

printOn: aStream
	"write a developer oriented representation of the receiver to aStream"

	self isDead ifTrue: [^ aStream display: 'a dead '; display: self class].

	aStream
		basicPrint: self;
		display: '(';
"		display: (self jniObject yourAddress printStringRadix: 16 showRadix: true);
		display: (self jniObject isGlobalRef ifTrue: ['(Global)'] ifFalse: ['']);
		display: ', ';
"		display: self;
		display: ')'.
!

setBooleanFID: aFieldID to: aBool
	"set the value of the field defined by aFieldID to aBool"

	self subclassResponsibility.!

setBooleanField: aStringName to: aBool
	"set the value of the boolean field of our Java object that is named by
	aStringName to aBool"

	^ self setBooleanFID: (self findField: aStringName signature: 'Z') to: aBool.!

setByteFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self subclassResponsibility.!

setByteField: aStringName to: anInteger
	"set the value of the byte field of our Java object that is named by
	aStringName to anInteger"

	^ self setByteFID: (self findField: aStringName signature: 'B') to: anInteger.!

setCharFID: aFieldID to: aCharacterOrInteger
	"set the value of the field defined by aFieldID to aCharacterOrInteger"

	self subclassResponsibility.!

setCharField: aStringName to: aCharacterOrInteger
	"set the value of the char field of our Java object that is named by
	aStringName to aCharacterOrInteger"

	^ self setCharFID: (self findField: aStringName signature: 'C') to: aCharacterOrInteger.!

setDoubleFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self subclassResponsibility.!

setDoubleField: aStringName to: aFloat
	"set the value of the double field of our Java object that is named by
	aStringName to aFloat"

	^ self setDoubleFID: (self findField: aStringName signature: 'D') to: aFloat.!

setField: aStringName signature: aJNISignature to: anObject
	"set the value of the field named by aStringName and the given signature to anObject"

	| type fid |

	fid := self findField: aStringName signature: aJNISignature.
	type := aJNISignature last.

	"Smalltalk has no need of a case-switch.  Yeah right..."

	type = $; ifTrue: [^ self setObjectFID: fid to: anObject].
	type = $I ifTrue: [^ self setIntFID: fid to: anObject].
	type = $Z ifTrue: [^ self setBooleanFID: fid to: anObject].
	type = $C ifTrue: [^ self setCharFID: fid to: anObject].
	type = $F ifTrue: [^ self setFloatFID: fid to: anObject].
	type = $J ifTrue: [^ self setLongFID: fid to: anObject].
	type = $D ifTrue: [^ self setDoubleFID: fid to: anObject].
	type = $B ifTrue: [^ self setByteFID: fid to: anObject].
	type = $S ifTrue: [^ self setShortFID: fid to: anObject].

	Error signal: 'Unrecognised JNI signature' with: aJNISignature.
!

setFloatFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self subclassResponsibility.!

setFloatField: aStringName to: aFloat
	"set the value of the float field of our Java object that is named by
	aStringName to aFloat"

	^ self setFloatFID: (self findField: aStringName signature: 'F') to: aFloat.!

setIntFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self subclassResponsibility.!

setIntField: aStringName to: anInteger
	"set the value of the int field of our Java object that is named by
	aStringName to anInteger"

	^ self setIntFID: (self findField: aStringName signature: 'I') to: anInteger.!

setLongFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self subclassResponsibility.!

setLongField: aStringName to: anInteger
	"set the value of the long field of our Java object that is named by
	aStringName to anInteger"

	^ self setLongFID: (self findField: aStringName signature: 'J') to: anInteger.!

setObjectFID: aFieldID to: aJavaObjectOrNil
	"set the value of the field defined by aFieldID to aJavaObjectOrNil"

	self subclassResponsibility.!

setObjectField: aStringName signature: aJNISignature to: aJavaObjectOrNil
	"set the value of the object field of our Java object that is named by
	aStringName to aJavaObjectOrNil"

	^ self setObjectFID: (self findField: aStringName signature: aJNISignature) to: aJavaObjectOrNil.!

setObjectField: aStringName to: aJavaObjectOrNil
	"set the value of the java.lang.Object-valued field of our Java object that is named by
	aStringName to aJavaObjectOrNil"

	^ self
		setObjectFID: (self findField: aStringName signature: 'Ljava/lang/Object;')
		to: aJavaObjectOrNil.!

setShortFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self subclassResponsibility.!

setShortField: aStringName to: anInteger
	"set the value of the short field of our Java object that is named by
	aStringName to anInteger"

	^ self setShortFID: (self findField: aStringName signature: 'S') to: anInteger.!

setStringField: aStringName to: aStringOrSimilar
	"set the value of the java.lang.String-valued field of our Java object that is named by
	aStringName to aJavaObjectOrNil.
	Note that this method will accept Smalltalk as well as Java strings as the
	value to be assigned"

	^ self
		setObjectFID: (self findField: aStringName signature: 'Ljava/lang/String;')
		to: (aStringOrSimilar asJavaString: self jvm).!

shallowCopy
	"overridden since instances have no properly mutable state, hence a copy would be
	pointless"

	^ self.!

sharedMutex
	"answer the JVM-wide mutex"

	^ self jvm sharedMutex.!

synchronized: a0Block
	"answer the result of evaluating a0Block while holding the Java
	lock on the underyling object.
	NB: Caution is advised -- the thread models of Java (typically) and
	Dolphin (as currently implemented) are very different, this may give
	rise to unexpected consequences.
	In particular, all Dolphin code runs on the same OS thread, so
	Java will not stop two Dolphin Processes from accessing
	a locked object at the same time"

	self monitorEnter.
	^ a0Block ensure: [self monitorExit].! !
!JavaObject categoriesFor: #_debugPrintString!development!printing!public! !
!JavaObject categoriesFor: #allFields!public!reflection! !
!JavaObject categoriesFor: #allMethods!public!reflection! !
!JavaObject categoriesFor: #allRealFields!public!reflection! !
!JavaObject categoriesFor: #asParameter!converting!public! !
!JavaObject categoriesFor: #beNotAGhost!ghost classes!private! !
!JavaObject categoriesFor: #callBooleanMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callBooleanMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callBooleanMID:!Java method calls!public! !
!JavaObject categoriesFor: #callBooleanMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callByteMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callByteMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callByteMID:!Java method calls!public! !
!JavaObject categoriesFor: #callByteMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callCharMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callCharMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callCharMID:!Java method calls!public! !
!JavaObject categoriesFor: #callCharMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callDoubleMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callDoubleMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callDoubleMID:!Java method calls!public! !
!JavaObject categoriesFor: #callDoubleMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callFloatMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callFloatMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callFloatMID:!Java method calls!public! !
!JavaObject categoriesFor: #callFloatMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callIntMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callIntMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callIntMID:!Java method calls!public! !
!JavaObject categoriesFor: #callIntMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callLongMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callLongMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callLongMID:!Java method calls!public! !
!JavaObject categoriesFor: #callLongMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callMethod:signature:!Java method calls!public! !
!JavaObject categoriesFor: #callMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMethod:signature:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMethod:signature:withArguments:wrapperFactory:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMethod:signature:wrapperFactory:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMID:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMID:withArguments:wrapperFactory:!Java method calls!public! !
!JavaObject categoriesFor: #callObjectMID:wrapperFactory:!Java method calls!public! !
!JavaObject categoriesFor: #callShortMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callShortMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callShortMID:!Java method calls!public! !
!JavaObject categoriesFor: #callShortMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callStringMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callVoidMethod:!Java method calls!public! !
!JavaObject categoriesFor: #callVoidMethod:signature:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #callVoidMID:!Java method calls!public! !
!JavaObject categoriesFor: #callVoidMID:withArguments:!Java method calls!public! !
!JavaObject categoriesFor: #classRegistry!accessing!public! !
!JavaObject categoriesFor: #fields!public!reflection! !
!JavaObject categoriesFor: #findField:signature:!Java field access!public! !
!JavaObject categoriesFor: #findMethod:signature:!Java method calls!public! !
!JavaObject categoriesFor: #getBooleanFID:!Java field access!public! !
!JavaObject categoriesFor: #getBooleanField:!Java field access!public! !
!JavaObject categoriesFor: #getByteFID:!Java field access!public! !
!JavaObject categoriesFor: #getByteField:!Java field access!public! !
!JavaObject categoriesFor: #getCharFID:!Java field access!public! !
!JavaObject categoriesFor: #getCharField:!Java field access!public! !
!JavaObject categoriesFor: #getDoubleFID:!Java field access!public! !
!JavaObject categoriesFor: #getDoubleField:!Java field access!public! !
!JavaObject categoriesFor: #getField:signature:!Java field access!public! !
!JavaObject categoriesFor: #getFloatFID:!Java field access!public! !
!JavaObject categoriesFor: #getFloatField:!Java field access!public! !
!JavaObject categoriesFor: #getIntFID:!Java field access!public! !
!JavaObject categoriesFor: #getIntField:!Java field access!public! !
!JavaObject categoriesFor: #getLongFID:!Java field access!public! !
!JavaObject categoriesFor: #getLongField:!Java field access!public! !
!JavaObject categoriesFor: #getObjectFID:!Java field access!public! !
!JavaObject categoriesFor: #getObjectFID:wrapperFactory:!Java field access!public! !
!JavaObject categoriesFor: #getObjectField:!Java field access!public! !
!JavaObject categoriesFor: #getObjectField:signature:!Java field access!public! !
!JavaObject categoriesFor: #getShortFID:!Java field access!public! !
!JavaObject categoriesFor: #getShortField:!Java field access!public! !
!JavaObject categoriesFor: #getStringField:!Java field access!public! !
!JavaObject categoriesFor: #isDead!public!testing! !
!JavaObject categoriesFor: #isGhost!ghost classes!public!testing! !
!JavaObject categoriesFor: #isGlobalRef!public!testing! !
!JavaObject categoriesFor: #isLive!public!testing! !
!JavaObject categoriesFor: #isLocalRef!public!testing! !
!JavaObject categoriesFor: #isOwnedBy:!managed objects!public!testing! !
!JavaObject categoriesFor: #isSameAs:!comparing!public! !
!JavaObject categoriesFor: #javaVM!accessing!public! !
!JavaObject categoriesFor: #jniEnv!accessing!public! !
!JavaObject categoriesFor: #jniObject!accessing!public! !
!JavaObject categoriesFor: #jvm!accessing!managed objects!public! !
!JavaObject categoriesFor: #jvmSettings!accessing!managed objects!public! !
!JavaObject categoriesFor: #managedInstance!accessing!managed objects!public! !
!JavaObject categoriesFor: #methods!public!reflection! !
!JavaObject categoriesFor: #monitorEnter!public!synchronising! !
!JavaObject categoriesFor: #monitorExit!public!synchronising! !
!JavaObject categoriesFor: #printOn:!printing!public! !
!JavaObject categoriesFor: #setBooleanFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setBooleanField:to:!Java field access!public! !
!JavaObject categoriesFor: #setByteFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setByteField:to:!Java field access!public! !
!JavaObject categoriesFor: #setCharFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setCharField:to:!Java field access!public! !
!JavaObject categoriesFor: #setDoubleFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setDoubleField:to:!Java field access!public! !
!JavaObject categoriesFor: #setField:signature:to:!Java field access!public! !
!JavaObject categoriesFor: #setFloatFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setFloatField:to:!Java field access!public! !
!JavaObject categoriesFor: #setIntFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setIntField:to:!Java field access!public! !
!JavaObject categoriesFor: #setLongFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setLongField:to:!Java field access!public! !
!JavaObject categoriesFor: #setObjectFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setObjectField:signature:to:!Java field access!public! !
!JavaObject categoriesFor: #setObjectField:to:!Java field access!public! !
!JavaObject categoriesFor: #setShortFID:to:!Java field access!public! !
!JavaObject categoriesFor: #setShortField:to:!Java field access!public! !
!JavaObject categoriesFor: #setStringField:to:!Java field access!public! !
!JavaObject categoriesFor: #shallowCopy!copying!public! !
!JavaObject categoriesFor: #sharedMutex!accessing!public! !
!JavaObject categoriesFor: #synchronized:!public!synchronising! !

!JavaObject class methodsFor!

clearGhostsOwnedBy: aJVM
	"private -- make sure no ghost instances remain that are owned by aJVM"

	(self instancesOwnedBy: aJVM) do: [:each | each beNotAGhost].!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated (including ghost methods)
	methods which wrap Java constructors..
	Note that this does not include inherited selectors"

	^ #().
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated (including ghost methods)
	Java field getters.
	Note that this does not include inherited selectors"

	^ #().
!

generatedSelectors
	"answer an OrderedCollection of the selectors of automatically generated (including ghost methods)
	methods which wrap Java methods, constructors, and fields.
	Note that this does not include inherited selectors"

	^ (OrderedCollection new)
		addAll: self generatedConstructorSelectors;
		addAll: self generatedGetterSelectors;
		addAll: self generatedSetterSelectors;
		addAll: self generatedWrapperSelectors;
		yourself.
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated (including ghost methods)
	Java field setters.
	Note that this does not include inherited selectors"

	^ #().
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated (including ghost methods)
	methods which wrap Java methods.
	Note that this does not include inherited selectors"

	^ #().
!

icon
	"answer an Icon representing the receiver"

	^ JVM icon: 'JavaObject'.!

instancesOwnedBy: aJVM
	"answer a list of all the (sub) instances that are owned by aJVM"

	^ self allSubinstances select: [:each | each isOwnedBy: aJVM].!

isWrapperClass
	"answer whether this class is intended to correspond to some particular Java class"

	^ self class includesSelector: #javaClassName.!

registerWith: aClassFinder
	"this is called as JVM is initialised, or with a SupplementaryClassloader, and gives
	us the chance (should we wish to accept it) to register ourself with it as a wrapper
	class"

	self subclassResponsibility.!

registerWithJVM: aJVM
	"this is called as JVM is initialised and gives us the chance (should we wish to accept it)
	to register ourself with it as a wrapper class.
	NB: this is only called if #shouldRegisterWithJVM answers true"

	"most subclass classes don't care what sort of <javaClassFinder> they are registering with
	so the default is to pass it on to a more generic implementation"
	self registerWith: aJVM.!

registerWithSupplementaryClassloader: aSupplementaryClassloader
	"this is called with a SupplementaryClassloader, and gives us the chance
	(should we wish to accept it) to register ourself with it as a wrapper class.
	NB: this is only called if #shouldRegisterWithSupplementaryClassloader
	answers true"

	"most subclass classes don't care what sort of <javaClassFinder> they are registering with
	so the default is to pass it on to a more generic implementation"
	self registerWith: aSupplementaryClassloader.
!

registerWrapperClassesWithJVM: aJVM
	"called during JVM initialisation, gives us a chance to register our subclasses"

	aJVM classRegistry suspendRegistrationNotificationsWhile:
		[self allSubclassesPreOrderDo:
			[:each | (each shouldRegisterWithJVM: aJVM) ifTrue:
				[each registerWithJVM: aJVM]]].!

registerWrapperClassesWithSupplementaryClassloader: aSupplementaryClassloader
	"called when aSupplementaryClassloader is activated, gives us a chance to register
	any subclasses with it that want to be registered"

	aSupplementaryClassloader jvm classRegistry suspendRegistrationNotificationsWhile:
		[self allSubclassesPreOrderDo:
			[:each | (each shouldRegisterWithSupplementaryClassloader: aSupplementaryClassloader)
				ifTrue: [each registerWithSupplementaryClassloader: aSupplementaryClassloader]]].!

shouldRegisterWithJVM: aJVM
	"answer whether this class should be registered as a wrapper class with the given JVM"

	^ self isWrapperClass and: [self supplementaryClassloaderNames isEmpty].!

shouldRegisterWithSupplementaryClassloader: aSupplementaryClassloader
	"answer whether this class should be registered as a wrapper class with the given
	SupplementaryClassloader.
	Note, it is a logic error to answer true to both this and #shouldRegisterWithJVM: since if
	the latter answers true, the reciever will be loaded by the main JVM directly rather than
	via a custom classloader, and so no classloaders that are created subsequently will
	ever have a chance to load (and hence control) that class"

	^ self isWrapperClass and: [self supplementaryClassloaderNames includes: aSupplementaryClassloader name].!

supplementaryClassloaderNames
	"answer the names of any 'local classpath' entry that we wish to register
	with in preference to being registered globablly as the JVM starts up.
	See the class comment for SupplementaryClassloaderTree for an
	explanation of why we might want to do that"

	"by default we do not wish to do so"
	^ #().! !
!JavaObject class categoriesFor: #clearGhostsOwnedBy:!ghost classes!managed objects!private! !
!JavaObject class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaObject class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaObject class categoriesFor: #generatedSelectors!constants!listing wrapper methods!public! !
!JavaObject class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaObject class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaObject class categoriesFor: #icon!constants!ghost classes!public! !
!JavaObject class categoriesFor: #instancesOwnedBy:!managed objects!public! !
!JavaObject class categoriesFor: #isWrapperClass!public!registering wrapper classes!testing! !
!JavaObject class categoriesFor: #registerWith:!public!registering wrapper classes! !
!JavaObject class categoriesFor: #registerWithJVM:!public!registering wrapper classes! !
!JavaObject class categoriesFor: #registerWithSupplementaryClassloader:!public!registering wrapper classes! !
!JavaObject class categoriesFor: #registerWrapperClassesWithJVM:!public!registering wrapper classes! !
!JavaObject class categoriesFor: #registerWrapperClassesWithSupplementaryClassloader:!public!registering wrapper classes! !
!JavaObject class categoriesFor: #shouldRegisterWithJVM:!public!registering wrapper classes!testing! !
!JavaObject class categoriesFor: #shouldRegisterWithSupplementaryClassloader:!public!registering wrapper classes!testing! !
!JavaObject class categoriesFor: #supplementaryClassloaderNames!constants!public!registering wrapper classes! !

JavaObjectRegistry guid: (GUID fromString: '{0C4F3622-CB09-42F5-82EC-7EC9EAD6CFD7}')!
JavaObjectRegistry comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These are used by class statics (JavaClassStatic) to hold their collections of canonical instances.  These are essentially just weak sets that are specialised to hold just one JavaClassInstance for any given underlying Java object.'!
!JavaObjectRegistry categoriesForClass!Unclassified! !
!JavaObjectRegistry methodsFor!

add: aJavaObject
	"add a java object to the registry, replacing the existing incumbent, if any"

	sharedMutex critical: [registry
					remove: aJavaObject ifAbsent: [];
					add: aJavaObject].!

averageProbesPerElement
	"answer the <Float> average number of probes to find an item already in the registry"

	^ sharedMutex critical: [registry averageProbesPerElement].!

basicFindOrRelease: aJNIObject ifAbsent: a0Block
	"private -- look for an entry in the registry that refers to the same Java object as
	aJNIObject, if one exists then release aJNIObject and answer the incumbent.
	Otherwise answer the result of evaluating a0Block.  This is the guts of the
	implementation of #findOrRelease:ifAbsent:"

	| previous |

	previous := registry find: aJNIObject ifAbsent: [nil].
	previous isNil
		ifTrue: [previous := a0Block value]
		ifFalse: [aJNIObject releaseRef: jvm jniEnv].

	^ previous.

!

basicFindOrRelease: aJNIObject ifAbsentPut: a0Block
	"private -- look for an entry in the registry that refers to the same Java object as
	aJNIObject, if one exists then release aJNIObject and answer the incumbent.
	Otherwise add and answer the result of evaluating a0Block.  This is the guts of the
	implementation of #findOrRelease:ifAbsentPut:"

	| previous |

	previous := registry find: aJNIObject ifAbsent: [nil].
	previous isNil
		ifTrue: [registry add: (previous := a0Block value)]
		ifFalse: [aJNIObject releaseRef: jvm jniEnv].

	^ previous.

!

compare: aJavaObjectOrJNIObject with: anotherJavaObjectOrJNIObject
	"private -- necessary part of the <SearchPolicy> protocol which allows us to
	to act as our PluggableSet's search policy"

	^ jvm is: aJavaObjectOrJNIObject identicalTo: anotherJavaObjectOrJNIObject.!

contents
	"answer an OrderedCollection with the same contents as our collection"

	^ sharedMutex critical: [^ OrderedCollection withAll: registry].!

findOrRelease: aJNIObject ifAbsent: a0Block
	"look for an entry in the registry that refers to the same Java object as
	aJNIObject, if one exists then release aJNIObject and answer the incumbent.
	Otherwise answer the result of evaluating a0Block.  Note that the whole
	method is executed under our shared mutex, and so is atomic"

	^ sharedMutex critical: [self basicFindOrRelease: aJNIObject ifAbsent: a0Block].!

findOrRelease: aJNIObject ifAbsentPut: a0Block
	"look for an entry in the registry that refers to the same Java object as
	aJNIObject, if one exists then release aJNIObject and answer the incumbent.
	Otherwise add and answer the result of evaluating a0Block.  Note that the whole
	method is executed under our shared mutex, and so is atomic"

	^ sharedMutex critical: [self basicFindOrRelease: aJNIObject ifAbsentPut: a0Block].!

hash: aJavaObjectOrJNIObject max: anInteger
	"private -- necessary part of the <SearchPolicy> protocol which allows us to
	to act as our PluggableSet's search policy"

	^ jvm isLive
		ifTrue: [(jvm identityHash: aJavaObjectOrJNIObject) \\ anInteger + 1]
		ifFalse: [1].!

includes: aJavaObject
	"answer whether the registry includes aJavaObject"

	^ sharedMutex critical: [registry includes: aJavaObject].!

initialize: aCapacity
	"private -- establish a coherent initial state"

	registry := self makeRegistry: aCapacity.!

isDead
	"answer whether we are associated with a dead JVM"

	^ jvm isDead.!

isEmpty
	"answer whether we have no instances registered"

	^ self tableSize = 0.!

isLive
	"answer whether we are associated with a live JVM"

	^ jvm isLive.!

jvm: aJVM
	"private -- set the JVM and shared Mutex we will use.
	We pick up the mutex from the JVM in order to avoid deadlocks"

	jvm := aJVM.
	sharedMutex := aJVM sharedMutex.
!

makeRegistry: aCapacity
	"private -- answer a new registry table of the stated capacity"

	^ self class registryContainerClass new: aCapacity searchPolicy: self.!

notEmpty
	"answer whether we have any instances registered"

	^ self tableSize > 0.!

purge: aJavaObject
	"ensure that the given object is no longer in the registry"

	sharedMutex critical: [registry remove: aJavaObject ifAbsent: []].
!

select: a1Block
	"answer a collection of the entries that satisfy the given condition"

	^ self contents select: a1Block.!

tableLoad
	"answer the <Float> percentage load on the registry"

	^ sharedMutex critical: [registry size * 100.0 / registry basicSize].!

tableSize
	"answer how many classes have been registered"

	^ sharedMutex critical: [registry size].! !
!JavaObjectRegistry categoriesFor: #add:!adding!public! !
!JavaObjectRegistry categoriesFor: #averageProbesPerElement!measuring!public! !
!JavaObjectRegistry categoriesFor: #basicFindOrRelease:ifAbsent:!accessing!private! !
!JavaObjectRegistry categoriesFor: #basicFindOrRelease:ifAbsentPut:!accessing!private! !
!JavaObjectRegistry categoriesFor: #compare:with:!comparing!private! !
!JavaObjectRegistry categoriesFor: #contents!accessing!public! !
!JavaObjectRegistry categoriesFor: #findOrRelease:ifAbsent:!accessing!public! !
!JavaObjectRegistry categoriesFor: #findOrRelease:ifAbsentPut:!accessing!public! !
!JavaObjectRegistry categoriesFor: #hash:max:!comparing!private! !
!JavaObjectRegistry categoriesFor: #includes:!public!searching! !
!JavaObjectRegistry categoriesFor: #initialize:!initializing!private! !
!JavaObjectRegistry categoriesFor: #isDead!public!testing! !
!JavaObjectRegistry categoriesFor: #isEmpty!public!testing! !
!JavaObjectRegistry categoriesFor: #isLive!public!testing! !
!JavaObjectRegistry categoriesFor: #jvm:!initializing!private! !
!JavaObjectRegistry categoriesFor: #makeRegistry:!helpers!private! !
!JavaObjectRegistry categoriesFor: #notEmpty!public!testing! !
!JavaObjectRegistry categoriesFor: #purge:!public!removing! !
!JavaObjectRegistry categoriesFor: #select:!enumerating!public! !
!JavaObjectRegistry categoriesFor: #tableLoad!measuring!public! !
!JavaObjectRegistry categoriesFor: #tableSize!accessing!measuring!public! !

!JavaObjectRegistry class methodsFor!

defaultCapacity
	"private -- answer the default capacity to use for instances"

	^ 10.!

new
	"private -- use #newWithJVM:mutex:"

	^ (super new)
		initialize: self defaultCapacity;
		yourself.
!

new: aCapacity
	"private -- use #new:withJVM:mutex"

	^ (super new)
		initialize: aCapacity;
		yourself.!

new: aCapacity withJVM: aJVM
	"answer a new instance which uses aJVM to determine object hashes, etc"

	^ (self new: aCapacity)
		jvm: aJVM;
		yourself.!

newWithJVM: aJVM
	"answer a new instance which uses aJVM to determine object hashes, etc"

	^ (self new)
		jvm: aJVM;
		yourself.!

registryContainerClass
	"private -- answer the kind of container used to hold elements"

	^ PluggableWeakSet.! !
!JavaObjectRegistry class categoriesFor: #defaultCapacity!constants!private! !
!JavaObjectRegistry class categoriesFor: #new!instance creation!private! !
!JavaObjectRegistry class categoriesFor: #new:!instance creation!private! !
!JavaObjectRegistry class categoriesFor: #new:withJVM:!instance creation!public! !
!JavaObjectRegistry class categoriesFor: #newWithJVM:!instance creation!public! !
!JavaObjectRegistry class categoriesFor: #registryContainerClass!constants!private! !

JVM guid: (GUID fromString: '{6D7B3902-A66B-4FE5-AF2D-1BF192D9EBEB}')!
JVM comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

These act as the central point of the Java stuff.  Essentially instances act as hubs through which the various co-operating objects can find each other.  In particular instances are the entry-point through which newly created references to Java objects are wrapped in Smalltalk objects of the correct class.

Also contains the initialisation code, and some housekeeping.

Note that some of the logic of this class has been forcibly factored out into two subclasses. There is no OO justification for doing this, it just allows me to split out callback handling into a separate package.  Also the choice of concrete subclass is used as the means to configure whether callbacks are supported or not.  Note that all three classes were designed together and althogh physically separable, are not logically so.  Also note that the split into subclasses is not to my taste, so they may well end up merged together again in a later version of JNIPort.'!
!JVM categoriesForClass!Unclassified! !
!JVM methodsFor!

addJVMHookOptionsTo: aJVMInitArgs
	"private -- add options to aJVMInitArgs which will allow us to handle the defined JVM hooks"

	| jniPortSettings hook |

	jniPortSettings := settings jniPortSettings.

	jniPortSettings useExitHook ifTrue:
		[hook := self makeExitHook.
		self rememberExternalCallback: hook.
		jniHelper isNil ifFalse: [hook := jniHelper redirectedExitCallback: hook].
		aJVMInitArgs addExitHook: hook].

	jniPortSettings useAbortHook ifTrue:
		[hook := self makeAbortHook.
		self rememberExternalCallback: hook.
		jniHelper isNil ifFalse: [hook := jniHelper redirectedAbortCallback: hook].
		aJVMInitArgs addAbortHook: hook].

	jniPortSettings useVFPrintfHook ifTrue:
		[hook := self makeVFPrintfHook.
		self rememberExternalCallback: hook.
		jniHelper isNil ifFalse: [hook := (jniPortSettings useVFPrintfRedirection
							ifTrue: [jniHelper systemDebugVFPrintfCallback]
							ifFalse: [jniHelper redirectedVFPrintfCallback: hook])].
		aJVMInitArgs addVFPrintfHook: hook].!

addWatcher: aJVMWatcher
	"ensure that aJVMWatcher is on our list of watchers"

	(watchers identityIncludes: aJVMWatcher) ifFalse:
		[watchers add: aJVMWatcher.
		aJVMWatcher onWatchingJvm: self].!

arrangeForCleanup
	"private -- called during initialisation.  Arrange to be able to clean up adequately"

	SessionManager current when: #sessionStopped send: #onExit to: self.
	SessionManager current when: #sessionStarted send: #onStartup to: self.
	self beFinalizable.
!

asCallbackDo: a0Block
	"answer the result of evaluating a0Block in an environment where we have
	replaced the current JNIEnv with the currently valid one"

	self subclassResponsibility.
!

basicDisown: aJavaInstance
	"private -- clean up after aJavaInstance has no further role to play in the world.
	This is the implementation of #disown: except that it is called under the protection
	of our shared mutex"

	objectsReleased := objectsReleased + 1.

	aJavaInstance isGlobalRef
		ifTrue: [globalCount := globalCount - 1]
		ifFalse: [localCount := localCount - 1].
!

basicNotifyNowGlobal: aJavaInstance
	"private -- aJavaInstance has just become a global ref, adjust the counts accordingly.
	This is the implementation of #notifyNowGlobal: except that it is called under the protection
	of our shared mutex"

	globalCount := globalCount + 1.
	localCount := localCount - 1.
!

basicNotifyNowLocal: aJavaInstance
	"private -- aJavaInstance has just become a global ref, adjust the counts accordingly.
	This is the implementation of #notifyNowLocal: except that it is called under the protection
	of our shared mutex"

	globalCount := globalCount - 1.
	localCount := localCount + 1.
	localCount >= localCapacity ifTrue: [self increaseLocalCapacity].
!

basicOwn: aJavaInstance
	"private -- aJavaInstance has just been born, keep track of it from now on.
	This is the implementation of #own: except that it is called under the protection
	of our shared mutex"

	"I'm a little concerned that after 4 months of creating 100 objects a second, this figure will
	start to require BigInteger manipulation; however Dolphin's BigIntegers are pretty quick so it'll
	probably be OK..."
	objectsCreated := objectsCreated + 1.

	aJavaInstance isGlobalRef
		ifTrue:
			[globalCount := globalCount + 1]
		ifFalse:
			[localCount := localCount + 1.
			localCount >= localCapacity ifTrue: [self increaseLocalCapacity]].!

basicShutdown: aBool
	"private -- shutdown the JVM and release any resources it holds.  aBool tells us whether we were apparently
	still alive when we entered this code.

	Note that this can be called in several ways:
	-	explicity by client code (via #shutdown).
	-	implicitly via finalization (via #shutdown).
	-	implictly at Dolphin system startup (via #die).
	-	implicitly as a result of the JVM telling us that it has closed  (via #die).

	See #initializeFrom: for the sequence of initialisation steps that we have to unwind.

	This code is executed as a critical section protected by our mutex.

	Note that the callback registry #shutdownJava happens in our caller, if at all (since we don't want to
	be talking to the Java world from inside our mutex)"

	"we don't need to be told to clean up anymore"
	self beUnfinalizable.
	SessionManager current removeEventsTriggeredFor: self.

	"discard our local object refs and clean up with JNI"
	aBool
		ifTrue:
			[JavaObject clearGhostsOwnedBy: self.
			JavaClassInstance freeInstancesOwnedBy: self.
			rawJavaLangSystem notNull ifTrue: [rawJavaLangSystem releaseRef: jniEnv].
			rawJavaLangClass notNull ifTrue: [rawJavaLangClass releaseRef: jniEnv].
			rawJavaLangObject notNull ifTrue: [rawJavaLangObject releaseRef: jniEnv].
			self javaVM destroy]
		ifFalse:
			[JavaClassInstance killInstancesOwnedBy: self].
	
	"kill our refs to a now non-operative library,  JNIEnv, etc"
	library := javaVM := nil.
	jniEnv := DeadJNIEnv new.
	jniHelper := nil.

	"we can't be entirely certain that the JNI library won't use the callbacks
	we set (threads and race conditions), so we don't dare discard them"
	"externalCallbacks := nil."

	"don't want the watchers anymore"
	watchers := nil.

	"we close down the registries, but keep handles to them"
	supplementaryClassloaders notNil ifTrue: [supplementaryClassloaders shutdown].
	classIndex notNil ifTrue: [classIndex shutdown].
	classRegistry notNil ifTrue: [classRegistry shutdown].
	self shutdownCallbackRegistry.

	"discard our handles on various Java objects"
	rawJavaLangSystem
		:= rawJavaLangClass
		:= rawJavaLangObject
		:= isArrayMethodID
		:= isInterfaceMethodID
		:= identityHashCodeMethodID
		:= hashCodeMethodID
		:= equalsMethodID
			:= nil.

	"the system keeps a cache of compilation results, which can result in
	old references to (with all our resulting machinery) being kept around for
	much longer than you'd expect.  This is a brutal, but
	effective way of stopping it"
	CompiledCode initializeInfoCache.!

callbackDepth
	"answer how deeply nested we are in callbacks"

	self subclassResponsibility.
!

callbackRegistry
	"answer our registry of callbacks or nil if we don't support callbacks"

	self subclassResponsibility.!

callbacks
	"answer how many callbacks have been serviced"

	self subclassResponsibility.
!

classIndex
	"answer our index of known-by-name classes"

	^ classIndex.!

classloader
	"alias provided so that we can act more polymorphically with other <javaClassFinder>s"

	^ self systemClassloader.!

classRegistry
	"answer our registry of known classes"

	^ classRegistry.!

defineClass: aString fromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a Class Static corresponding to the newly defined class.
	Will throw an error if the class definition does not define a class named by
	aString (which can be in JNI or Java format)."

	| answer |

	answer := self
			defineClassObject: aString asJavaQuasiUTF8EncodedString
			fromBytes: aByteArray
			classloader: nil.

	^ answer ifNotNil: [:it | it classStatic].!

defineClass: aString fromBytes: aByteArray classloader: aJavaClassLoader
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a class Static corresponding to the newly defined class, which will have
	aJavaClassLoader as its class loader.
	Will throw an error if the class definition does not define a class named by
	aString (which can be in JNI or Java format)."

	| answer |

	answer := self
			defineClassObject: aString asJavaQuasiUTF8EncodedString
			fromBytes: aByteArray
			classloader: aJavaClassLoader.

	^ answer ifNotNil: [:it | it classStatic].!

defineClass: aString fromBytes: aByteArray classLoader: aJavaClassLoader

	"deprecated in favour of a more consistant naming convention"
	Notification deprecated.
	^ self defineClass: aString fromBytes: aByteArray classloader: aJavaClassLoader.!

defineClassFromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	defining the class named by aJNIClassName.
	Answers a class Static corresponding to the newly defined class"

	| answer |

	answer := self
			defineClassObject: nil
			fromBytes: aByteArray
			classloader: nil.

	^ answer ifNotNil: [:it | it classStatic].
!

defineClassFromBytes: aByteArray classloader: aJavaClassLoader
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a class Static corresponding to the newly defined class, which will have
	aJavaClassLoader as its class loader"

	| answer |

	answer := self
			defineClassObject: nil
			fromBytes: aByteArray
			classloader: aJavaClassLoader.

	^ answer ifNotNil: [:it | it classStatic].!

defineClassFromBytes: aByteArray classLoader: aJavaClassLoader

	"deprecated in favour of a more consistant naming convention"
	Notification deprecated.
	^ self defineClassFromBytes: aByteArray classloader: aJavaClassLoader.!

defineClassObject: aJNIClassName fromBytes: aByteArray classloader: aJavaClassLoader
	"private -- passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	defining the class named by aJNIClassName.
	Answers a JavaLangClass corresponding to the newly defined class.
	The name may be left nil, in which case the JVM will not check the supplied name against
	that in the definition.
	If the JavaObject aJavaClassLoader is not nil, then it is expect to be a Java object
	of a subclass of java.lang.ClassLoader, and it will be assigned as the classloader of the new class,
	otherwise the JVM will supply a default (possibly nil -- I can't find any doc for this)"

	| answer |

	answer := jniEnv
			DefineClass_name: aJNIClassName asParameter
			loader: aJavaClassLoader asParameter
			buf: aByteArray
			len: aByteArray size
			onException: [:jex | self throwJavaException: jex].

	^ answer asJavaObject: self.
!

die
	"tell the JVM that it's underlying Java VM has already been shutdown, and so that
	it should clean up.

	Note that this can be called in several ways:
	-	explicity by client code (though it's a bad idea unless you know something has gone pear-shaped).
	-	implicitly as a result of the JVM telling us that it has closed.
	-	implictly at Dolphin system startup.

	The difference between this and #shutdown, is that this is called when there is reason
	to expect that the JNI library is already dead, whereas #shutdown is called when we think
	it is still in working order"

	"NB: there's a hole here.  Any objects which are finalised between the point where the JNI library actually
	stopped working, and the time we enter this mutex, will try to #free themselves, and will fail in whatever
	manner happens to appeal to the JNI implementation.  There's nothing we can do to stop that"

	sharedMutex critical:
		[self basicShutdown: false].

	self notifyDead.!

disown: aJavaInstance
	"private -- clean up after aJavaInstance has no further role to play in the world"

	sharedMutex critical: [self basicDisown: aJavaInstance].!

equalityHash: aJavaObjectOrJNIObject
	"answer the aJNIObjectOrJavaObject's Java hashCode().
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up"

	| answer |

	answer := jniEnv
			CallIntMethodA_obj: aJavaObjectOrJNIObject asParameter
			methodID: hashCodeMethodID
			args: nil.

	jniEnv checkForException ifTrue: [jniEnv exceptionDo: [:jex | ^ self throwJavaException: jex]].

	^ answer.!

finalize
	"private -- we are no longer referenced, so shutdown"

	self isLive ifTrue: [self shutdown].!

findClass: aString
	"answer a JavaStatic object corresponding to aString -- note that this does
	not allow lookup in user-installed classloaders"

	"this will bounce back to our own #findClassObject: if the index doesn't have it already"
	^ classIndex findClass: aString.!

findClass: aString in: aStringOrSymbol
	"answer a JavaStatic object corresponding to aString, loaded
	via the named supplementary classloader entry (i.e. ultimately by a specially created
	Java classloader).
	Will throw an exceptioin if we are not using an enabled supplementary classloader
	with the given name"

	^ supplementaryClassloaders findClass: aString in: aStringOrSymbol.!

findClassFor: aJNIObject
	"private -- find/make the JavaStatic that will wrap aJNIObject"

	^ (self wrapJNIClass: (jniEnv GetObjectClass_obj: aJNIObject)) classStatic.!

findClassObject: aJNIClassName
	"answer a JavaLangClass object corresponding to aJNIClassName -- note that this does
	not allow lookup in user-installed classloaders"

	| answer |

	answer := jniEnv
			FindClass_name: aJNIClassName asJavaQuasiUTF8EncodedString asParameter
			onException: [:jex | self throwJavaException: jex].

	^ answer asJavaObject: self.
!

findJNIPortClass: aString
	"private -- answer a JavaStatic object corresponding to aString.
	This is used in preference to findClass: for looking up classes that are internal to the
	JNIPort implementation.
	Currently there is no real difference, but someday I plan to switch to loading the JNIPort
	internall classes via a supplementary classloader"

	^ self findClass: aString.!

findSystemClasses
	"private -- called during initialisation.  Pre-locate the JNIClass object we need to before we can start registering
	classes"

	| class |

	#CUtodo.  "what the hell are we supposed to do to signal these potential errors -- we can't even trace them...?"

	class := jniEnv FindClass_name: 'java/lang/Object'.
	rawJavaLangObject := class getGlobalRef: jniEnv onException: [:ex | self assert: [false]].
	class releaseRef: jniEnv.
	self assert: [rawJavaLangObject notNull].

	class := jniEnv FindClass_name: 'java/lang/Class'.
	rawJavaLangClass := class getGlobalRef: jniEnv onException: [:ex | self assert: [false]].
	jniEnv DeleteLocalRef_obj: class.
	self assert: [rawJavaLangClass notNull].

	class := jniEnv FindClass_name: 'java/lang/System'.
	rawJavaLangSystem := class getGlobalRef: jniEnv onException: [:ex | self assert: [false]].
	jniEnv DeleteLocalRef_obj: class.
	self assert: [rawJavaLangSystem notNull].!

findSystemMethods
	"private -- called during initialisation.  Pre-locate the MethodIDs we need to before we can start registering
	classes"

	isArrayMethodID := jniEnv
					GetMethodID_class: rawJavaLangClass
					name: 'isArray'
					sig: '()Z'.
	self assert: [isArrayMethodID notNull].

	isInterfaceMethodID := jniEnv
					GetMethodID_class: rawJavaLangClass
					name: 'isInterface'
					sig: '()Z'.
	self assert: [isInterfaceMethodID notNull].

	identityHashCodeMethodID := jniEnv
					GetStaticMethodID_class: rawJavaLangSystem
					name: 'identityHashCode'
					sig: '(Ljava/lang/Object;)I'.
	self assert: [identityHashCodeMethodID notNull].

	"from this point on, the #isArrayClass:, #isInterfaceClass:, and #jniObjectIdentityHash: methods will work,
	which are prerequisites for class registration"

	equalsMethodID := jniEnv
				GetMethodID_class: rawJavaLangObject
				name: 'equals'
				sig: '(Ljava/lang/Object;)Z'.
	self assert: [equalsMethodID notNull].

	hashCodeMethodID := jniEnv
					GetMethodID_class: rawJavaLangObject
					name: 'hashCode'
					sig: '()I'.
	self assert: [hashCodeMethodID notNull].

	"this is slightly hacky, but it's much simpler that checking for the existence ot
	the reflective methods we are /really/ interested in"
	hasJava5Extensions := (jniEnv
					GetMethodID_class: rawJavaLangClass
					name: 'isSynthetic'
					sig: '()Z'
					onException: [:ex | 0 "0 isNull"]) notNull.
!

ghostClassMode
	"answer one of #( #NotUsed #Eager #Lazy ) dependng on what sort of using ghost classes
	we are using"

	self usesLazyGhostClasses ifTrue: [^ #Lazy].
	self usesGhostClasses ifTrue: [^ #Eager].
	^ #NotUsed.
!

globalRefCount
	"answer how many global managed objects our JVM is aware of at this time"

	^ globalCount.!

handleJVMAbort
	"private -- this is our onAbort() hook for the JVM"

	Notification signal: 'JVM abort() -- closing down'.

	self die.

	self trigger: #JVMAbort.

!

handleJVMExit: anInteger
	"private -- this is our onExit() hook for the JVM"

	self die.

	self trigger: #JVMExit: with: anInteger.

!

handleVFPrintf: anExternalAddress format: aString args: aVaList
	"private -- this is our vfprintf() hook for the JVM"

	| string |

	"NB: some JNI libraries trigger this for *each character* of text to be output, don't
	assume that you'll see a whole message in one go"

	string := aString sprintfWithArguments: aVaList.
	self
		trigger: #JVMMessage:address:
		with: string
		with: anExternalAddress.

	^ string size.
!

hasJava5Extensions
	"answer whether we think our Java runtime has the Java 5 extensions.
	(this is needed for ghost method generation, specifically the JavaLangReflectMethod>>isBridge
	test)"

	^ hasJava5Extensions!

identityHash: aJavaObjectOrJNIObject
	"answer the aJNIObjectOrJavaObject's Java identityHashCode().
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up"

	| args answer |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aJavaObjectOrJNIObject asParameter;
			yourself.

	answer := jniEnv
			CallStaticIntMethodA_class: rawJavaLangSystem
			methodID: identityHashCodeMethodID
			args: args.

	jniEnv checkForException ifTrue: [jniEnv exceptionDo: [:jex | ^ self throwJavaException: jex]].

	^ answer.!

increaseLocalCapacity
	"private -- ask the JNI library to increase the amount of local reference space it allows us.
	NB: only called under the protection of our shared mutex"

	localCapacity := localCapacity + self localCapacityIncrement.
	jniEnv
		EnsureLocalCapacity_capacity: localCapacity + self localCapacitySlop
		onException: [:jex | self throwJavaException: jex].


!

initialize
	"private -- establish a coherent initial state"

	jniEnv := DeadJNIEnv new.
	status := #Dead.
	sharedMutex := Mutex new.
	externalCallbacks := OrderedCollection new.
!

initializeCallbackRegistry
	"private -- called during initialisation.  We are now sufficiently initialized for a callback registry
	to work; if we want one then set one up now"

	self subclassResponsibility.!

initializeClasses
	"private -- called during initialisation.  Pre-locate any system classes and methods we need to operate"

	self findSystemClasses.
	self findSystemMethods.
!

initializeClassIndex
	"private -- called during initialisation.  We are now sufficiently initialized for a class index to work;
	set one up now"

	classIndex := JavaMainClassIndex newFor: self.!

initializeClassRegistry
	"private -- called during initialisation.  We are now sufficiently initialized for a class registry to work;
	set one up now"

	classRegistry := JavaClassRegistry newWithJVM: self.

	"this must not be called until after the assignment to classRegistry"
	classRegistry registerSystemClasses.
!

initializeFromSettings: aJVMSettings
	"private -- initialise ourselves from the given JVMSettings"

	settings :=  aJVMSettings deepCopy.	"we don't want settings changed on us after we've started running"

	self
		initializeObjectCounts;
		initializeLibrary;
		initializeName;
		initializeJNIHelper;
		initializeJNI;
		initializeLocalCapacity;
		initializeClasses;
		initializeClassRegistry;
		initializeClassIndex;
		initializeCallbackRegistry;
		notifyBorn;		"*after* we're mostly ready"
		initializeWatchers;
		initializeWrapperClasses;
		initializeSupplementaryClassloaders;
		arrangeForCleanup;
		notifyLive.
!

initializeJNI
	"private -- initialise ours JNI structures"

	| javaVMInitArgs |

	javaVMInitArgs :=  settings runtimeSettings javaVMInitArgs.
	self addJVMHookOptionsTo: javaVMInitArgs.

	jniEnv := library createFirstJNIEnv: javaVMInitArgs.
	javaVM := jniEnv javaVM.

	"check we're running on a late enough version of JNI"
	self assert: [jniEnv GetVersion >= JNI_VERSION_1_2].
!

initializeJNIHelper
	"private -- allocate ourselves new JNIHelper, if we are configured to use one"

	jniHelper := nil.

	settings jniPortSettings useJNIHelperLibrary ifFalse: [^ self].

	"use Smalltalk to avoid creating a package dependency"
	jniHelper := (Smalltalk at: #DolphinJNIHelperLibrary) default makeJNIHelper.!

initializeLibrary
	"private -- initialise our JNI library"

	library := settings jniPortSettings jniLibrary.
!

initializeLocalCapacity
	"private -- ensure we've got some initial local capacity (must be at least large enough for us to complete
	basic initialisation before it has to be expanded)"

	| answer |

	localCapacity := self initialLocalCapacity.
	answer := jniEnv EnsureLocalCapacity_capacity: localCapacity.
	self assert: [answer = 0].
!

initializeName
	"private -- initialise our name"

	name := self makeUniqueName: (settings name isNil ifTrue: [library name] ifFalse: [settings name]).
!

initializeObjectCounts
	"private -- initialise ours tallies of object references"

	localCount := globalCount := objectsCreated := objectsReleased := 0.
!

initializeSupplementaryClassloaders
	"private -- called during initialisation.  We now have set up the global
	wrapper classes (the ones that register directly with the JVM) so it's now
	time to set up any supplementary classloaders"

	self useSupplementaryClassloaders: (settings supplementaryClassloaders useAtStartup).!

initializeWatchers
	"private -- initialise our watcher list"

	watchers := OrderedCollection new.
	settings jniPortSettings watcherClasses do: [:each | each onJvmStartup: self].
!

initializeWrapperClasses
	"private -- called during initialisation.  We now have both a class registry and class index, so
	get classes to register themselves as the 'preferred' wrappers"

	JavaObject registerWrapperClassesWithJVM: self.
!

initialLocalCapacity
	"private -- answer how many slots of local references we will demand of the JVM"

	"must be at least big enough for us to bootstrap the system without calling #increaseLocalCapacity"
	^ 100.	"the JNI default is only 16 !!"!

is: aJavaObjectOrJNIObject equalTo: anotherJavaObjectOrJNIObject
	"answer whether the two Java objects are equal in the sense of the java.lang.Object.equals() method.
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up.
	Note also, that since we are calling a method on the first object it cannot be nil, the second object can"

	| args answer |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anotherJavaObjectOrJNIObject asParameter;
			yourself.

	answer := jniEnv
			CallBooleanMethodA_obj: aJavaObjectOrJNIObject asParameter
			methodID: equalsMethodID
			args: args.

	jniEnv checkForException ifTrue: [jniEnv exceptionDo: [:jex | ^ self throwJavaException: jex]].

	^ answer == 1.
!

is: aJavaObjectOrJNIObject identicalTo: anotherJavaObjectOrJNIObject
	"answer whether the two Java objects are identical.
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up"

	^ (jniEnv
		IsSameObject_obj: aJavaObjectOrJNIObject asParameter
		obj: anotherJavaObjectOrJNIObject asParameter) == 1.!

isArrayClass: aJNIClass
	"answer true if aJNIClass stands for an array class.
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up -- indeed it is a needed by that machinery"

	| answer |

	answer := jniEnv
			CallBooleanMethodA_obj: aJNIClass
			methodID: isArrayMethodID
			args: nil.

	jniEnv checkForException ifTrue: [jniEnv exceptionDo: [:jex | ^ self throwJavaException: jex]].

	^ answer == 1.
!

isDead
	"answer whether this instance no longer represents a live connection to a Java Virtual Machine"

	^ jniEnv isDead.!

isInterfaceClass: aJNIClass
	"answer true if aJNIClass stands for an interface class.
	Note that this method is optimised, and does *not* require the managed object
	machinery (javaClass etc) to be set up -- indeed it is a needed by that machinery"

	| answer |

	answer := jniEnv
			CallBooleanMethodA_obj: aJNIClass
			methodID: isInterfaceMethodID
			args: nil.

	jniEnv checkForException ifTrue: [jniEnv exceptionDo: [:jex | ^ self throwJavaException: jex]].

	^ answer == 1.
!

isLive
	"answer whether this instance still represents a live connection to a Java Virtual Machine"

	^ jniEnv isDead not.!

isRunning
	"answer whether we are in our normal running state
	(which is not the same as #isLive since that starts being true early-on
	in our initialisation sequence, whereas this doesn't become true until
	we are fully initialised)"

	^ status = #Running.!

javaLangSystem
	"answer the class static for java.lang.System"

	^ self findClass: #'java.lang.System'.!

javaRuntime
	"answer a/the java.lang.Runtime object for this JVM"

	^ (self findClass: #'java.lang.Runtime') callObjectMethod: 'getRuntime' signature: '()Ljava/lang/Runtime;'.!

javaVM
	"answer the receiver's JavaVM"

	^ jniEnv javaVM.!

jniEnv
	"answer the receiver's currently active jniEnv (a JNIEnv)"

	^ jniEnv.
!

jniHelper
	"answer our jniHelper if we have one"

	^ jniHelper.!

jvm
	"implemented so #jvm can be part of the <javaClassFinder> and  <javaWrapperFactory>
	protocols"

	^ self.!

library
	"answer which JavaRuntimeLibrary created us"

	^ library.
!

localCapacityIncrement
	"private -- answer how many more slots of local references we will ask for if we
	discover that we're runnig out"

	^ 50.!

localCapacitySlop
	"private -- answer how many extra slots of local references we ask for in addition
	to our formal capacity"

	^ 10.!

localPoolCapacity
	"answer the amount of space we have pre-allocated in the local reference pool"

	^ localCapacity.!

localRefCount
	"answer how many local managed objects our JVM is aware of at this time"

	^ localCount.!

localsPoolCapacity
	"answer the amount of space we have pre-allocated in the local reference pool"

	^ localCapacity.!

makeAbortHook
	"private -- answer a new ExternalCallback suitable for use as an abort handler"

	^ ExternalCallback
		block: [self handleJVMAbort. nil]
		descriptor: (ExternalDescriptor fromString: 'stdcall: void').
!

makeExitHook
	"private -- answer a new ExternalCallback suitable for use as an exit handler"

	^ ExternalCallback
		block: [:code | self handleJVMExit: code. nil]
		descriptor: (ExternalDescriptor fromString: 'stdcall: void sdword').
!

makeUniqueName: aString
	"private -- answer a unique name based on aString"

	| i |

	(self class runningInstances anySatisfy: [:each | each name = aString]) ifFalse: [^ aString].

	i := 1.
	[| candidate |
	candidate := (String writeStream)
				nextPutAll: aString;
				nextPutAll: ' (';
				display: (i := i + 1);
				nextPut: $);
				contents.
	(self class runningInstances anySatisfy: [:each | each name = candidate]) ifFalse: [^ candidate]]
		repeat.
!

makeVFPrintfHook
	"private -- answer a new ExternalCallback suitable for use as an vfprintf handler"

	^ ExternalCallback
		block: [:fp :format :args | self handleVFPrintf: fp format: format args: args]
		descriptor: (ExternalDescriptor fromString: 'stdcall: sdword void* char* VaList*').
!

name
	"answer the name of this instance"

	^ name.!

name: aString
	"set the name of this instance"

	name := aString.!

notifyBorn
	"private -- tell anyone who is interested that we have been born"

	status := #Initializing.
	self class addRunningInstance: self.
	self trigger: #JVMIsBorn.
!

notifyClassPurged: aJavaStatic
	"tell any Watchers that are interested that a class is about to be purged from out registry"

	watchers do: [:each | each onClassPurged: aJavaStatic].!

notifyClassRegistered: aJavaStatic
	"tell any Watchers that are interested that we have registered a new class"

	watchers do: [:each | each onClassRegistered: aJavaStatic].!

notifyDead
	"private -- tell anyone who is interested that we have died"

	status := #Dead.
	self trigger: #JVMIsDead.
	self class removeRunningInstance: self.
!

notifyLive
	"private -- tell anyone who is interested that we are now live"

	status := #Running.
	watchers do: [:each | each onJmvInitialized: self].
	self trigger: #JVMIsLive.
!

notifyNowGlobal: aJavaInstance
	"private -- aJavaInstance has just become a global ref, adjust the counts accordingly"

	sharedMutex critical: [self basicNotifyNowGlobal: aJavaInstance].!

notifyNowLocal: aJavaInstance
	"private -- aJavaInstance has just become a global ref, adjust the counts accordingly"

	sharedMutex critical: [self basicNotifyNowLocal: aJavaInstance].!

notifyShutdownStarting
	"private -- tell anyone who is interested that we are about to shut down"

	status := #ShuttingDown.
	self trigger: #JVMShutdownStarting.
!

notifySupplementaryClassloadersChanged
	"private -- tell anyone who is interested that we have a different tree of supplementary
	classloaders (if you need to see changes /to/ that tree then you'll have to register for
	its own change events)"

	#CUtodo.  "is there any value in telling the watchers too ?"
	self trigger: #JVMSupplementaryClassloadersChanged.
!

objectRefCount
	"answer how many managed objects our JVM is aware of at this time"

	^ localCount + globalCount.!

objectsCreated
	"answer how many objects our JVM has ever created"

	^ objectsCreated.!

objectsReleased
	"answer how many objects our JVM ever released"

	^ objectsReleased.!

onExit
	"private -- called as an image exits, clean up gracefully"

	"not really worth the effort, so commented out"
	"self shutdown."
!

onStartup
	"private -- called as an image starts, ensure we know we are in an uninitialised state"

	self die.
!

own: aJavaInstance
	"private -- aJavaInstance has just been born, keep track of it from now on"

	sharedMutex critical: [self basicOwn: aJavaInstance].!

printOn: aStream
	"write a developer oriented representation of the receiver to aStream"

	self isDead
		ifTrue: [aStream display: 'a dead '; display: self class]
		ifFalse: [super printOn: aStream].

	name isNil ifFalse:
		[aStream nextPut: $(; display: name; nextPut: $)].!

rememberExternalCallback: anExternalCallback
	"add anExternalCallback to a collection that we hold to prevent them being GCed"

	externalCallbacks add: anExternalCallback.!

removeWatcher: aJVMWatcher
	"ensure that aJVMWatcher is not on our list of watchers"

	| index |

	index := watchers identityIndexOf: aJVMWatcher.
	index > 0 ifTrue:
		[watchers removeAtIndex: index.
		aJVMWatcher onNotWatchingJvm: self].!

resetRegistries
	"private -- do not call this"

	self isLive ifFalse: [^ self].

	"duplicated from #shutdown:"
	self callbackRegistry ifNotNil: [:it | it shutdownJava].

	"duplicated from #basicShutdown:"
	JavaObject clearGhostsOwnedBy: self.
	JavaClassInstance freeInstancesOwnedBy: self.
	supplementaryClassloaders notNil ifTrue: [supplementaryClassloaders shutdown].
	classIndex notNil ifTrue: [classIndex shutdown].
	classRegistry notNil ifTrue: [classRegistry shutdown].

	"duplicated from #initializeFromSettings:"
	self
		initializeClassRegistry;
		initializeClassIndex;
		initializeCallbackRegistry;
		initializeWatchers;
		initializeWrapperClasses;
		initializeSupplementaryClassloaders.
	watchers do: [:each | each onJmvInitialized: self].

	self trigger: #JVMReset.
!

settings
	"answer the JVMSettings object that we use"

	^ settings.!

shallowCopy
	"overridden since we stand in a singleton-like relationship to the wrapped JNI structures"

	^ self.!

sharedMutex
	"answer the JVM-wide mutex"

	^ sharedMutex.!

shutdown
	"shutdown the JVM and release any resources it holds.

	Note that this can be called in several ways:
	-	explicity by client code.
	-	implicitly via finalization (abnormal case).

	The difference between this and #die, is that #die is called when there is reason
	to expect that the JNI library is already dead, whereas this is called when we think
	it is still in working order"

	self notifyShutdownStarting.

	"give the callback registry -- if any --  a chance to shutdown stuff in the Java space"
	self callbackRegistry ifNotNil: [:it | it shutdownJava].

	watchers do: [:each | each onJvmShutdown: self].

	sharedMutex critical: [self basicShutdown: true].

	self notifyDead.!

shutdownCallbackRegistry
	"private -- shutdown any callback registry we have"

	self subclassResponsibility.
!

status
	"answer one of #(Dead Initializing Running ShuttingDown) to indicate our current status.
	Unfortunately, the status names do not match the names of the events used for status
	change norifications.  Sorry..."

	^ status!

stderr
	"answer the java.lang.System.err PrintStream"

	^ self javaLangSystem err.!

stdin
	"answer the java.lang.System.in InputStream"

	^ self javaLangSystem in.!

stdout
	"answer the java.lang.System.out PrintStream"

	^ self javaLangSystem out.!

supplementaryClassloaderNamed: aStringOrSymbol
	"if we are using supplementary classloaders, then answer the one with the given
	name, throw an exception otherwise"

	^ supplementaryClassloaders
		ifNil: [self errorNotFound: aStringOrSymbol]
		ifNotNil: [:it | it entryNamed: aStringOrSymbol].!

supplementaryClassloaders
	"answer our tree of supplementary classloaders if we are using them, or nil otherwise"

	^ supplementaryClassloaders.!

supportsCallbacks
	"answer whether this JVM supports callbacks from Java into Smalltalk"

	^ self callbackRegistry notNil.!

systemClassloader
	"answer the system java.lang.ClassLoader object for this JVM.
	This is also known as the application classloader, since it is the one used by default
	to load application classes (i.e. on the %CLASSPATH%, or whatever, rather than being
	supplied as an integral part of the Java platform)"

	^ (self findClass: #'java.lang.ClassLoader') getSystemClassLoader_null.!

systemClassLoader

	"deprecated in favour of a more consistant naming convention"
	Notification deprecated.
	^ self systemClassloader.
!

throwJavaException: aJNIThrowable
	"wrap a JNIThrowable object first in a JavaClassInstance and then throw a corresponding
	Smaltalk Exception"

	(aJNIThrowable asJavaObject: self) signal.!

usesGhostClasses
	"answer whether we are using ghost classes"

	^ watchers anySatisfy: [:each | each usesGhostClasses].!

usesLazyGhostClasses
	"answer whether we are using lazy ghost classes"

	^ watchers anySatisfy: [:each | each usesLazyGhostClasses].!

useSupplementaryClassloaders: aBool
	"set whether we should use our collection of supplementary classloaders.
	If this is turned off while we are running then the supplementary classloaders
	will all be purged.  Otherwise it can be turned on while we are running at
	which time any wrapper classes will be given a chance to get themselves
	loaded by the supplementary classloader(s) of their choice"

#CUtodo.  "should we protect the supplementatry classloaders list with our mutex ?"

	self usingSupplementaryClassloaders = aBool ifTrue: [^ self].

	aBool
		ifTrue: [supplementaryClassloaders := SupplementaryClassloaderTree forJVM: self]
		ifFalse: [supplementaryClassloaders purge.  supplementaryClassloaders := nil].

	self notifySupplementaryClassloadersChanged.!

usingSupplementaryClassloaders
	"answer whether we are current using a collection of supplementary classloaders
	(even if this answers true the collection itself may be empty)"

	^ supplementaryClassloaders notNil.!

wrapJNIClass: aJNIClass
	"answer a JavaInstance, owned by the receiver, for the JNI object, aJNIClass
	or nil if it is a null pointer"

	"ask the special class static for classes to create a wrapper object"
	^ classRegistry javaLangClass wrapJNIObject: aJNIClass.
!

wrapJNIObject: aJNIObject
	"answer a JavaInstance, owned by the receiver, for the JNI object, aJNIObject
	or nil if it is a null pointer"

	"this is invoked for new object refs where the Java class is not already known
	before the ref is created, so we just lookup the class and then ask it to wrap
	the object"
	^ aJNIObject isNull
		ifTrue: [nil]
		ifFalse: [(self findClassFor: aJNIObject) wrapJNIObject: aJNIObject].!

wrapJNIString: aJNIObject
	"answer a JavaInstance, owned by the receiver, for the JNI object, aJNIObject
	or nil if it is a null pointer.  This *MUST NEVER* be called with any object that is not, in
	fact, a java.lang.String (specifically the java.lang.String created by the primordial class
	loader)"

	"ask the known class static for java.lang.String to create a wrapper object, this
	saves about half the time it takes to import a String, since we don't need to mess
	around checking its actual class"
	^ classRegistry javaLangString wrapJNIObject: aJNIObject.
! !
!JVM categoriesFor: #addJVMHookOptionsTo:!initializing!JVM hooks!private! !
!JVM categoriesFor: #addWatcher:!events!public! !
!JVM categoriesFor: #arrangeForCleanup!event handling!initializing!private! !
!JVM categoriesFor: #asCallbackDo:!Java callbacks!public! !
!JVM categoriesFor: #basicDisown:!managed objects!private! !
!JVM categoriesFor: #basicNotifyNowGlobal:!managed objects!private! !
!JVM categoriesFor: #basicNotifyNowLocal:!managed objects!private! !
!JVM categoriesFor: #basicOwn:!managed objects!private! !
!JVM categoriesFor: #basicShutdown:!managed objects!private!shutting down! !
!JVM categoriesFor: #callbackDepth!Java callbacks!monitoring!public! !
!JVM categoriesFor: #callbackRegistry!accessing!Java callbacks!public! !
!JVM categoriesFor: #callbacks!Java callbacks!monitoring!public! !
!JVM categoriesFor: #classIndex!accessing!public! !
!JVM categoriesFor: #classloader!accessing!Java classes!public! !
!JVM categoriesFor: #classRegistry!accessing!public! !
!JVM categoriesFor: #defineClass:fromBytes:!Java classes!public! !
!JVM categoriesFor: #defineClass:fromBytes:classloader:!Java classes!public! !
!JVM categoriesFor: #defineClass:fromBytes:classLoader:!Java classes!public! !
!JVM categoriesFor: #defineClassFromBytes:!Java classes!public! !
!JVM categoriesFor: #defineClassFromBytes:classloader:!Java classes!public! !
!JVM categoriesFor: #defineClassFromBytes:classLoader:!Java classes!public! !
!JVM categoriesFor: #defineClassObject:fromBytes:classloader:!Java classes!private! !
!JVM categoriesFor: #die!operations!public!shutting down! !
!JVM categoriesFor: #disown:!managed objects!private! !
!JVM categoriesFor: #equalityHash:!comparing!public! !
!JVM categoriesFor: #finalize!finalizing!private!shutting down! !
!JVM categoriesFor: #findClass:!Java classes!public! !
!JVM categoriesFor: #findClass:in:!Java classes!public!supplementary classloaders! !
!JVM categoriesFor: #findClassFor:!managed objects!private! !
!JVM categoriesFor: #findClassObject:!Java classes!public! !
!JVM categoriesFor: #findJNIPortClass:!Java classes!private!supplementary classloaders! !
!JVM categoriesFor: #findSystemClasses!initializing!private! !
!JVM categoriesFor: #findSystemMethods!initializing!private! !
!JVM categoriesFor: #ghostClassMode!accessing!ghost classes!public! !
!JVM categoriesFor: #globalRefCount!managed objects!monitoring!public! !
!JVM categoriesFor: #handleJVMAbort!event handling!events!JVM hooks!private! !
!JVM categoriesFor: #handleJVMExit:!event handling!events!JVM hooks!private! !
!JVM categoriesFor: #handleVFPrintf:format:args:!event handling!events!JVM hooks!private! !
!JVM categoriesFor: #hasJava5Extensions!public!testing! !
!JVM categoriesFor: #identityHash:!comparing!public! !
!JVM categoriesFor: #increaseLocalCapacity!helpers!locals pool!private! !
!JVM categoriesFor: #initialize!initializing!private! !
!JVM categoriesFor: #initializeCallbackRegistry!initializing!Java callbacks!private! !
!JVM categoriesFor: #initializeClasses!initializing!private! !
!JVM categoriesFor: #initializeClassIndex!initializing!private! !
!JVM categoriesFor: #initializeClassRegistry!initializing!private! !
!JVM categoriesFor: #initializeFromSettings:!initializing!private! !
!JVM categoriesFor: #initializeJNI!initializing!private! !
!JVM categoriesFor: #initializeJNIHelper!initializing!JVM hooks!private! !
!JVM categoriesFor: #initializeLibrary!initializing!private! !
!JVM categoriesFor: #initializeLocalCapacity!initializing!locals pool!private! !
!JVM categoriesFor: #initializeName!initializing!private! !
!JVM categoriesFor: #initializeObjectCounts!initializing!private! !
!JVM categoriesFor: #initializeSupplementaryClassloaders!initializing!private!supplementary classloaders! !
!JVM categoriesFor: #initializeWatchers!events!initializing!private! !
!JVM categoriesFor: #initializeWrapperClasses!initializing!Java classes!private! !
!JVM categoriesFor: #initialLocalCapacity!constants!locals pool!private! !
!JVM categoriesFor: #is:equalTo:!comparing!public! !
!JVM categoriesFor: #is:identicalTo:!comparing!public! !
!JVM categoriesFor: #isArrayClass:!public!testing! !
!JVM categoriesFor: #isDead!public!testing! !
!JVM categoriesFor: #isInterfaceClass:!public!testing! !
!JVM categoriesFor: #isLive!public!testing! !
!JVM categoriesFor: #isRunning!public!testing! !
!JVM categoriesFor: #javaLangSystem!helpers!public! !
!JVM categoriesFor: #javaRuntime!accessing!public! !
!JVM categoriesFor: #javaVM!accessing!public! !
!JVM categoriesFor: #jniEnv!accessing!public! !
!JVM categoriesFor: #jniHelper!accessing!JVM hooks!public! !
!JVM categoriesFor: #jvm!accessing!public! !
!JVM categoriesFor: #library!accessing!public! !
!JVM categoriesFor: #localCapacityIncrement!constants!locals pool!private! !
!JVM categoriesFor: #localCapacitySlop!constants!locals pool!private! !
!JVM categoriesFor: #localPoolCapacity!monitoring!public! !
!JVM categoriesFor: #localRefCount!managed objects!monitoring!public! !
!JVM categoriesFor: #localsPoolCapacity!monitoring!public! !
!JVM categoriesFor: #makeAbortHook!JVM hooks!private! !
!JVM categoriesFor: #makeExitHook!JVM hooks!private! !
!JVM categoriesFor: #makeUniqueName:!helpers!private! !
!JVM categoriesFor: #makeVFPrintfHook!JVM hooks!private! !
!JVM categoriesFor: #name!accessing!public! !
!JVM categoriesFor: #name:!accessing!public! !
!JVM categoriesFor: #notifyBorn!events!initializing!private! !
!JVM categoriesFor: #notifyClassPurged:!events!public! !
!JVM categoriesFor: #notifyClassRegistered:!events!public! !
!JVM categoriesFor: #notifyDead!events!private!shutting down! !
!JVM categoriesFor: #notifyLive!events!initializing!private! !
!JVM categoriesFor: #notifyNowGlobal:!managed objects!private! !
!JVM categoriesFor: #notifyNowLocal:!managed objects!private! !
!JVM categoriesFor: #notifyShutdownStarting!events!private!shutting down! !
!JVM categoriesFor: #notifySupplementaryClassloadersChanged!events!private!supplementary classloaders! !
!JVM categoriesFor: #objectRefCount!managed objects!monitoring!public! !
!JVM categoriesFor: #objectsCreated!managed objects!monitoring!public! !
!JVM categoriesFor: #objectsReleased!managed objects!monitoring!public! !
!JVM categoriesFor: #onExit!event handling!private! !
!JVM categoriesFor: #onStartup!event handling!private! !
!JVM categoriesFor: #own:!managed objects!private! !
!JVM categoriesFor: #printOn:!printing!public! !
!JVM categoriesFor: #rememberExternalCallback:!helpers!Java callbacks!JVM hooks!public! !
!JVM categoriesFor: #removeWatcher:!events!public! !
!JVM categoriesFor: #resetRegistries!initializing!private!shutting down! !
!JVM categoriesFor: #settings!accessing!public! !
!JVM categoriesFor: #shallowCopy!copying!public! !
!JVM categoriesFor: #sharedMutex!accessing!public! !
!JVM categoriesFor: #shutdown!finalizing!operations!public!shutting down! !
!JVM categoriesFor: #shutdownCallbackRegistry!private!shutting down! !
!JVM categoriesFor: #status!accessing!public! !
!JVM categoriesFor: #stderr!helpers!public! !
!JVM categoriesFor: #stdin!helpers!public! !
!JVM categoriesFor: #stdout!helpers!public! !
!JVM categoriesFor: #supplementaryClassloaderNamed:!accessing!public!supplementary classloaders! !
!JVM categoriesFor: #supplementaryClassloaders!accessing!public!supplementary classloaders! !
!JVM categoriesFor: #supportsCallbacks!Java callbacks!public!testing! !
!JVM categoriesFor: #systemClassloader!accessing!Java classes!public! !
!JVM categoriesFor: #systemClassLoader!accessing!Java classes!public! !
!JVM categoriesFor: #throwJavaException:!exceptions!public! !
!JVM categoriesFor: #usesGhostClasses!ghost classes!public!testing! !
!JVM categoriesFor: #usesLazyGhostClasses!ghost classes!public!testing! !
!JVM categoriesFor: #useSupplementaryClassloaders:!public!supplementary classloaders! !
!JVM categoriesFor: #usingSupplementaryClassloaders!public!supplementary classloaders!testing! !
!JVM categoriesFor: #wrapJNIClass:!managed objects!public! !
!JVM categoriesFor: #wrapJNIObject:!managed objects!public! !
!JVM categoriesFor: #wrapJNIString:!managed objects!public! !

JVM methodProtocol: #javaClassFinder attributes: #() selectors: #(#classIndex #classloader #defineClass:fromBytes: #defineClassFromBytes: #findClass: #findClassObject: #jvm #sharedMutex)!
JVM methodProtocol: #javaWrapperFactory attributes: #() selectors: #(#jvm #wrapJNIObject:)!

!JVM class methodsFor!

addRunningInstance: aJVM
	"private -- add aJVM to the list of instances we know about"

	InstancesStarted := InstancesStarted + 1.
	RunningInstances add: aJVM.
	##(self) trigger: #JVMAdded: with: aJVM.
!

bugs
	"answer a String describing the less than outstanding work"

	^
'-- Fundamental --
   JavaLangString>>asByteArray is almost certanly mis-specified.
   Dolphin callbacks are not thread-safety aware (probably not fixable without help from the VM).
   Shouldn''t generate wrapper methods at all if we can''t make them unambiguous.
 
-- Deadlocks --
   Calling java.lang.System.exit() while hooking JVM exit() (causes runtime error in 1.4.1 or later).

-- Cosmetic --
   Status page doesn''t seem to refresh properly on #refresh/F5.
   #JVMIsDead doesn''t seem to be reaching the status monitor reliably.
   History status page gives all intervals the same display width even after update interval is changed (misleading).
   Classes page doesn''t update as new methods are generated.
   Ghost method counts can be misleading (they include #jvm if it''s defined).
'.!

current
	"answer an arbitrarily selected running instance

		self current.
	"

	^ self runningInstances first.!

currentIfNone: a0Block
	"answer an arbitrarily selected running instance or the result of evaluating a0block if there
	aren't any"

	^ self runningInstances at: 1 ifAbsent: a0Block.!

default
	"answer an arbitrarily selected running instance, or start a new one (with the default settings)
	if there isn't one running already.

		self default.
	"

	^ self currentIfNone: [self newWithDefaultSettngs].!

icon
	"answer an Icon representing the receiver"

	^ self icon: 'JVM'.!

icon: aString

	"this will have to be recompiled if the package we live in changes its name"
	^ (Smalltalk at: #Icon)
		fromFile: (File composeStem: aString extension: 'ico')
		usingLocator: (PackageResourceLocator packageNamed: ##(self owningPackage name)).
!

initialize
	"private -- class initialisation.

		self initialize.
	"

	InstancesStarted := 0.
	RunningInstances := OrderedCollection new.

	SessionManager current when: #sessionStarted send: #onStartup to: self.
!

instancesStarted
	"answer how many instances have been started since the begining of this session"

	^ InstancesStarted.!

newWithDefaultSettngs
	"answer a new instance which uses the default settings.
	NB: the actual instance answered will be of some subclass of JVM
	determined by the default settings"

	^ self newWithSettings: JVMSettings default.
!

newWithSettings: aJVMSettings
	"answer a new instance which  is initialised from the given JVMSettings.
	NB: the actual instance answered will be of some subclass of JVM
	determined by the given settings"

	"I haven't moved responsibility for creating and initialising instances onto the
	JVMSettings object, where it would seem to belong given that it knows what
	class of JVM to use, because I'm far from convinced that the #jvmClass setting
	will continue to exist.  Hence we leave public responsibility for being the JVM
	factory with JVM class"

	^ (aJVMSettings jniPortSettings jvmClass new)
		initialize;
		initializeFromSettings: aJVMSettings;
		yourself.
!

newWithSettingsNamed: aString
	"answer a new instance which  is initialised from the named JVMSettings"

	^ self newWithSettings: (JVMSettings named: aString).
!

onStartup
	"private -- called as an image starts, zero our count of running instances"

	InstancesStarted := 0.
!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by JVM instances.
	NB: this does *not* include the notification messages sent to Watchers
	registered via the #addWatcher: mechanism"

	^ (super publishedEventsOfInstances)
		add: #JVMIsBorn;
		add: #JVMIsLive;
		add: #JVMIsDead;
		add: #JVMShutdownStarting;
		add: #JVMAbort;
		add: #JVMExit:;			"parameter is the exit code"
		add: #JVMMessage:address:;	"parameters are the text of the message and the ExternalAddress of the VM FILE* stream"
		add: #JVMSupplementaryClassloadersChanged;
		add: #JVMReset;			"JNPort development only"
		yourself.!

removeRunningInstance: aJVM
	"private -- remove aJVM from the list of instances we know about"

	RunningInstances remove: aJVM ifAbsent: [].
	##(self) trigger: #JVMRemoved: with: aJVM.!

runningInstances
	"answer a collection of all the running instances

		self runningInstances.
	"

	^ RunningInstances select: [:each | each isLive].!

todo
	"answer a String describing the outstanding work"

	^
'-- General Functional Improvements --
   Move ghost class activation into the #ghostClassSettings,
   Write some tests for the new sruff.
   Update all packages'' versions to 2.00.
   Update documentation, write new page on classloaders.
   Provide easier access to nested classes (access via ghost methods too ?).
   Should be more aggressive about not generating unnecessary overriding methods.
   Add properly wrapped access for the direct byte functions (added in JNI 1.4).

-- IDE/GUI Support --
   Add some monitoring of "lazy" ghost numbers.
  *Supplementary classloaders page: finish it.
   Wrapper wizard: allow user to select which methods are generated individually.
   Wrapper wizard: allow user to regenerate only the pre-existing wrappers.
   Wrapper wizard: should be SupplementaryClassloaders-aware.
   Status page: should be cleverer about displaying the status of configurable features.
   Console page: add stdin ?

-- Things to Ponder --
   What is JavaLangString>>asByteArray really supposed to answer ?
   What else do we need to allow deployment of Java-based apps ?
   For Java 5, automagically:
               convert float params to java.lang.Float, etc ?
               convert Smalltalk collections passed to varadic methods ?.
               generate extension methods (doSomething:[withObject:[withObject:[...]]]) for varadic methods ?.
   How to handle STB-ing  (possibly link to Java serialization) ?
   How to handle Uncode better ?
   Add a #minimumJNIVersion to the jniPort settings ?
   Throw out use of JNI local references altogether ?
'.!

uninitialize
	"private -- class initialisation.

		self uninitialize.
	"

	self runningInstances do: [:each | each shutdown].
	RunningInstances := nil.
	SessionManager current removeEventsTriggeredFor: self.! !
!JVM class categoriesFor: #addRunningInstance:!instances!private! !
!JVM class categoriesFor: #bugs!documentation!public! !
!JVM class categoriesFor: #current!instances!public! !
!JVM class categoriesFor: #currentIfNone:!instances!public! !
!JVM class categoriesFor: #default!instance creation!instances!public! !
!JVM class categoriesFor: #icon!constants!public! !
!JVM class categoriesFor: #icon:!constants!public! !
!JVM class categoriesFor: #initialize!initializing!private! !
!JVM class categoriesFor: #instancesStarted!accessing!public! !
!JVM class categoriesFor: #newWithDefaultSettngs!instance creation!public! !
!JVM class categoriesFor: #newWithSettings:!instance creation!public! !
!JVM class categoriesFor: #newWithSettingsNamed:!instance creation!public! !
!JVM class categoriesFor: #onStartup!event handling!private! !
!JVM class categoriesFor: #publishedEventsOfInstances!constants!events!public! !
!JVM class categoriesFor: #removeRunningInstance:!instances!private! !
!JVM class categoriesFor: #runningInstances!instances!public! !
!JVM class categoriesFor: #todo!documentation!public! !
!JVM class categoriesFor: #uninitialize!initializing!private! !

JVMSettings guid: (GUID fromString: '{4D3803FC-B277-41BC-BCB5-C81430F40811}')!
JVMSettings comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

A JavaSettings is really just a dictionary of sub-settings keyed by a selector.  This allows us to add/remove categories of settings according to which JNIport extensions are in use.

We also (the class) act as a factory for instances in the normal manner, but the new instances are created by cloning our "template" (which is an editable aspect of the class), rather than by initialising a newly created instance.

Last we also contain a list of pre-defined settings, that is also an editable aspect of the class.  Default JVMs are initialised from the first on the list of predefined settings.

Instances know how to persist themselves into the registry, or to re-configure themselves by reading the registry.  The class is similarly able to persist the template and predefined list.'!
!JVMSettings categoriesForClass!Unclassified! !
!JVMSettings methodsFor!

addSubSettings: aSettings name: aSelector
	"private -- add the object aSettings to our collection of subsettings under the given name.
	This is intended to be called as subsystems install themselves, and it (when called
	via our class method, #addToTemplate:definition:) allows subsystems to swap in
	more-or-less dynamic extensions to the Java settings.
	Note, however, that aSelector must be defined against this class too"

	categories at: aSelector put: aSettings.!

displayOn: aStream
	"append a user-oriented description of ourselves to aStream"

	name displayOn: aStream
!

initialize
	"private -- establish a coherent initial state.
	NB: this is called only once, as the 'template' instance is first created; subsequent
	instances of this class are created by cloning that"

	super initialize.
	categories := IdentityDictionary new.
!

jniPortSettings
	"answer the subcollection of settings for configuring the JVM object"

	^ self subSettings: #jniPortSettings.!

loadFromRegistryUnder: aRegKey
	"load our state from the registry under the given key.  Answer whether
	we found an acceptable minimum of data there.
	NB: we are, in fact, exceedingly forgiving about missing data"

	| ok |

	name := aRegKey valueAt: 'name' ifAbsent: [''].

	ok := true.
	categories keysAndValuesDo:
		[:key :each |
		aRegKey at: key ifPresent: [:it | ok := ok & (each loadFromRegistryUnder: it)]].

	^ ok.!

localClasspath
	"answer the subcollection of settings for setting a 'local' class path"

	^ self subSettings: #localClasspath.!

name
	"answer the receiver's String name"

	^ name.
!

name: aString
	"set the receiver's name to aString"

	name := aString.!

printOn: aStream
	"write a developer oriented representation of the receiver to aStream"

	super printOn: aStream.
	name isNil ifFalse:
		[aStream nextPut: $(; display: name; nextPut: $)].!

publishedAspects
	"answer the aspects of this object"

	| aspect |

	aspect := Smalltalk at: #Aspect.

	^ (super publishedAspects)
		add: (aspect string: #name);
		addAll: (categories keys collect: [:each | aspect name: each]);
		yourself.
!

removeSubSettings: aSelector
	"private -- remove the named set of subsettings"

	categories removeKey: aSelector ifAbsent: [].!

runtimeSettings
	"answer the subcollection of settings for the JNI runtime"

	^ self subSettings: #runtimeSettings.!

saveToRegistryUnder: aRegKey
	"save our state to the registry under the given key"

	aRegKey valueAt: 'name' put: name.
	categories keysAndValuesDo:
		[:key :each || subkey |
		subkey := aRegKey createKey: key.
		each saveToRegistryUnder: subkey].
!

subSettings: aSelector
	"answer the subsettings object with the given name.
	NB: this is normally only called from within this class since,
	by convention, any dynamically added sub-settings will also
	add a selector with the same name to this class"

	^ categories at: aSelector.!

supplementaryClassloaders
	"answer the subcollection of settings for setting a 'local' class path"

	^ self subSettings: #supplementaryClassloaders.! !
!JVMSettings categoriesFor: #addSubSettings:name:!operations!public! !
!JVMSettings categoriesFor: #displayOn:!displaying!public! !
!JVMSettings categoriesFor: #initialize!initializing!private! !
!JVMSettings categoriesFor: #jniPortSettings!accessing!public! !
!JVMSettings categoriesFor: #loadFromRegistryUnder:!public!registry! !
!JVMSettings categoriesFor: #localClasspath!accessing!public! !
!JVMSettings categoriesFor: #name!accessing!public! !
!JVMSettings categoriesFor: #name:!accessing!public! !
!JVMSettings categoriesFor: #printOn:!printing!public! !
!JVMSettings categoriesFor: #publishedAspects!constants!development!must strip!public! !
!JVMSettings categoriesFor: #removeSubSettings:!operations!public! !
!JVMSettings categoriesFor: #runtimeSettings!accessing!public! !
!JVMSettings categoriesFor: #saveToRegistryUnder:!public!registry! !
!JVMSettings categoriesFor: #subSettings:!accessing!public! !
!JVMSettings categoriesFor: #supplementaryClassloaders!accessing!public! !

!JVMSettings class methodsFor!

addToTemplate: aTemplateObject name: aSelector
	"add aTemplateObject to our template accessible via the given name.
	Also adds it to any any predefined instances too"

	"ensure we are initialised, since the order of class initialisation is not defined by Dolphin"
	template isNil ifTrue: [self initialize].

	template addSubSettings: aTemplateObject name: aSelector.
	predefined do: [:each | each addSubSettings: aTemplateObject deepCopy name: aSelector].
!

default
	"answer the default instance which is, in fact, always the first of the predefined settings.  It
	is an error if no such settings have been defined.

		self default.
	"

	^ predefined first.!

icon
	"answer an Icon representing the receiver"

	^ JVM icon.!

initialize
	"private -- class-side intialisation.

		self initialize.

		JVMSubSettings allSubclasses do: [:each | each initialize].
	"

	template := (super new)		"can't call our own #new since it just clones the template"
			initialize;
			name: '<template>';
			yourself.

	predefined := OrderedCollection new.

	self registerOptions.
!

loadFromRegistry
	"reload class settings (i.e. the system-wide list of pre-defined JVM configurations) from
	the registry.

		self loadFromRegistry.
	"

	self loadFromRegistryUnder: self registryEntryRoot.!

loadFromRegistryUnder: aRegKey
	"load our class settings (i.e. the system-wide list of pre-defined JVM configurations) from
	the registry under the given key"

	| count |

	template := self newFromRegistryKey: 'template' under: aRegKey ifNone: [^ self].

	count := aRegKey valueAt: 'predefined count' ifAbsent: [0].
	predefined := OrderedCollection new: count.
	1 to: count do:
		[:i || subkey |
		subkey := 'predefined %02d' sprintfWith: i.
		predefined addLast: (self newFromRegistryKey: subkey under: aRegKey ifNone: [^ self])].!

named: aString
	"answer the predefined instance with the given name"

	^ predefined detect: [:each | each name = aString].!

new
	"answer a new instance that is a copy of the template instance"

	^ (template deepCopy)
		name: '';
		yourself.!

newFromRegistryKey: aString under: aRegKey ifNone: a0Block
	"answer a new instance created (as always) by cloning our template, but then overwriting
	any information in the instance with whatever data there is in the registry in the named subkey of
	the given key.
	If there is no reasonable minimum of such data then answer the result of evaluating a0block"

	| subkey |

	subkey := aRegKey at: aString ifAbsent: [^ a0Block value].
	^ self newFromRegistryUnder: subkey ifNone: a0Block.
!

newFromRegistryUnder: aRegKey ifNone: a0Block
	"answer a new instance created (as always) by cloning our template, but then overwriting
	any information in the instance with whatever data there is in the registry under the given key.
	If there is no reasonable minimum of such data then answer the result of evaluating a0block"

	| new |

	new := self new.

	^ (new loadFromRegistryUnder: aRegKey)
		ifTrue: [new]
		ifFalse: [a0Block value].!

predefined
	"answer the OrderedCollection containing the predefined instances.

		self predefined.
	"

	^ predefined.!

predefined: anOrderedCollection
	"set the OrderedCollection containing the predefined instances"

	predefined := anOrderedCollection.!

publishedAspects
	"answer our own (the class's) aspects.

		PublishedAspectInspector shellOn: self.
	"

	| aspect |

	aspect := Smalltalk at: #Aspect.

	^ super publishedAspects
		add: (aspect name: #template) beReadOnly;
		add: (aspect sequenceableCollection: #predefined addEvaluationFrom: #(##(self name , ' new')));
		yourself.
!

registerOptions
	"private -- add the default JVM settings to the system options menu.

		self registerOptions.
	"

	(Smalltalk at: #SmalltalkSystem) registerTool: self.!

registryEntryRoot
	"private -- answer a RegKey representing the root of our registry entry"

	^ RegKey userRoot createKey: self registryRootName.
!

registryRootName
	"private -- answer the name of the root of our registy entry"

	^ 'Software\Metagnostic\JNIPort\Predefined settings'.!

removeFromTemplate: aString
	"remove the subsetting accessed via aSelector from our template and all predefined
	settings"

	template removeSubSettings: aString.
	predefined do: [:each | each removeSubSettings: aString].!

saveToRegistry
	"save our class settings (i.e. the system-wide list of pre-defined JVM configurations) to
	the registry.

		self saveToRegistry.
	"

	self saveToRegistryUnder: self registryEntryRoot.!

saveToRegistryUnder: aRegKey
	"save our class settings (i.e. the system-wide list of pre-defined JVM configurations) to
	the registry under the given key"

	template saveToRegistryUnder: (aRegKey createKey: 'template').

	aRegKey valueAt: 'predefined count' put: predefined size.
	predefined keysAndValuesDo:
		[:i :each || subkey |
		subkey := aRegKey createKey: ('predefined %02d' sprintfWith: i).
		each saveToRegistryUnder: subkey].
!

template
	"answer a the template instance which is cloned for further instance creation.
	Since this is a published aspect, the defaults can be edited from within the IDE.

		PublishedAspectInspector shellOn: self.
	"

	^ template.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	self unregisterOptions.
	template := predefined := nil.!

unregisterOptions
	"private -- add the default JVM settings to the system options menu.

		self unregisterOptions.
	"

	(Smalltalk at: #SmalltalkSystem) unregisterTool: self.! !
!JVMSettings class categoriesFor: #addToTemplate:name:!adding!public! !
!JVMSettings class categoriesFor: #default!accessing!public! !
!JVMSettings class categoriesFor: #icon!constants!public! !
!JVMSettings class categoriesFor: #initialize!initializing!private! !
!JVMSettings class categoriesFor: #loadFromRegistry!public!registry! !
!JVMSettings class categoriesFor: #loadFromRegistryUnder:!public!registry! !
!JVMSettings class categoriesFor: #named:!accessing!public! !
!JVMSettings class categoriesFor: #new!instance creation!public! !
!JVMSettings class categoriesFor: #newFromRegistryKey:under:ifNone:!instance creation!public!registry! !
!JVMSettings class categoriesFor: #newFromRegistryUnder:ifNone:!instance creation!public!registry! !
!JVMSettings class categoriesFor: #predefined!accessing!public! !
!JVMSettings class categoriesFor: #predefined:!accessing!public! !
!JVMSettings class categoriesFor: #publishedAspects!constants!development!must strip!public! !
!JVMSettings class categoriesFor: #registerOptions!development!initializing!private! !
!JVMSettings class categoriesFor: #registryEntryRoot!private!registry! !
!JVMSettings class categoriesFor: #registryRootName!constants!private!registry! !
!JVMSettings class categoriesFor: #removeFromTemplate:!adding!public!removing! !
!JVMSettings class categoriesFor: #saveToRegistry!public!registry! !
!JVMSettings class categoriesFor: #saveToRegistryUnder:!public!registry! !
!JVMSettings class categoriesFor: #template!accessing!public! !
!JVMSettings class categoriesFor: #uninitialize!initializing!private! !
!JVMSettings class categoriesFor: #unregisterOptions!development!initializing!private! !

JVMSubSettings guid: (GUID fromString: '{5FF84DE4-B68D-449E-A738-5C867369646F}')!
JVMSubSettings comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

This class is to be considered deprecated since I intend to move the subclasses to use my more powerful "application settings" package/framework, when -- if -- I finish it.'!
!JVMSubSettings categoriesForClass!Unclassified! !
!JVMSubSettings methodsFor!

allFlagsSet: anInteger
	"answer true iff all the bits that are set in anInteger are also set in our flags mask"

	^ flags allMask: anInteger.!

anyFlagsSet: anInteger
	"answer true iff alny bits that are set in anInteger are also set in our flags mask"

	^ flags anyMask: anInteger.!

clearFlags: anInteger
	"turn off any bits in our flags mask that are set in anInteger"

	flags := flags maskClear: anInteger.!

initialize
	"private -- establish a coherent initial state"

	flags := self class defaultFlags.!

loadClassFromRegistryUnder: aRegKey ifNone: a0Block
	"private -- answer the class that is referenced by the data in the registry under the given key"

	| name guid |

	name := aRegKey valueAt: 'name' ifAbsent: [nil].
	guid := aRegKey valueAt: 'guid' ifAbsent: [nil].

	name isNil ifFalse: [Smalltalk at: name ifPresent: [:it | ^ it]].

	Notification signal: ('Warning: found no class called ' , name , ' trying GUID').

	guid isNil ifFalse: [guid := GUID fromBytes: guid. Class allClasses do: [:each | each guid = guid ifTrue: [^ each]]].

	^ a0Block value.!

loadFromRegistryUnder: aRegKey
	"load our state from the registry under the given key.  Answer whether
	we found an acceptable minimum of data there.
	NB: we are, in fact, exceedingly forgiving about missing data"

	self class booleanAspectNames do:
		[:each || value |
		value := aRegKey valueAt: each ifAbsent: [nil].
		value isNil ifFalse: [self perform: (each , ':') asSymbol with: value ~= 0]].

	self class integerAspectNames do:
		[:each || value |
		value := aRegKey valueAt: each ifAbsent: [nil].
		value isNil ifFalse: [self perform: (each , ':') asSymbol with: value]].

	self class stringAspectNames do:
		[:each || value |
		"use 77 as a magic cookie, since we want to allow nil strings"
		value := aRegKey valueAt: each ifAbsent: [77].
		value == 77 ifFalse: [self perform: (each , ':') asSymbol with: value]].

	^ self loadOtherAspectsFromRegistryUnder: aRegKey.!

loadOtherAspectsFromRegistryUnder: aRegKey
	"private -- load our non-standard state from the registry under the given key.
	Answer true iff we found an acceptable minumum of data"

	"default is to do nothing"
	#subclassResponsibility.

	^ true.
!

saveClass: aClass toRegistryUnder: aRegKey
	"private -- save a reference to the given class  to the registry under the given key"

	aRegKey
		valueAt: 'name' put: aClass name;
		valueAt: 'guid' put: aClass guid.!

saveOtherAspectsToRegistryUnder: aRegKey
	"private -- save our non-standard state to the registry under the given key"

	"default is to do nothing"
	#subclassResponsibility.
!

saveToRegistryUnder: aRegKey
	"save our state to the registry under the given key"

	self class booleanAspectNames do:
		[:each || value |
		value := (self perform: each) ifTrue: [1] ifFalse: [0].
		aRegKey valueAt: each put: value].

	self class integerAspectNames do:
		[:each | aRegKey valueAt: each put: (self perform: each)].

	self class stringAspectNames do:
		[:each | aRegKey valueAt: each put: (self perform: each)].

	self saveOtherAspectsToRegistryUnder: aRegKey.
!

setFlags: anInteger
	"turn on any bits in our flags mask that are set in anInteger"

	flags := flags maskSet: anInteger.!

setFlags: anInteger to: aBool
	"turn on/off any bits in our flags mask that are set in anInteger"

	flags := flags mask: anInteger set: aBool.! !
!JVMSubSettings categoriesFor: #allFlagsSet:!accessing!public!testing! !
!JVMSubSettings categoriesFor: #anyFlagsSet:!accessing!public!testing! !
!JVMSubSettings categoriesFor: #clearFlags:!accessing!public! !
!JVMSubSettings categoriesFor: #initialize!initializing!private! !
!JVMSubSettings categoriesFor: #loadClassFromRegistryUnder:ifNone:!private!registry! !
!JVMSubSettings categoriesFor: #loadFromRegistryUnder:!public!registry! !
!JVMSubSettings categoriesFor: #loadOtherAspectsFromRegistryUnder:!private!registry! !
!JVMSubSettings categoriesFor: #saveClass:toRegistryUnder:!private!registry! !
!JVMSubSettings categoriesFor: #saveOtherAspectsToRegistryUnder:!private!registry! !
!JVMSubSettings categoriesFor: #saveToRegistryUnder:!public!registry! !
!JVMSubSettings categoriesFor: #setFlags:!accessing!public! !
!JVMSubSettings categoriesFor: #setFlags:to:!accessing!public! !

!JVMSubSettings class methodsFor!

booleanAspectNames
	"private -- answer an Array of the names of boolean aspects of instances"

	^ #().!

defaultFlags
	"answer the collection of flags that are set by default"

	^ 0.!

integerAspectNames
	"private -- answer an Array of the names of integer aspects of instances"

	^ #().!

new
	"answer a new, default initialised instance"

	^ (super new)
		initialize;
		yourself.!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

	| aspects aspect |

	aspects := super publishedAspectsOfInstances.
	aspect := Smalltalk at: #Aspect.

	self booleanAspectNames do: [:each | aspects add: (aspect boolean: each)].
	self integerAspectNames do: [:each | aspects add: (aspect integer: each)].
	self stringAspectNames do: [:each | aspects add: (aspect string: each)].

	^ aspects.!

stringAspectNames
	"private -- answer an Array of the names of string aspects of instances"

	^ #().! !
!JVMSubSettings class categoriesFor: #booleanAspectNames!constants!development!must strip!private! !
!JVMSubSettings class categoriesFor: #defaultFlags!constants!public! !
!JVMSubSettings class categoriesFor: #integerAspectNames!constants!development!must strip!private! !
!JVMSubSettings class categoriesFor: #new!instance creation!public! !
!JVMSubSettings class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!JVMSubSettings class categoriesFor: #stringAspectNames!constants!development!must strip!private! !

JVMWatcher guid: (GUID fromString: '{6BFB368B-30DC-4026-B46F-CA621BDDFA32}')!
JVMWatcher comment: 'Copyright © Chris Uppal, 2001-2004.
chris.uppal@metagnostic.org

Base class for classes that are informed whenever a JVM starts up.  The typical action is to add a new instance to that JVM''s watcher list; the JVM will then send notification messages to that watcher as it operates.

The idea of a JVMWatcher is to allow flexible and configurable extensions to the JVM.  The immediate application is to create a Ghost class "manager" that watches for new classes being registered and converts them into ghosts.  Other potential applications will undoubtedly discover themselves to us in due course.

Note that the list of subclasses to which a new JVM will send #onJvmStartup is configurable as part of the JavaBaseSettings, and therefore will vary on a by-JVM basis.  That is one reason why the event system is not used.  (Another is that the event system is relatively slow, and -- although I don''t yet have a need for it -- this mechanism is intended to provide a tightly-coupled linkage to the JVM.)'!
!JVMWatcher categoriesForClass!Unclassified! !
!JVMWatcher methodsFor!

initialize
	"private -- establish a coherent initial state"

	jvmIsInitialized := false.!

jvmIsInitialized
	"answer whether our JVM has been initialized (and not yet shotdown)"

	^ jvmIsInitialized.!

onClassPurged: aJavaStatic
	"notification recieved when a class is about to be removed from the JVM's
	registry.  Since the class is in an iffy state, we should refrain from sending it
	any messages that will be forwarded to the real Java runtime"!

onClassRegistered: aJavaStatic
	"notification recieved when a class has been added to the JVM's
	registry (the owning JVM can be found via the class object).
	Note that this will be called for each wrapper class during JVM
	bootstrap, *before* we recieve notification that the initialisation
	is complete"!

onJmvInitialized: aJVM
	"notification recieved when the given JVM has finished initialisation"

	jvmIsInitialized := true.!

onJvmShutdown: aJVM
	"notification recieved when the given JVM is about to shutdown.  There's not much that
	can safely be assumed about its state at this point"

	jvmIsInitialized := false.
!

onNotWatchingJvm: aJVM
	"notification recieved when we have been removed from the JVM's watchlist.
	This is only called if someone explicitly removes us from its list, there is
	no automatic way that this happens"!

onWatchingJvm: aJVM
	"notification recieved when we have been added to the JVM's watchlist.
	Note that this is called *before* the JVM has finished initialisation if we are
	added though the normal configuration mechanism.
	Specifically, this is called before the JVM has registered any wrapper classes
	(although it has bootstrapped the class registry)"!

usesGhostClasses
	"answer whether we use ghost class wrappers"

	^ false.!

usesLazyGhostClasses
	"answer whether we use lazy ghost class wrappers"

	^ false.! !
!JVMWatcher categoriesFor: #initialize!initializing!private! !
!JVMWatcher categoriesFor: #jvmIsInitialized!public!testing! !
!JVMWatcher categoriesFor: #onClassPurged:!event handling!public! !
!JVMWatcher categoriesFor: #onClassRegistered:!event handling!public! !
!JVMWatcher categoriesFor: #onJmvInitialized:!event handling!public! !
!JVMWatcher categoriesFor: #onJvmShutdown:!event handling!public! !
!JVMWatcher categoriesFor: #onNotWatchingJvm:!event handling!public! !
!JVMWatcher categoriesFor: #onWatchingJvm:!event handling!public! !
!JVMWatcher categoriesFor: #usesGhostClasses!public!testing! !
!JVMWatcher categoriesFor: #usesLazyGhostClasses!public!testing! !

!JVMWatcher class methodsFor!

new
	"answer a default initialized instance"

	^ (super new)
		initialize;
		yourself.!

onJvmStartup: aJVM
	"this is called whenever a JVM is started with settngs that include this class on
	its list of watcher classes"

	"default response is to add an instance to the JVM's list of watchers"
	aJVM addWatcher: self new.! !
!JVMWatcher class categoriesFor: #new!instance creation!public! !
!JVMWatcher class categoriesFor: #onJvmStartup:!event handling!public! !

SupplementaryClassloader guid: (GUID fromString: '{DB9174EC-61DA-4B4C-B42D-7640A5753E85}')!
SupplementaryClassloader comment: 'jCopyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

These are part of the framework for manipulating Java classloaders within JNIPort.  Instances are arranged into trees (as elements of SupplementaryClasspathTree).  Each instance corresponds to one, lazily created, instance of java.lang.Classsloader, and uses that to load "local" classes.  See the SupplementaryClasspathTree class comment for a more complete discussion.

Note that, although we conform to <javaClassFinder>, we actually implement it by delegating to our actual classloader (since the logic has to live somewhere, I thought it better to add it to JavaLangClassloader where it can be used independently of this part of the framework).

These should only exist as entries on a list maintained by a SupplementaryClassloaderTree, and so should not be created independently.

NB: currently the entire supplementary classloader framework lacks proper mutex protection.  This needs to be addressed !!

'!
!SupplementaryClassloader categoriesForClass!Unclassified! !
!SupplementaryClassloader methodsFor!

activate
	"if we are enabled and not already active then activate ourself, otherwise
	do nothing"

#CUtodo.  "this should probably be protected by the shared mutex"

	self isActive ifTrue: [^ self].
	isEnabled ifFalse: [^ self].

	classloader := self createClassloader.
	JavaObject registerWrapperClassesWithSupplementaryClassloader: self.
	self notifyChanged.

!

classIndex
	"answer our index of known-by-name classes"

	^ self classloader classIndex.!

classloader
	"answer our classloader, may activate it if we are not yet active,
	or answer nil if we are disabled"

	"ensure that we're as active as we're alowed to be"
	self activate.

	^ classloader.
!

classloaderClass
	"answer the Symbol name of our classloader class"

	^ classloaderClass ifNil: [self defaultClassloaderClass].
!

classloaderClass: aString
	"set our classloader classname to aString"

	self assert: [self isActive not].

	classloaderClass := aString asSymbol.
	self notifyChanged.!

classloaderIs: aJavaLangClassloader
	"answer whether our classloader is the same as that given"

	"we use an identity test since JavaLangClassloader has canonical instances"
	^ aJavaLangClassloader == classloader.!

copyFrom: aSupplementaryClassloader
	"private -- copy configuration data from aSupplementaryClassloader"

	path := aSupplementaryClassloader path.
	parentName := aSupplementaryClassloader parentName.
	classloaderClass := aSupplementaryClassloader classloaderClass.
	isEnabled := aSupplementaryClassloader isEnabled.

	self notifyChanged.!

createClassloader
	"private -- answer a new JavaLangClassloader (or an instance of a subclass)
	created from our #path and #classloaderClass definition"

	| parent finder classStatic |

	parent := self parentEntry.
	finder := parent ifNil: [self jvm].
	classStatic := finder findClass: self classloaderClass.

	^ classStatic
		newPath: self path
		parent: (parent ifNotNil: [:it | it classloader]).!

defaultClassloaderClass
	"private -- answer the classloader class to use by default"

	^ #'java.net.URLClassLoader'.!

defineClass: aString fromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a Class Static corresponding to the newly defined class and loaded by our
	underlying classloader.
	Will throw an error if the class definition does not define a class named by
	aString (which can be in JNI or Java format)."

	^ self classloader defineClass: aString fromBytes: aByteArray.!

defineClassFromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	defining the class named by aJNIClassName.
	Answers a Class Static corresponding to the newly defined class and loaded by our
	underlying classloader"

	^ self classloader defineClassFromBytes: aByteArray.
!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream
		display: self name; space;
		print: self path;
		yourself.

	self hasParent ifTrue: [aStream nextPutAll: ' ['; display: self parentName; nextPutAll: ']'].
!

findClass: aStringOrSymbol
	"answer a JavaStatic object corresponding to aString -- the name will be looked up in the
	normal Java way startng at our underlying classloader, but delagating /first/ to its parent
	classloader and the system classloaders.  Note that the delagation is handled inside
	the Java runtime, we don't do it ourself"

	^ self classloader
		ifNil: [self errorNotFound: aStringOrSymbol]
		ifNotNil: [:it | it findClass: aStringOrSymbol].!

findClassObject: aJNIClassName
	"answer JavaLangClass object corresponding to aJNIClassName -- the name will be looked up in the
	normal Java way startng at our underlying classloader, but delagating /first/ to its parent
	classloader and the system classloaders.  Note that the delagation is handled inside
	the Java runtime, we don't do it ourself"

	^ self classloader findClassObject: aJNIClassName.
!

hasParent
	"answer whether we have a parent entry"

	^ self parentName notEmpty.!

initialize
	"private -- establish a coherent initial state"

	isEnabled := false.
!

isActive
	"answer whether we are active"

	^ classloader notNil.
!

isDisabled
	"answer whether we are disabled"

	^ isEnabled ifNil: [false] ifNotNil: [:it | it not].
!

isEnabled
	"answer whether we are enabled"

	^ isEnabled.
!

isEnabled: aBool
	"set whether we are enabled.  If not then we will not activate ourself lazily, but will instead throw an error.
	Note that disabling an active instance will cause it to purge itself"

	aBool = isEnabled ifTrue: [^ self].

	self isActive ifTrue: [self purge].

	isEnabled := aBool.
	self notifyChanged.
!

jvm
	"answer our owning JVM"

	^ containingTree jvm.!

loadFromRegistryUnder: aRegKey
	"load our state from the registry under the given key.  Answer whether
	we found an acceptable minimum of data there.
	NB: we are, in fact, exceedingly forgiving about missing data"

	#( #isEnabled ) do:
		[:each || value |
		value := aRegKey valueAt: each ifAbsent: [nil].
		value isNil ifFalse: [self perform: (each , ':') asSymbol with: value ~= 0]].

	#( #name #path #parentName #classloaderClass ) do:
		[:each || value |
		value := aRegKey valueAt: each ifAbsent: [nil].
		value isNil ifFalse: [self perform: (each , ':') asSymbol with: value]].

	^ true.!

localClassIndex
	"answer our index of known-by-name classes"

	^ self classloader localClassIndex.!

name
	"answer our Symbol name"

	^ name ifNil: ['<not set>' asSymbol].
!

name: aString
	"private -- set our name to aString.
	Note that it is an error to change the name of an instance after it has been added to a SupplementaryClassloaderTree.
	The only reason this method exists at all is to allow instances to be created and editied in a PAE as part of a
	settings object"

	self assert: [containingTree isNil].

	name := aString asSymbol.!

name: aSymbol owner: aSupplementaryClassloaderTree
	"private -- called by aSupplementaryClassloaderTree to tell what our name is and us that it owns us"

	name := aSymbol.
	containingTree := aSupplementaryClassloaderTree.!

nameOrPath
	"private -- answer our #name, or if that is not set, our #path.
	This is only used when a SupplementaryClassloaderTree is configuring itself
	by copying the edit-omly elements in a settings object"

	^ (name ifNil: [path ifNil: ['<anonymous>']]) asSymbol.
!

notifyChanged
	"private -- trigger change notification off our owner"

	containingTree ifNotNil: [:it | it notifyChanged: self].!

parentEntry
	"answer our parent, or nil if we have no declared parent"

	^ (parentName isNil or: [parentName isEmpty])
		ifTrue: [nil]
		ifFalse: [containingTree entryNamed: parentName ifAbsent: [nil]].!

parentName
	"answer our parent name, may be empty"

	^ parentName ifNil: [''].
!

parentName: aString
	"set our parent name to aString.  This is used to link instances
	up into trees"

	self assert: [self isActive not].

	parentName := aString asSymbol.
	self notifyChanged.
!

path
	"answer our path.  This is a slightly ill-defined concept, if our classloaderClass
	is #java.lang.Classloader, then the #path is the string that will be passed to
	its constructor -- a searchpath of URLs.  If the class is different then the #path
	will be interpreted in whatever way is appropriate (in so far as we can make a
	sensible guess about what is appropriate)"

	^ path ifNil: [''].
!

path: aString
	"set our path.  This is a slightly ill-defined concept, if our classloaderClass
	is #java.lang.Classloader, then the #path is the string that will be passed to
	its constructor -- a searchpath of URLs.  If the class is different then the #path
	will be interpreted in whatever way is appropriate (in so far as we can make a
	sensible guess about what is appropriate)"

	self assert: [self isActive not].

	path := aString.
	self notifyChanged.

!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		space;
		display: self.!

purge
	"purge this classloader.  That means that we:
		- purge any child classloaders we know about,
		- purge and discard our own uderlying JavaLangClassloader"

	self isActive ifFalse: [^ self].

	(containingTree childrenOfEntry: self) do: [:each | each purge].
	classloader purge.
	classloader := nil.

	self notifyChanged.!

saveToRegistryUnder: aRegKey
	"save our state to the registry under the given key"

	aRegKey valueAt: 'name' put: self name.
	aRegKey valueAt: 'path' put: self path.
	aRegKey valueAt: 'parentName' put: self parentName.
	aRegKey valueAt: 'classloaderClass' put: self classloaderClass.
	aRegKey valueAt: 'isEnabled' put: (self isEnabled ifTrue: [1] ifFalse: [0]).
!

sharedMutex
	"answer the JVM-wide mutex"

	^ self jvm sharedMutex.!

shutdown
	"private -- called when the JVM is shutting down.
	All we do is discard any runtime information, we don't attempt anything
	meaningfull in the way of cleaning up"

	classloader := nil.! !
!SupplementaryClassloader categoriesFor: #activate!operations!public! !
!SupplementaryClassloader categoriesFor: #classIndex!accessing!public! !
!SupplementaryClassloader categoriesFor: #classloader!accessing!public! !
!SupplementaryClassloader categoriesFor: #classloaderClass!accessing!public! !
!SupplementaryClassloader categoriesFor: #classloaderClass:!accessing!public! !
!SupplementaryClassloader categoriesFor: #classloaderIs:!public!testing! !
!SupplementaryClassloader categoriesFor: #copyFrom:!initializing!private! !
!SupplementaryClassloader categoriesFor: #createClassloader!helpers!private! !
!SupplementaryClassloader categoriesFor: #defaultClassloaderClass!constants!initializing!private! !
!SupplementaryClassloader categoriesFor: #defineClass:fromBytes:!Java classes!public! !
!SupplementaryClassloader categoriesFor: #defineClassFromBytes:!Java classes!public! !
!SupplementaryClassloader categoriesFor: #displayOn:!displaying!public! !
!SupplementaryClassloader categoriesFor: #findClass:!Java classes!public! !
!SupplementaryClassloader categoriesFor: #findClassObject:!Java classes!public! !
!SupplementaryClassloader categoriesFor: #hasParent!public!testing! !
!SupplementaryClassloader categoriesFor: #initialize!initializing!private! !
!SupplementaryClassloader categoriesFor: #isActive!public!testing! !
!SupplementaryClassloader categoriesFor: #isDisabled!public!testing! !
!SupplementaryClassloader categoriesFor: #isEnabled!public!testing! !
!SupplementaryClassloader categoriesFor: #isEnabled:!public!purging!testing! !
!SupplementaryClassloader categoriesFor: #jvm!accessing!public! !
!SupplementaryClassloader categoriesFor: #loadFromRegistryUnder:!public!registry! !
!SupplementaryClassloader categoriesFor: #localClassIndex!accessing!public! !
!SupplementaryClassloader categoriesFor: #name!accessing!public! !
!SupplementaryClassloader categoriesFor: #name:!initializing!private! !
!SupplementaryClassloader categoriesFor: #name:owner:!initializing!private! !
!SupplementaryClassloader categoriesFor: #nameOrPath!accessing!private! !
!SupplementaryClassloader categoriesFor: #notifyChanged!events!private! !
!SupplementaryClassloader categoriesFor: #parentEntry!accessing!public! !
!SupplementaryClassloader categoriesFor: #parentName!accessing!public! !
!SupplementaryClassloader categoriesFor: #parentName:!accessing!public! !
!SupplementaryClassloader categoriesFor: #path!accessing!public! !
!SupplementaryClassloader categoriesFor: #path:!accessing!public! !
!SupplementaryClassloader categoriesFor: #printOn:!printing!public! !
!SupplementaryClassloader categoriesFor: #purge!operations!public!purging! !
!SupplementaryClassloader categoriesFor: #saveToRegistryUnder:!public!registry! !
!SupplementaryClassloader categoriesFor: #sharedMutex!accessing!public! !
!SupplementaryClassloader categoriesFor: #shutdown!operations!private!purging! !

SupplementaryClassloader methodProtocol: #javaClassFinder attributes: #() selectors: #(#classIndex #classloader #defineClass:fromBytes: #defineClassFromBytes: #findClass: #findClassObject: #jvm #sharedMutex)!

!SupplementaryClassloader class methodsFor!

fromCLASSPATH
	"answer a new instance initialised from the %CLASSPATH% environment variable"

	^ (self new)
		name: 'Classpath';
		path: (SessionManager current getenv: 'CLASSPATH');
		yourself.!

new
	"answer a new instance with default initialisation"

	^ (self basicNew)
		initialize;
		yourself.!

newFromRegistryUnder: aRegKey ifNone: a0Block
	"answer a new instance populated with whatever data there is in the registry under the given key.
	If there is no reasonable minimum of such data then answer the result of evaluating a0block"

	| new |

	new := self new.

	^ (new loadFromRegistryUnder: aRegKey)
		ifTrue: [new]
		ifFalse: [a0Block value].!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

    	^ (super publishedAspectsOfInstances)
		add: (Aspect string: #classloaderClass);
		add: (Aspect boolean: #isEnabled);
		add: (Aspect string: #name);
		add: (Aspect string: #parentName);
		add: (Aspect string: #path);
		yourself.! !
!SupplementaryClassloader class categoriesFor: #fromCLASSPATH!instance creation!public! !
!SupplementaryClassloader class categoriesFor: #new!instance creation!public! !
!SupplementaryClassloader class categoriesFor: #newFromRegistryUnder:ifNone:!instance creation!public!registry! !
!SupplementaryClassloader class categoriesFor: #publishedAspectsOfInstances!commands!constants!development!must strip!public! !

SupplementaryClassloaderTree guid: (GUID fromString: '{7A4B5550-FB4F-4373-899E-F46604B61FC5}')!
SupplementaryClassloaderTree comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

This is the central object in the JNIPort framework for using custom Java classloaders.

Please note that instances of java.lang.ClassLoader (and its subclasses) are normal Java objects like any other, and as such can be manipuilated via JNIPort without using this framework.  It''s just a bit more work, and doesn''t mesh very nicely with the wrapper class concept.

First a bit of explanation about classloaders.  In Java you can define a classloader at any time, and then use it to load classes from classfiles (or similar).  The main points of doing so are:
	- in order to obtain a namespace that is completely independent of any other.
	- in order to be able to discard a classloader, and hence allow the classes it loaded to be GCed.
Java classes are never GCed unless their owning classloader is also eligible for GC.   Classloaders are aranged into a tree.  At the root is the primordial classloader which is built into the JVM (often called the ''null classloader'' or the ''bootstrap classloader''), this is often written in the same implementation language as the JVM itself, and so may not be a Java object at all (whence it''s representation as Java ''null'' -- and one of its nicknames).   As a child of that classloader is the ''application classloader'', this is the one that loads the application classes from the %CLASSPATH% (or from the path supplied by a -cp argument, or whatever similar mechanism is used -- in JNIPort it''s the #classpath in the #runtimeSetting that are used to initialise JNIPort).   Applications can then define further classloaders that are children of the application classloader.  It''s important to realise any classloaders (if it follows the normal rules) will delagate any attempt to load a class to its parent /before/ it will attempt to load the class itself.  So classloaders do NOT "override" their parents.

The JVM instance controlling JNIPort contains an instance of this class.  You can create and use additional instances independently of the one the JVM knows about, but this comment does not address that case.  The ''central'' instance is created from the #supplementaryClassloaders sub-setting of the JNIPort settings.  If those sub-settings have #useAtStartup then JNIPort will create a tree of classloaders as it starts up, otherwise it will not do so, and the supplementary classloaders feature will be inactive (you can [dis]activate it by sending #useSupplementaryClassloaders: to the JVM object at any time after it has started up).

The tree is populated by instances of SupplementaryClassloader.  Each of these has a Symbol name, a Symbol (or nil) name of its parent, and some data to alloow it to create an instance of java.lang.ClassLoader to do the actual classloading.  In fact, by default, they will create instances of java.net.URLClassLoader, and pass their #path string to it  to use as the search path.  SupplementaryClassloader can be added to, or removed from, the tree at any time after the JVM has started up, thus allowing some dynamic control over the algorothm use to locate Javas classsfiles by name.

Each SupplementaryClassloader can be enabled or disabled (almost) independently of all the others.  If it is active, then it acts as a <javaClassFinder> and will find classes by name, (#findClass: -- just like the JVM object, which is also a <javaClassFinder) using the internal java.lang.ClassLoader to do the work.  SupplementaryClassloader are laziliy initialised, when you ask one to frind a class it will first ask its parent (if any) to find the class, then if that fails, it will ensure that it is initialised (this is the time at which the internal classloader is created), and then ask the internal classloader to find the class with the given name.  If it finds one, then it will add the classname -> class record to its classIndex (to be used as a cache), and answers the discovered JavaClassStatic.

When a SupplementaryClassloader becomes active, it searches the hierarchy under JavaObject for any wrapper classes that declare that they are wrappers for classes that should be loaded via a SupplementaryClassloader with a given name.  Any such that it finds will be regstered with that SupplementaryClassloader in much the same way as most wrapper classes register with the JVM object.   Wrapper classes may elect to register with more than one SupplementaryClassloader, but there is no point in them attempting to register with the JVM object too, since it they do so, their underlying Java class will be loaded via the application or primordial classloader, and hence no custorm classloader will ever ''see'' that class.   Classes opt-in to particular classloaders by answering a list of classloader names from the class-side method #supplementaryClassloaderNames.  Alternatively, you could override #shouldRegisterWithSupplementaryClassloader: if you need to use a more complicated criterion).

Thus far we have seen that SupplementaryClassloaders allow you to load classes into custom classloaders, and to control what wrapper classes, if any, will be used for them.  That covers (as far as I think it worthwhile) the ''namespace'' aspect of classloader use.   The other part is the dynamic loading/unloading part, which is probably best described by example.

Say you are developing some new Java classes.  You would like to be able to test them out from Smalltalk, but there''s a problem -- whenever you change and recompile a Java class, you have to restart Dolphin (and JNIPort) or it will not ''see'' the changes.   SupplementaryClassloader are intended to help with this case (and with others involving dynamic use of classloaders, but this is the best example I can think of).  To do that, set up a SupplementaryClassloader called, say, #Development, and arrange that it''s #path points to the place where your Java development tools (IDE or whatever) put the generated classfiles (a folder or JARfile).  That place must not be on your main classpath or (as I said above) the classes will be loaded by the application classloader and then can never be unloaded again.  You may need to set up a more complicated tree of classloaders, but for this simple case, I imagine that is unlikely.  Now you can activate the SupplementaryClassloader, you may choose to have it do so by default (in the settings) or maybe you prefer to do it explicitly.  You can also arrange to create the SupplementaryClassloader as part of the JNIPort settings, or do so explicitly from code after the JVM has started, in this case I think the former makes more sense, but either is possible.

Once that is set up, you can load classes via the new classloader.  You can either say:
	jvm := JVM current.
	myClass := jvm findClass: ''my.test.Class'' in: #Development.
or you can ask for a reference to the SupplementaryClassloader itself,
	jvm := JVM current.
	loader := jvm supplementaryClassloaderNamed: #Development.
and use that directly as a <javaClassFinder>
	myClass := loader findClass: ''my.test.Class''.

If you have set up any wrapper classes for ''my.test.Class'' and arranged for the class''s #supplementaryClassloaderNames to include #Development, then those classes will be set up as wrappers from the newly loaded class.  (BTW, at this time the Wrapper Generator Wizzard is not SupplementaryClassloader-aware, so you will have to create the relevant overload of #supplementaryClassloaderNames yourself).

Now suppose that you change and re-compile your Java classes.  You now need to tell the #Development SupplementaryClassloader that it is out-of-date.  You do that by sending it #purge.  That will do several things:
	1) purge any child classloaders in the tree.
	2) remove any record of the classes it has loaded from JNIPort.
	3) discard its own classloader (so that it can be re-created lazily).
Now you can re-load the class by using #findClass: again, and JNIPort should now see the new versions of the classfiles.  If you are using Ghost Classes, then the ghost wrappers will be re-generated and will reflect any changes to the class definitions.

Note that JNIPort does not (not yet, anyway) actually ''kill'' the old class objects and their instances, so they will still hang around if you are holding onto references to them, and should continue to work more-or-less ''normally'' (but it would be better not to rely on that -- ''normal'' in this case probably means ''buggy'').  Also it is your responsibility to remove any callbacks that rely on those classes, JNIPort won''t do that for you.   

NB: currently the supplementary classloader framework lacks proper mutex protection.  The class indexes themselves are OK, but the tree structure, and the ''central instance'' in the JVM are not thread-safe.
'!
!SupplementaryClassloaderTree categoriesForClass!Unclassified! !
!SupplementaryClassloaderTree methodsFor!

addCopyOfEntry: aSupplementaryClassloader
	"private -- add a new entry configured by copying the data in aSupplementaryClassloader.
	Answers the new entry"

	^ (self addEntryNamed: aSupplementaryClassloader nameOrPath)
		copyFrom: aSupplementaryClassloader;
		yourself.!

addEntryNamed: aString
	"add a blank new entry to our list, it will have the given name (which /must/ not
	be modified thereafter) and be otherwise blank.
	Answers the new entry"

	| new |

	new := (SupplementaryClassloader new)
			name: aString	owner: self;
			yourself.

	entriesByName at: new name put: new.

	self notifyAdded: new.

	^ new.!

childrenOfEntry: aSupplementaryClassloader
	"answer a collection of the immediate dependents of the given entry"

	| name |

	name := aSupplementaryClassloader name.

	^ entriesByName asArray select: [:each | each parentName == name].!

configureFrom: aLocalJavaClasspathSettings
	"private -- add entries corresponding to those defined in aLocalJavaClasspathSettings"

	aLocalJavaClasspathSettings entries do: [:each | self addCopyOfEntry: each].!

entryHasChildren: aSupplementaryClassloader
	"answer whether the given entry has any dependent entries"

	| name |

	name := aSupplementaryClassloader name.

	^ entriesByName anySatisfy: [:each | each parentName == name].!

entryNamed: aStringOrSymbol
	"answer the entry named by aStringOrSymbol"

	^ self entryNamed: aStringOrSymbol ifAbsent: [self errorNotFound: aStringOrSymbol].
!

entryNamed: aStringOrSymbol ifAbsent: a0Block
	"answer the entry named by aStringOrSymbol or the result of evaluating a0Block if there is no
	such entry"

	^ entriesByName at: aStringOrSymbol asSymbol ifAbsent: a0Block.
!

entryWithClassloader: aJavaLangClassloader
	"answer the entry that uses the given JavaLangClassloader or throw an error if there isn't one"

	^ entriesByName detect: [:each | each classloaderIs: aJavaLangClassloader].!

entryWithClassloader: aJavaLangClassloader ifNone: a0Block
	"answer the entry that uses the given JavaLangClassloader, or the result
	of evaluating a0Block if there isn't one"

	^ entriesByName
		detect: [:each | each classloaderIs: aJavaLangClassloader]
		ifNone: a0Block.!

findClass: aClassName in: aName
	"answer a JavaStatic object for the class named by the String or Symbol aClassName,
	the classloader asked to find it will be the one corresponding to the given local
	classpath entry named by the String or Symbol aName"

	^ (self entryNamed: aName) findClass: aClassName.!

initialize
	"private -- establish a coherent initial state"

	entriesByName := IdentityDictionary new.!

jvm
	"answer our owning JVM"

	^ jvm.!

jvm: aJVM
	"set our owning JVM"

	jvm := aJVM.!

notifyAdded: aSupplementaryClassloader
	"private -- trigger change notification to say that the given entry has been added"

	self trigger: #supplementaryClassloaderAdded: with: aSupplementaryClassloader.!

notifyChanged: aSupplementaryClassloader
	"private -- trigger change notification say that the given entry has changed status in
	some way"

	self trigger: #supplementaryClassloaderChanged: with: aSupplementaryClassloader.!

notifyRemoved: aSupplementaryClassloader
	"private -- trigger change notification to say that the given entry has been removed"

	self trigger: #supplementaryClassloaderRemoved: with: aSupplementaryClassloader.!

purge
	"purge all of our entries"

	entriesByName do: [:each | each purge].
!

removeEntry: aSupplementaryClassloader
	"ensure we have no entry named by aString"

	self removeEntryNamed: aSupplementaryClassloader name.
!

removeEntryNamed: aString
	"ensure we have no entry named by aString"

	| gone |

	gone := entriesByName removeKey: aString asSymbol ifAbsent: [^ self].

	gone purge.
	self notifyRemoved: gone.
!

rootEntries
	"answer a list of the entries with no parents"

	^ entriesByName values asArray reject: [:each | each hasParent].
!

shutdown
	"private -- called when the JVM is shutting down.
	All we do is discard any runtime information, we don't attempt anything
	meaningfull in the way of cleaning up"

	entriesByName do: [:each | each shutdown].! !
!SupplementaryClassloaderTree categoriesFor: #addCopyOfEntry:!accessing!private! !
!SupplementaryClassloaderTree categoriesFor: #addEntryNamed:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #childrenOfEntry:!hierarchy!public! !
!SupplementaryClassloaderTree categoriesFor: #configureFrom:!initializing!public! !
!SupplementaryClassloaderTree categoriesFor: #entryHasChildren:!hierarchy!public! !
!SupplementaryClassloaderTree categoriesFor: #entryNamed:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #entryNamed:ifAbsent:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #entryWithClassloader:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #entryWithClassloader:ifNone:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #findClass:in:!Java classes!public! !
!SupplementaryClassloaderTree categoriesFor: #initialize!initializing!private! !
!SupplementaryClassloaderTree categoriesFor: #jvm!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #jvm:!initializing!public! !
!SupplementaryClassloaderTree categoriesFor: #notifyAdded:!events!private! !
!SupplementaryClassloaderTree categoriesFor: #notifyChanged:!events!private! !
!SupplementaryClassloaderTree categoriesFor: #notifyRemoved:!events!private! !
!SupplementaryClassloaderTree categoriesFor: #purge!public!purging! !
!SupplementaryClassloaderTree categoriesFor: #removeEntry:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #removeEntryNamed:!accessing!public! !
!SupplementaryClassloaderTree categoriesFor: #rootEntries!hierarchy!public! !
!SupplementaryClassloaderTree categoriesFor: #shutdown!private!purging! !

!SupplementaryClassloaderTree class methodsFor!

forJVM: aJVM
	"private -- answer a new instance which is owned by the given JVM and
	initially configured from its settings"

	^ (self jvm: aJVM)
		configureFrom: aJVM settings supplementaryClassloaders;
		yourself.
!

jvm: aJVM
	"answer a new instance which is owned by the given 	JVM and which has no
	entries"

	^ (self new)
		jvm: aJVM;
		yourself.!

new
	"private -- use #jvm:"

	^ (self basicNew)
		initialize;
		yourself.!

publishedEventsOfInstances
	"answers a Set of Symbols that describe the published events triggered
	by instances of the receiver."	

	#CUtodo.  "use TreeModel style notifications"
	^ (super publishedEventsOfInstances)
		add: #supplementaryClassloaderAdded;
		add: #supplementaryClassloaderChanged;
		add: #supplementaryClassloaderRemoved;
		yourself.
! !
!SupplementaryClassloaderTree class categoriesFor: #forJVM:!instance creation!private! !
!SupplementaryClassloaderTree class categoriesFor: #jvm:!instance creation!public! !
!SupplementaryClassloaderTree class categoriesFor: #new!instance creation!private! !
!SupplementaryClassloaderTree class categoriesFor: #publishedEventsOfInstances!constants!development!events!public! !

JavaException guid: (GUID fromString: '{53635895-BA67-4435-9F51-CA26BDE363ED}')!
JavaException comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These are used to hold a Java exception.  The ''tag'' is an instance of some subclass of JavaLangThrowable which, in turn, wraps (or is a proxy for) a reference to a Java java.lang.Thowable object.

It''s worth noting that you can trap Java exceptions (in Smalltalk) by doing something like:

	[...blah blah...]
		on: XXX
		do: [:err | ...]

where XXX can be any of:
	This class (i.e. normal Smalltalk exception trapping).
	JavaLangThrowable, or a subclass.
	The class Static of the Java class of the Java exception object.'!
!JavaException categoriesForClass!Unclassified! !
!JavaException methodsFor!

_descriptionArguments
	"answer an array of arguments to our #_descriptionFormat"

	^ Array
		with: self messageText
		with: self tag displayString.!

_descriptionFormat
	"answer the Win32 format String to be used to format the description of the receiver"
	
	^ '%1: %2'.!

cause
	"answer the underlying exception's 'cause' (i.e. the previous exception
	in the backchain).  NB: only available with J2SDK1.4 and later JVMs"

	^ tag cause.!

javaStackTrace
	"answer the underlying exception's array of java.lang.StackTraceElements.
	NB: only available with J2SDK1.4 and later JVMs"

	^ tag stackTrace.!

shortMessage
	"answer the underlying exception's so-called: 'short message'"

	^ tag message.! !
!JavaException categoriesFor: #_descriptionArguments!displaying!public! !
!JavaException categoriesFor: #_descriptionFormat!displaying!public! !
!JavaException categoriesFor: #cause!accessing!public! !
!JavaException categoriesFor: #javaStackTrace!accessing!public! !
!JavaException categoriesFor: #shortMessage!accessing!public! !

NoJavaSuperclassException guid: (GUID fromString: '{9C6FCFEC-60B2-46DE-9DFC-6B76A3B4189A}')!
NoJavaSuperclassException comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Thrown if you send #super to a JavaClassInstance that is an instance of java.lang.Object (and so has no ''super'').  Also see the class comment for JavaNonvirtual.'!
!NoJavaSuperclassException categoriesForClass!Unclassified! !
JavaMainClassIndex guid: (GUID fromString: '{4FF997EA-CC35-4E48-88D6-3FD2AFDB0FD8}')!
JavaMainClassIndex comment: 'Copyright © Chris Uppal, 2001-2005.
chris.uppal@metagnostic.org

This is slightly enhanced version of a JavaClassIndex wihch is used as the main index by the JVM, there is one of these per JVM.

This is used to hold classes that have been found by name without using an explicit classloader.'!
!JavaMainClassIndex categoriesForClass!Unclassified! !
!JavaMainClassIndex methodsFor!

javaLangClass
	"answer the JavaStatic corresponding to java.lang.Class"

	"the class registry knows about this anyway, so let it do the work"
	^ self jvm classRegistry javaLangClass.!

javaLangObject
	"answer the JavaStatic corresponding to java.lang.Object"

	"the class registry knows about this anyway, so let it do the work"
	^ self jvm classRegistry javaLangObject.!

javaLangString
	"answer the JavaStatic corresponding to java.lang.String"

	"the class registry knows about this anyway, so let it do the work"
	^ self jvm classRegistry javaLangString.
!

jvm: aJVM
	"private -- set the JVM we will use"

	jvm := aJVM.
! !
!JavaMainClassIndex categoriesFor: #javaLangClass!accessing!public! !
!JavaMainClassIndex categoriesFor: #javaLangObject!accessing!public! !
!JavaMainClassIndex categoriesFor: #javaLangString!accessing!public! !
!JavaMainClassIndex categoriesFor: #jvm:!initializing!private! !

!JavaMainClassIndex class methodsFor!

defaultCapacity
	"private -- answer the default capacity to use for instances"

	^ 50.!

newWithJVM: aJVM
	"answer a new instance which uses aJVM to find previously unknown classes"

	^ (self newFor: aJVM)
		jvm: aJVM;
		yourself.! !
!JavaMainClassIndex class categoriesFor: #defaultCapacity!constants!private! !
!JavaMainClassIndex class categoriesFor: #newWithJVM:!instance creation!public! !

JavaInstance guid: (GUID fromString: '{FD24011E-F247-4A42-B0F2-97FC01B00EF7}')!
JavaInstance comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class that captures the commonality amongst a number of subclasses, each of which represents a proxy (in some sense, that depends on the subclass) for a Java object.

The main points of this class are to expose some reflection methods, and to provide default wrapper methods for all the methods defined on java.lang.Object.  (Since *all* Java objects inherit from java.lang.Object, it is always possible to call, say, toString() in Java, even if the reference is declared to be of an interface type that doens''t obviously provide toString().  To emulate that we define wrappers for those methods here, rather than down in the JavaLangObject subclass where you might expect to find them).'!
!JavaInstance categoriesForClass!Unclassified! !
!JavaInstance methodsFor!

= aJavaInstance
	"answer whether the receiver's underlying Java object is identical to that of aJavaInstance"

	self == aJavaInstance ifTrue: [^ true].

	self isDead ifTrue: [^ false].

	"NB: we don't have the usual class comparsion here because the mapping from Java objects to
	Smalltalk classes is not straightforward"
	(aJavaInstance isKindOf: JavaInstance) ifFalse: [^ false].

	self static = aJavaInstance static ifFalse: [^ false].

	^ self isSameAs: aJavaInstance.!

allFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this includes fields inherited from superclasses.  Fields from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	^ static allInstanceFields.!

allMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of our Java object's instance-side
	methods, this includes methods inherited from superclasses.  Methods from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	^ static allInstanceMethods.!

allRealFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this includes fields *genuinely* inherited from superclasses.  Fields from superclasses appear before those
	from derived classes, but are not otherwise in any special order"

	"instances only have 'real' fields, and all inherited fields are real, so..."
	^ self allFields.!

asA: aJavaClassName
	"answer a JavaInterfaceInstance wrapping the same underling object as ourselves, but with
	the class given by JavaClassName..
	NB: there is no check that our underlyng object actually implements the interface defined
	by the Java class.  In particular, if the Java class is an interface (the normal case) then we
	don't check that the object formally implements that interface.  In fact if it does not, but still
	happens to define the relevant methods, then this will still work OK (i.e. we take a Smalltalk
	style approach to type-checking!!)"

	^ self asInstanceOf: (self jvm findClass: aJavaClassName).!

asAn: aJavaClassName
	"many of my friends, and even some of my *best* friends, are pedants"

	^ self asA: aJavaClassName.!

asInstanceOf: aJavaStatic
	"answer a JavaInterfaceInstance wrapping the same underling object as ourselves, but with
	the given Java class static.  I.e. appearing to have a different Java class -- typically a Java interface
	class object.
	Note: this won't allow you to convert a JavaInterfaceInstance back to a JavaClassInstance
	since that would evade the management machinery, use #asYourself for that"

	^ aJavaStatic instanceClass
		wrap: self managedInstance
		asStatic: aJavaStatic.!

asYourself
	"answer the real JavaObject underlying whatever view we have of it"

	^ self managedInstance.!

beNotCanonical
	"ensure that this instance is not 'canonical'"

	"only JavaClassInstances can be canonical, so we can ignore this"!

displayOn: aStream
	"append a user-oriented representation of our underlying Java object to aStream"

	"actually, this isn't all that 'user-oriented' since Java programmers typically use toString()
	more like our #printString; but that's life..."
	self isDead
		ifTrue: [aStream nextPutAll: '<dead>']
		ifFalse: [aStream display: self toString].
!

equals: aJavaObject
	"answer the result of invoking our Java object's equals() method with aJavaObject
	as it's argument"

	"pick up the ghost implementation if it is installed (faster)"
	^ self equals_Object: aJavaObject.
!

equals_Object: aJavaObject
	"answer the result of calling the receiver's public equals(java.lang.Object) Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aJavaObject;
			yourself.

	^ self callBooleanMethod: 'equals' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

fields
	"answer an OrderedCollection of JavaLangReflectField objects for each of our Java object's instance-side
	fields, this does not includes fields inherited from superclasses.  They are in no special order"

	^ static instanceFields.!

getClass
	"answer the JavaLangClass representing the receiver's Java class.  This is just exposing the
	Java getClass() method (and note that it answers the class object, not the corresponding
	class static).  For Instances of JavaClassInstance, that will be the same as #javaClassObject,
	but for interface instances and non-virtuals, it will answer the real class of the underlying
	Java object, not the class that the adaptor object is 'pretending' to be"

	"pick up the ghost implementation if it is installed (faster)"
	^ self getClass_null.!

getClass_null
	"answer the result of calling the receiver's public final native getClass() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	^ self callObjectMethod: 'getClass' signature: '()Ljava/lang/Class;'.
!

hash
	"answer the underlying javaObject's identity hash"

	self isDead ifTrue: [^ super hash].	"i.e. the hash value'll change when we die, but that's better than causing walkbacks"

	^ self jvm identityHash: self.!

hashCode
	"answer the result of invoking our Java object's hashCode() method"

	"pick up the ghost implementation if it is installed (faster)"
	^ self hashCode_null.!

hashCode_null
	"answer the result of calling the receiver's public native hashCode() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	^ self callIntMethod: 'hashCode'.
!

isCanonical
	"answer true iff this instance is 'canonical' -- i.e. is the sole object that will be
	used to represent the underlying Java object"

	"only JavaClassInstances can be canonical"
	^ false.!

isInstanceOf: aJavaStatic
	"answer whether we are an instance of any class whish is derived from the Java class or interface
	represented by aJavaStatic"

	^ aJavaStatic classObject isInstance: self.!

javaClassObject
	"answer the JavaLangClass representing the receiver's Java class"

	^ static classObject.
!

jniEnv
	"answer the receiver's (current) JNIEnv"

	"overridden for speed"
	^ static jniEnv.
!

jvm
	"answer the receiver's owning JVM"

	^ static jvm.
!

methods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of our Java object's instance-side
	methods, this does not includes methods inherited from superclasses.  They are in no special order"

	^ static instanceMethods.!

notify_null
	"invoke the receiver's public final native notify() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	self callVoidMethod: 'notify'.
!

notifyAll_null
	"invoke the receiver's public final native notifyAll() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	self callVoidMethod: 'notifyAll'.
!

static
	"answer the JavaStatic representing the receiver's Java class"

	^ static.
!

static: aJavaStatic
	"private -- set the pointer to the JavaStatic object representing our Java class"

	static := aJavaStatic.!

super
	"answer a non-virtual 'view' of our subject from the point-of-view of the superclass of the class
	we actually belong to.
	Note that you can navigate further up the hierarchy by doing:
		anObject super super.
	and so on.  (Or just create a JavaNonvirtual with the desired JavaStatic directly).
	Throws a NoJavaSuperclassException if the receiver has no superclass"

	^ JavaNonvirtual
		wrap: (self managedInstance)
		asStatic: (static javaSuperclassIfNone: [NoJavaSuperclassException signal: 'No superclass' with: static]).!

toString
	"answer the result of invoking our Java object's toString() method"

	"pick up the ghost implementation if it is installed (faster)"
	^ self toString_null.
!

toString_null
	"answer the result of calling the receiver's public toString() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
!

wait_long: anInteger
	"invoke the receiver's public final native wait(long) Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: anInteger;
			yourself.

	self callVoidMethod: 'wait' signature: '(J)V' withArguments: args.
!

wait_long: anInteger int: anotherInteger
	"invoke the receiver's public final wait(long, int) Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	| args |

	args := (JNIValueArray new: 2)
			longAt: 1 put: anInteger;
			intAt: 2 put: anotherInteger;
			yourself.

	self callVoidMethod: 'wait' signature: '(JI)V' withArguments: args.
!

wait_null
	"invoke the receiver's public final wait() Java method.
	Provided here, rather than on JavaLangObject so that everything can respond to the
	basic Java Object methods (as happens in Java)."

	self callVoidMethod: 'wait'.
! !
!JavaInstance categoriesFor: #=!comparing!public! !
!JavaInstance categoriesFor: #allFields!public!reflection! !
!JavaInstance categoriesFor: #allMethods!public!reflection! !
!JavaInstance categoriesFor: #allRealFields!public!reflection! !
!JavaInstance categoriesFor: #asA:!converting!public! !
!JavaInstance categoriesFor: #asAn:!converting!public! !
!JavaInstance categoriesFor: #asInstanceOf:!converting!public! !
!JavaInstance categoriesFor: #asYourself!converting!public! !
!JavaInstance categoriesFor: #beNotCanonical!managed objects!modes!public! !
!JavaInstance categoriesFor: #displayOn:!displaying!public! !
!JavaInstance categoriesFor: #equals:!comparing!public! !
!JavaInstance categoriesFor: #equals_Object:!Java-methods!public! !
!JavaInstance categoriesFor: #fields!public!reflection! !
!JavaInstance categoriesFor: #getClass!accessing!public! !
!JavaInstance categoriesFor: #getClass_null!Java-methods!public! !
!JavaInstance categoriesFor: #hash!comparing!public! !
!JavaInstance categoriesFor: #hashCode!comparing!public! !
!JavaInstance categoriesFor: #hashCode_null!Java-methods!public! !
!JavaInstance categoriesFor: #isCanonical!managed objects!public!testing! !
!JavaInstance categoriesFor: #isInstanceOf:!public!testing! !
!JavaInstance categoriesFor: #javaClassObject!accessing!public! !
!JavaInstance categoriesFor: #jniEnv!accessing!public! !
!JavaInstance categoriesFor: #jvm!accessing!public! !
!JavaInstance categoriesFor: #methods!public!reflection! !
!JavaInstance categoriesFor: #notify_null!Java-methods!public! !
!JavaInstance categoriesFor: #notifyAll_null!Java-methods!public! !
!JavaInstance categoriesFor: #static!accessing!public! !
!JavaInstance categoriesFor: #static:!initializing!private! !
!JavaInstance categoriesFor: #super!converting!public! !
!JavaInstance categoriesFor: #toString!converting!public! !
!JavaInstance categoriesFor: #toString_null!Java-methods!public! !
!JavaInstance categoriesFor: #wait_long:!Java-methods!public! !
!JavaInstance categoriesFor: #wait_long:int:!Java-methods!public! !
!JavaInstance categoriesFor: #wait_null!Java-methods!public! !

!JavaInstance class methodsFor!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	Override in subclasses, to force this"

	"we don't have any real instances; overriden in JavaClassInstance and below"
	^ false.!

icon
	"answer an Icon representing the receiver"

	^ JVM icon: (self isGhostClass ifTrue: ['JavaInstance-Ghost'] ifFalse: ['JavaInstance']).!

registerWith: aClassFinder
	"this is called as JVM is initialised, or with a SupplementaryClassloader, and gives
	us the chance (should we wish to accept it) to register ourself with it as a wrapper
	class"

	"set the instance class associated with our Java name to this class"
	[(aClassFinder findClass: self javaClassName) changeInstanceClassTo: self]
		on: JavaException
		do: [:ex | ex notify].
! !
!JavaInstance class categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!JavaInstance class categoriesFor: #icon!constants!ghost classes!public! !
!JavaInstance class categoriesFor: #registerWith:!initializing!public!registering wrapper classes! !

JavaStatic guid: (GUID fromString: '{B3C85F86-282F-4705-B092-A4D354C269F8}')!
JavaStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

A JavaStatic (usually referred to as a "class static", or just a "static") is an object that stands for a Java class.  It acts as a "proxy" for the class in the sense that the static methods and fields of the Java class are exposed via methods on these objects.

Each instance is paired with a normal (instance-side) proxy for the actual java.lang.Class object that stands for the class in Java space.  That object is accessed by #classObject.

All JavaInstances know which JavaStatic they belong to, e.g. all wrappers for java.lang.String objects (which are all instances of JavaLangString) know that they are owned by the specific instance of JavaStatic that corresponds to java.lang.String (actually that instance will be the only instance of our subclass StaticJavaLangString).  You can get the owning class static of any class instance by sending it the #static message.'!
!JavaStatic categoriesForClass!Unclassified! !
!JavaStatic methodsFor!

= aJavaStatic
	"answer whether the receiver is = (in Java terms, this is an identity comparison) to aJavaStatic"

	self == aJavaStatic ifTrue: [^ true].

	self isDead ifTrue: [^ false].

	"NB: we don't have the usual class comparsion here because the mapping from Java objects to
	Smalltalk classes is not straightforward"
	(aJavaStatic isKindOf: JavaStatic) ifFalse: [^ false].

	^ self classObject = aJavaStatic classObject.!

abstractMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of this Java class's
	abstract methods"

	^ (classObject declaredMethods select: [:each | each isAbstract]) asOrderedCollection.!

abstractMethodsPlusInterfaceMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of this Java class's abstract methods,
	this includes (abstract) methods inherited from our declared interfaces (and their superinterfaces).
	It does not includes methods inherited from our superclass.
	The answer is in no particular order.
	P.S: Sorry about the method name !!"

	| methods |

	methods := self abstractMethods.

	"want to be carefull to avoid duplicates"
	self interfaces do:
		[:interface | interface abstractMethodsPlusInterfaceMethods do:
			[:method | (methods anySatisfy: [:each | each overridesOrDuplicates: method])
					ifFalse: [methods add: method]]].

	^ methods.!

allAbstractMethods
	"answer an OrderedCollection of JavaLangReflectMethd objects for each of this Java class's abstract methods,
	including those inherited from our superclasses, and interfaces.
	Note that this does NOT exclude abstract methods that have been implemented in some sub-(or super)-class.
	The result is in no special order"

	| methods |

	methods := self abstractMethodsPlusInterfaceMethods.

	javaSuperclass isNil ifTrue: [^ methods].

	"want to be carefull to avoid duplicates"
	javaSuperclass allAbstractMethods do:
		[:method | (methods anySatisfy: [:each | each overridesOrDuplicates: method])
				ifFalse: [methods add: method]].

	^ methods.!

allFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of this Java class's class-side fields,
	this includes fields 'inherited' (technically, just 'in scope') from superclasses and interfaces.
	The order is undefined for fields within an class or interface, but otherwise follows the order:
		fields inherited from interfaces of our root class
		fields inherited from our root class
		...
		fields inherited from interfaces of our superclass
		fields inherited from our superclass
		fields inherited from our interfaces (including their super-interfaces)
		our own declared fields
	Fields that are inherited several times (from an interface being included more than
	once) are included in the *last* relevant position"

	| fields |

	fields := javaSuperclass isNil
			ifTrue: [OrderedCollection new]
			ifFalse: [javaSuperclass allFields].

	self fieldsPlusInterfaceFields do:
		[:field | fields
				remove: field ifAbsent: [];
				addLast: field].

	^ fields.
!

allInstanceFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of this Java class's instance-side fields,
	this includes fields 'inherited' from superclasses.  Note that we include private fields, even though these are, technically,
	not inherited.
	The order is undefined for fields within a class, but otherwise is ordered with fields from superclasses
	before those from derived classes"

	| fields |

	fields := javaSuperclass isNil
			ifTrue: [OrderedCollection new]
			ifFalse: [javaSuperclass allInstanceFields].

	fields addAll: self instanceFields.

	^ fields.
!

allInstanceMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of this Java class's concrete instance-side
	methods, this includes methods inherited from superclasses.  Note that we include private methods, even though
	these are, technically, not inherited.
	Methods from superclasses that are overriden in subclasses are NOT included.
	Please note: this does *not* include abstract methods.
	The order is undefined for methods within a class, but otherwise is ordered with methods from superclasses
	before those from derived classes"

	| methods |

	methods := self instanceMethods.

	javaSuperclass isNil ifTrue: [^ methods].

	methods addAll: (javaSuperclass allInstanceMethods reject: [:each | methods anySatisfy: [:other | other overrides: each]]).

	^ methods reverse.!

allInterfaces
	"answer an OrderedCollection of JavaLangClass objects for each interface this Java class implements
	including those inherited from superclasses.  They are in no special order"

	| interfaces |

	interfaces := Set new.
	self withAllJavaSuperclassesDo: [:each | interfaces addAll: each interfacesWithAllSuperinterfaces].

	^ interfaces asOrderedCollection.!

allJavaSuperclasses
	"answer an OrderedCollection of this class's Java super classes in order"

	| classes |

	classes := OrderedCollection new.
	self allJavaSuperclassesDo: [:c | classes addLast: c].

	^ classes.!

allJavaSuperclassesDo: a1Block
	"evaluate a1Block for each of this class's Java superclasses in order"

	^ self javaSuperclassDo: [:it | it withAllJavaSuperclassesDo: a1Block].!

allMethods
	"answer an OrderedCollection of JavaLangReflectMethod objects for each of this Java class's class-side
	methods, this includes methods 'inherited' (technically, just 'in scope') from superclasses.
	The order is undefined for methods within a class, but otherwise is ordered with methods from superclasses
	before those from derived classes"

	| methods |

	methods := javaSuperclass isNil
			ifTrue: [OrderedCollection new]
			ifFalse: [javaSuperclass allMethods].

	methods addAll: self methods.

	^ methods.
!

allRealFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of this Java class's class-side fields,
	plus those *genuinely* inherited from superclasses (i.e. none)."

	^ self fields.!

allUndefinedMethods
	"answer an OrderedCollection of JavaLangReflectMethd objects for each of this Java class's abstract methods,
	including those inherited from our superclasses and interfaces, that have not been given a concrete implementation
	in this class or one of our supers.
	The result is in no special order"

	| abstract concrete |

	abstract := self allAbstractMethods.
	abstract isEmpty ifTrue: [^ abstract].

	concrete := self allInstanceMethods.
	^ abstract reject: [:each | concrete anySatisfy: [:other | other overrides: each]].!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ self class arrayClass.!

asJavaLangClass
	"answer the receiver converted to a class object"

	^ classObject.!

asStatic
	"answer the receiver converted to a class static"

	^ self.!

callBooleanMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	| answer |

	answer := self jniEnv
			CallStaticBooleanMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].

	^ answer == 1.
!

callByteMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallStaticByteMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].!

callCharMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			CallStaticCharMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].
!

callDoubleMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallStaticDoubleMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].
!

callFloatMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallStaticFloatMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].!

callIntMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallStaticIntMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].!

callLongMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallStaticLongMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].!

callObjectMID: aMethodID withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			CallStaticObjectMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

callShortMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^self jniEnv
			CallStaticShortMethodA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | jvm throwJavaException: ex].!

callVoidMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self jniEnv
		CallStaticVoidMethodA_class: self jniObject
		methodID: aMethodID
		args: aJNIValueArray
		onException: [:ex | jvm throwJavaException: ex].
!

canBePurged
	"private -- answer whether the receiver is purgeable"

	^ true.!

changeInstanceClassTo: aClass
	"private -- change to using the given class as our instance class.
	This is called during bootstrapping (as classes register themselves)
	and as ghost classes are created"

	self sharedMutex critical: [self unsafeChangeInstanceClassTo: aClass].!

changeStaticClassTo: aClass
	"private -- change to using the given class as our static class.
	This is called during bootstrapping (as classes register themselves)
	and as ghost classes are created.
	NB: since we are an instance of the class that's being changed, this will
	change our class"

	self sharedMutex critical: [self unsafeChangeStaticClassTo: aClass].
!

classloader
	"answer the classloader that loaded our class"

	^ classObject classloader.!

classObject
	"answer the JavaLangClass instance corresponding to this Java class"

	^ classObject.!

classObject: aJavaLangClass
	"private -- set the JavaLangClass that defines the Java class we stand for"

	classObject := aJavaLangClass.
!

classRegistry
	"answer the JavaClassRegistry wherin the receiver is lodged"

	^ jvm classRegistry.
!

constructors
	"answer a OrderedCollection of JavaLangReflectConstructor objects for each of the Java class's constructors.
	NB: since constructors are not inherited, there is no #allConstructors method"

	^ OrderedCollection withAll: classObject declaredConstructors.!

couldBeSubstituted
	"answer true if we represent a class for which the JVM rules do not allow any other
	to be substituted at runtime (e.g. final classes)"

	self subclassResponsibility.!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	^ self class defaultInstanceClass.
!

description
	"answer a short textual description of this class, suitable for use as, say, an info tip"

	| stream |

	classObject isDead ifTrue: [^ ''].

	stream := String writeStream.

	self writeDescriptionOn: stream.

	^ stream contents.
!

displayOn: aStream
	"append a user-oriented representation of our underlying Java object to aStream"

	classObject isDead ifTrue: [^ super displayOn: aStream].

	aStream nextPutAll: self name.
!

fields
	"answer an OrderedCollection of JavaLangReflectField objects for each of this Java class's class-side fields"

	^ (classObject declaredFields select: [:each | each isStatic]) asOrderedCollection.
!

fieldsPlusInterfaceFields
	"answer an OrderedCollection of JavaLangReflectField objects for each of this Java class's class-side fields,
	this includes fields 'inherited' (technically, just 'in scope') from our declared interfaces (and their superinterfaces).
	It does not includes fields inherited from our superclass.
	Fields inherited from our interfaces appear before our own fields, but the order is otherwise undefined.
	P.S: Sorry about the method name !!"

	| fields |

	fields := OrderedCollection new.

	self interfaces do:
		[:interface | interface allFields do:
			[:field | fields
					remove: field ifAbsent: [];
					addLast: field]].

	fields addAll: self fields.

	^ fields.!

findField: aStringName signature: aJNISignature
	"answer the JavaFieldID corresponding to our Java object's field named aStringName,
	with signature defined by aJNISignature"

	^ self jniEnv
		GetStaticFieldID_class: self jniObject
		name: aStringName
		sig: aJNISignature
		onException: [:ex | jvm throwJavaException: ex].
!

findInstanceMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java class object's instance method
	named aStringName, and signature defined by aJNISignature"

	^ self jniEnv
		GetMethodID_class: self jniObject
		name: aStringName
		sig: aJNISignature
		onException: [:ex | jvm throwJavaException: ex].

!

findMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java class object's static method
	named aStringName, and signature defined by aJNISignature"

	^ self jniEnv
		GetStaticMethodID_class: self jniObject
		name: aStringName
		sig: aJNISignature
		onException: [:ex | jvm throwJavaException: ex].

!

getBooleanFID: aFieldID
	"get the value of the field defined by aFieldID"

	| answer |

	answer := self jniEnv
			GetStaticBooleanField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].

	^ answer == 1.!

getByteFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticByteField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getCharFID: aFieldID
	"get the value of the field defined by aFieldID
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			GetStaticCharField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].!

getDoubleFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticDoubleField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getFloatFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticFloatField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getIntFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticIntField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getLongFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticLongField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getObjectFID: aFieldID wrapperFactory: aJavaStatic
	"get the value of the field defined by aFieldID.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			GetStaticObjectField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].

	^ aJavaStatic wrapJNIObject: answer.!

getShortFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetStaticShortField_class: self jniObject
			fieldID: aFieldID
			onException: [:ex | jvm throwJavaException: ex].!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the field of this type defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field.  We are overriden in
	the primitive classes)"

	^ aJavaObject getObjectFID: aJNIFieldID.!

hasAnyCanonicalInstances
	"answer whether any instances are currently canonical.
	(Also see #hasCanonicalInstances)"

	"we don't have instances!!"
	^ false.
!

hasCanonicalInstances
	"answer whether *all* instances are automatically made canonical.
	(Also see #hasAnyCanonicalInstances)"

	"we don't have instances!!"
	^ false.!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later)"

	"we never have instances; overriden in subclasses that do"
	^ false.!

hash
	"answer the underlying javaObject's identity hash"

	^ classObject hash.!

icon
	"answer an Icon representing the receiver"

	^ self isGhost
		ifTrue: [instanceClass isGhostClass
			ifTrue: [JVM icon: 'JavaStatic-Ghost']
			ifFalse: [JVM icon: 'JavaStatic-GhostStatic']]
		ifFalse: [instanceClass isGhostClass
			ifTrue: [JVM icon: 'JavaStatic-GhostInstanceClass']
			ifFalse: [JVM icon: 'JavaStatic']].!

inheritanceString
	"answer a short textual description of this classes inheritance"

	| stream |

	classObject isDead ifTrue: [^ ''].

	stream := String writeStream.

	self writeInheritanceOn: stream.

	^ stream contents.
!

inheritanceStringWithStatus
	"answer a short textual description of this classes inheritance and current status"

	| stream |

	classObject isDead ifTrue: [^ ''].

	stream := String writeStream.

	self writeStatusOn: stream.
	self writeInheritanceOn: stream.

	^ stream contents.
!

initialize
	"private -- establish a coherent initial state"

	super initialize.
	instanceClass := self defaultInstanceClass.
!

initializeFrom: aJavaStatic
	"private -- copy as much data as possible from aJavaStatic.
	This is called during bootstrapping and conversion of clases to ghosts, so aJavaStatic
	will typically be of a *superclass* of the receiver.
	Should be overriden by subclasses that may need to copy extra data"

	super initialize.

	jvm := aJavaStatic jvm.
	classObject := aJavaStatic classObject.
	javaSuperclass := aJavaStatic javaSuperclass.
	instanceClass := aJavaStatic instanceClass.!

instanceClass
	"answer the Smalltalk class to use for objects representing instances
	of our Java class"

	^ instanceClass.!

instanceClass: aSubclassOfJavaClassInstance
	"private -- set the Smaltalk class to use to represent objects which are instance of this
	Java class"

	instanceClass := aSubclassOfJavaClassInstance.
!

instanceFields
	"answer a Collection of JavaLangReflectField objects for each of this Java class's instance-side fields"

	^ (classObject declaredFields select: [:each | each isStatic not]) asOrderedCollection.
!

instanceMethods
	"answer a Collection of JavaLangReflectMethod objects for each of this Java class's concrete
	instance-side methods.
	Please note: this does *not* include abstract methods"

	^ (classObject declaredMethods select: [:each | each isStaticOrAbstract not]) asOrderedCollection.
!

interfaces
	"answer an OrderedCollection of JavaInterfaceStatic objects for each interface this Java
	class directly declares itself to implement, in the order in which the were declared.
	(Java interfaces actually use the keyword 'extends' rather than 'implements', but it comes
	to the same thing)"

	^ (classObject declaredInterfaces collect: [:each | each classStatic]) asOrderedCollection.!

interfacesWithAllSuperinterfaces
	"answer an OrderedCollection of JavaLangClass objects for each interface this Java class implements
	directly or inherits via one of those interfaces.  They are in no special order"

	| interfaces |

	interfaces := Set new.
	self interfaces do: [:each | interfaces
					add: each;
					addAll: each interfacesWithAllSuperinterfaces].

	^ interfaces asOrderedCollection.!

isAbstract
	"answer whether we stand for an abstract class"

	^ classObject isAbstract.!

isArray
	"answer whether we stand for an array class"

	"overridden in JavaArrayClassStatic"
	^ false.!

isDerivedFrom: aJavaStatic
	"answer whether this class or interface is derived from (is a subclass of, or implements
	the interface defined by) aJavaStatic"

	^ classObject isDerivedFrom: aJavaStatic classObject.!

isFinal
	"answer whether we stand for an final class"

	^ classObject isFinal.!

isInterface
	"answer whether we stand for an Java interface 'class'"

	"overridden in JavaInterfaceStatic"
	^ false.!

isPrimitive
	"answer whether we stand for an Java primitive class (int, float, etc)"

	"overridden in JavaPrimitiveStatic"
	^ false.!

isStrict
	"answer whether we stand for an strict FP class"

	^ classObject isStrict.!

isSubclassOf: aJavaStatic
	"answer whether this Java class includes the Java class defined by aJavaStatic in
	its inheritance chain.
	Note: this does *not* include interfaces"

	"default implementation, overridden for real classes"
	^ false.!

isVoid
	"answer whether we stand for the Java primitive type void"

	"overridden in JavaPrimitiveVoidStatic"
	^ false.!

javaSuperclass
	"answer the JavaStatic corresponding to our Java superclass or nil
	if we don't have one"

	^ javaSuperclass.!

javaSuperclass: aJavaStatic
	"private -- set the record of our Java superclass"

	javaSuperclass := aJavaStatic.
	javaSuperclass notNil ifTrue: [instanceClass := javaSuperclass instanceClass].
!

javaSuperclassDo: a1Block
	"if we have a Java superclass then answer the result of evaluating a1Block with it
	as the parameter"

	^ javaSuperclass isNil ifTrue: [nil] ifFalse: [a1Block value: javaSuperclass].!

javaSuperclassIfNone: a0Block
	"answer the JavaStatic corresponding to our Java superclass, or the result of evaluating
	the <niladicValuable>, a0Block, if there isn't one"

	^ javaSuperclass ifNil: [a0Block value].!

jniEnv
	"answer the receiver's (current) JNIEnv"

	"overridden for speed"
	^ jvm jniEnv.
!

jniSignature
	"answer a JNI-style signature for this class.
	(note that this is a fairly expensive operation)"

	^ self name asJNISignature.!

jvm
	"answer the receiver's owning JVM"

	^ jvm.
!

jvm: aJVM
	"private -- set the JVM that we, and all our 'instances' will use"

	jvm := aJVM.
!

knownDirectJavaSubclasses
	"answer an OrderedCollection of all the *known* direct subclasses of this class"

	^ self classRegistry knownDirectSubclassesOf: self.!

knownJavaSubclasses
	"answer an OrderedCollection of all the *known* subclasses of this class"

	^ self classRegistry knownSubclassesOf: self.!

managedInstance
	"answer the managed object (a JavaClassInstance) which really owns the javaObject we wrap"

	^ classObject.!

methods
	"answer a OrderedCollection of JavaLangReflectMethod objects for each of this Java class's class-side
	methods"

	^ (classObject declaredMethods select: [:each | each isStatic]) asOrderedCollection.
!

name
	"answer the name of this class (Note that names are not necessarily
	unique, even within one JVM)"

	^ classObject name.!

nameAndModifiers
	"answer a String consisting of the name and modifiers of our Java class"

	| mods |

	mods := classObject modifiersString.
	^ mods isEmpty
		ifTrue: [self name]
		ifFalse: [mods , ' ' , self name].

!

newArray: size
	"answer a new array of instances of this Java class with all elements set to the default value
	(i.e. some flavour of zero or null)"

	^ self arrayClass new: size elementClass: self.
!

newArrayWith: anObject
	"answer a new array of instances of this Java class with 1 element which is anObject"

	^ self arrayClass newWith: anObject elementClass: self.
!

newArrayWith: anObject1 with: anObject2
	"answer a new array of instances of this Java class with the given elements"

	^ self arrayClass newWith: anObject1 with: anObject2 elementClass: self.
!

newArrayWith: anObject1 with: anObject2 with: anObject3
	"answer a new array of instances of this Java class with the given elements"

	^ self arrayClass newWith: anObject1 with: anObject2  with: anObject3 elementClass: self.
!

newArrayWithAll: aList
	"answer a new array of instances of this Java class with the given elements"

	^ self arrayClass newWithAll: aList elementClass: self.
!

notifyRegistered
	"this is called by the class registry once we have been fully initialised.
	Normally that means that the instance class and static class are both
	correct and stable (will not change unless you, the programmer, manually
	create and register a new wrapper class that is more appropriate for the
	Java class we represent.  If ghost classes are in use then we are fully
	populated with ghost methods by the time this is called too"

	"default is not to be interested"
!

purge
	"private -- remove this class object from the main registries"

	self isLive ifFalse: [^ self].

	#CUtodo.  "should we #free the class object ?"
	#CUtodo.  "should we #free our instances (and similar) ?"

	self knownDirectJavaSubclasses do: [:each | each purge].
	self jvm classIndex purge: self.
	self classRegistry purge: self.
!

replaceInstanceClass: anOldClass by: aNewClass
	"private -- if we are using anOld class as our instance class, then change to use aNewClass"

	"remember our new instance class"
	instanceClass == anOldClass ifTrue:
		[instanceClass := aNewClass].

	"NB: it is arguable that we should send becomeA: to all our known instances at this point; however
	the only class with instances that mutates in this way is java.lang.Class (and only during bootstrapping
	at that) so that case is handled separately, see the override in StaticJavaLangClass)"
"	(anOldClass allInstances
		select: [:each | each javaClass == self])
			do: [:each | each becomeA: aNewClass].		"
!

replaceStaticClass: anOldClass by: aNewClass
	"private -- if we are using anOld class as our class static class (i.e. if we
	are an instance of it), then change be an instance of aNewClass instead"

	self class == anOldClass ifTrue:
		[self swappingBecome: (aNewClass newAsCopyOf: self)].
!

resolve
	"if we are some sort of lazy stub (such as are used in lazy ghost classes) then
	resolve and replace ourself with the real thing.
	Answer the resolved reciever (which may not be the original reciever!!)"

	"we are the real thing, so ignore it..."!

setBooleanFID: aFieldID to: aBool
	"set the value of the field defined by aFieldID to aBoolean"

	self jniEnv
			SetStaticBooleanField_class: self jniObject
			fieldID: aFieldID
			val: (aBool ifTrue: [1] ifFalse: [0])
			onException: [:ex | jvm throwJavaException: ex].

	^ aBool.!

setByteFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetStaticByteField_class: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | jvm throwJavaException: ex].

	^ anInteger.!

setCharFID: aFieldID to: aCharacterOrInteger
	"set the value of the field defined by aFieldID to aCharOrInteger"

	self jniEnv
			SetStaticCharField_class: self jniObject
			fieldID: aFieldID
			val: aCharacterOrInteger asInteger
			onException: [:ex | jvm throwJavaException: ex].

	^ aCharacterOrInteger.!

setDoubleFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetStaticDoubleField_class: self jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | jvm throwJavaException: ex].

	^ aFloat.!

setFloatFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetStaticFloatField_class: self jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | jvm throwJavaException: ex].

	^ aFloat.!

setIntFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetStaticIntField_class: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | jvm throwJavaException: ex].

	^ anInteger.
!

setLongFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetStaticLongField_class: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | jvm throwJavaException: ex].

	^ anInteger.!

setObjectFID: aFieldID to: aJavaObjectOrNil
	"set the value of the field defined by aFieldID to aJavaObjectOrNil"

	self jniEnv
			SetStaticObjectField_class: self jniObject
			fieldID: aFieldID
			val: aJavaObjectOrNil asParameter
			onException: [:ex | jvm throwJavaException: ex].

	^ aJavaObjectOrNil.!

setShortFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetStaticShortField_class: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | jvm throwJavaException: ex].

	^ anInteger.!

setValueOfFID: aJNIFieldID in: aJavaObject to: anObject
	"private -- set the field of this type defined by JNIFieldID it aJavaObject to anObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field.  We are overriden in
	the primitive classes)"

	aJavaObject setObjectFID: aJNIFieldID to: anObject.!

shallowCopy
	"overridden since we stand in a 1-1 relationship to our wrapped JavaLangClass"

	^ self.!

static
	"this is provided to allow class statics to look as much as possible like ordinary objects.
	It always answers the JavaStatic corresponding to java.lang.Class"

	^ classObject static.
!

super
	"this is provided to allow class statics to look as much as possible like ordinary objects.
	See JavaInstance>>super.
	Throws a NoJavaSuperclassException if the receiver has no superclass"

	^ self javaSuperclassIfNone: [NoJavaSuperclassException signal: 'No superclass' with: self].!

unsafeChangeInstanceClassTo: aClass
	"private -- change to using the given class as our instance class.
	This is called during bootstrapping (as classes register themselves)
	and as ghost classes are created"

	| oldClass |

	instanceClass == aClass ifTrue: [^ self].

	oldClass := instanceClass.

	"remember our new instance class; we also have to update any other class statics
	which copied their instance class from this class"
	self replaceInstanceClass: oldClass by: aClass.
	self knownJavaSubclasses do: [:each | each replaceInstanceClass: oldClass by: aClass].
!

unsafeChangeStaticClassTo: aClass
	"private -- change to using the given class as our static class.
	This is called during bootstrapping (as classes register themselves)
	and as ghost classes are created.
	NB: since we are an instance of the class that's being changed, this will
	change our class"

	| oldClass |

	self class == aClass ifTrue: [^ self].

	oldClass := self class.

	"change our class to the new one, but we also have to change any other
	class statics which copied their class from us"
	self replaceStaticClass: oldClass by: aClass.
	self knownJavaSubclasses do: [:each | each replaceStaticClass: oldClass by: aClass].
!

withAllJavaSuperclasses
	"answer an OrderedCollection of this class followed by any Java super classes in order"

	| classes |

	classes := OrderedCollection new.
	self withAllJavaSuperclassesDo: [:c | classes addLast: c].

	^ classes.!

withAllJavaSuperclassesDo: a1Block
	"evaluate a1Block for this class followed by each Java superclass in order"

	a1Block value: self.
	^ self allJavaSuperclassesDo: a1Block.
!

writeDescriptionOn: aStream
	"write a short description of our methods on aStream"

	aStream nextPutAll: self nameAndModifiers.
	aStream cr.

	aStream nextPutAll: 'Instance class:'; space; display: instanceClass.
	instanceClass isGhostClass ifTrue:
		[aStream nextPutAll: ' (ghost, '; display: instanceClass methodDictionary size; nextPutAll: ' methods)'].
	aStream cr.

	aStream nextPutAll: 'Static class:'; space; display: self class.
	self class isGhostClass ifTrue:
		[aStream nextPutAll: ' (ghost, '; display: self class methodDictionary size; nextPutAll: ' methods)'].

!

writeInheritanceOn: aStream
	"write a formatted description of our inheritance on aStream"

	aStream nextPutAll: 'Java class:'; cr.
	self withAllJavaSuperclassesDo:
		[:each |
		aStream tab; nextPutAll: each nameAndModifiers.
		aStream cr].
	aStream cr.

	aStream nextPutAll: 'Smalltalk instance class:'; cr.
	self instanceClass withAllSuperclassesDo:
		[:each |
		aStream tab; nextPutAll: each name.
		each isGhostClass ifTrue:
			[aStream nextPutAll: ' (ghost class, '; display: each methodDictionary size; nextPutAll: ' methods)'].
		aStream cr].
	aStream cr.

	aStream nextPutAll: 'Smalltalk class static class:'; cr.
	self class withAllSuperclassesDo:
		[:each |
		aStream tab; nextPutAll: each name.
		each isGhostClass ifTrue:
			[aStream nextPutAll: ' (ghost class, '; display: each methodDictionary size; nextPutAll: ' methods)'].
		aStream cr].
!

writeStatusOn: aStream
	"write a formatted description of our status on aStream"

	"currently, only instantialble classes have any status"
! !
!JavaStatic categoriesFor: #=!comparing!public! !
!JavaStatic categoriesFor: #abstractMethods!public!reflection! !
!JavaStatic categoriesFor: #abstractMethodsPlusInterfaceMethods!public!reflection! !
!JavaStatic categoriesFor: #allAbstractMethods!public!reflection! !
!JavaStatic categoriesFor: #allFields!public!reflection! !
!JavaStatic categoriesFor: #allInstanceFields!public!reflection! !
!JavaStatic categoriesFor: #allInstanceMethods!public!reflection! !
!JavaStatic categoriesFor: #allInterfaces!public!reflection! !
!JavaStatic categoriesFor: #allJavaSuperclasses!Java class hierarchy!public! !
!JavaStatic categoriesFor: #allJavaSuperclassesDo:!enumerating!Java class hierarchy!public! !
!JavaStatic categoriesFor: #allMethods!public!reflection! !
!JavaStatic categoriesFor: #allRealFields!public!reflection! !
!JavaStatic categoriesFor: #allUndefinedMethods!public!reflection! !
!JavaStatic categoriesFor: #arrayClass!arrays!constants!public! !
!JavaStatic categoriesFor: #asJavaLangClass!converting!public! !
!JavaStatic categoriesFor: #asStatic!converting!public! !
!JavaStatic categoriesFor: #callBooleanMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callByteMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callCharMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callDoubleMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callFloatMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callIntMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callLongMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callObjectMID:withArguments:wrapperFactory:!Java method calls!public! !
!JavaStatic categoriesFor: #callShortMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #callVoidMID:withArguments:!Java method calls!public! !
!JavaStatic categoriesFor: #canBePurged!private!purging!testing! !
!JavaStatic categoriesFor: #changeInstanceClassTo:!mutating!private! !
!JavaStatic categoriesFor: #changeStaticClassTo:!mutating!private! !
!JavaStatic categoriesFor: #classloader!accessing!public! !
!JavaStatic categoriesFor: #classObject!accessing!public! !
!JavaStatic categoriesFor: #classObject:!initializing!private! !
!JavaStatic categoriesFor: #classRegistry!accessing!public! !
!JavaStatic categoriesFor: #constructors!public!reflection! !
!JavaStatic categoriesFor: #couldBeSubstituted!public!testing! !
!JavaStatic categoriesFor: #defaultInstanceClass!constants!public! !
!JavaStatic categoriesFor: #description!displaying!public! !
!JavaStatic categoriesFor: #displayOn:!displaying!public! !
!JavaStatic categoriesFor: #fields!public!reflection! !
!JavaStatic categoriesFor: #fieldsPlusInterfaceFields!public!reflection! !
!JavaStatic categoriesFor: #findField:signature:!Java field access!public! !
!JavaStatic categoriesFor: #findInstanceMethod:signature:!Java method calls!public! !
!JavaStatic categoriesFor: #findMethod:signature:!Java method calls!public! !
!JavaStatic categoriesFor: #getBooleanFID:!Java field access!public! !
!JavaStatic categoriesFor: #getByteFID:!Java field access!public! !
!JavaStatic categoriesFor: #getCharFID:!Java field access!public! !
!JavaStatic categoriesFor: #getDoubleFID:!Java field access!public! !
!JavaStatic categoriesFor: #getFloatFID:!Java field access!public! !
!JavaStatic categoriesFor: #getIntFID:!Java field access!public! !
!JavaStatic categoriesFor: #getLongFID:!Java field access!public! !
!JavaStatic categoriesFor: #getObjectFID:wrapperFactory:!Java field access!public! !
!JavaStatic categoriesFor: #getShortFID:!Java field access!public! !
!JavaStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaStatic categoriesFor: #hasAnyCanonicalInstances!managed objects!public!testing! !
!JavaStatic categoriesFor: #hasCanonicalInstances!managed objects!public!testing! !
!JavaStatic categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public!testing! !
!JavaStatic categoriesFor: #hash!comparing!public! !
!JavaStatic categoriesFor: #icon!constants!ghost classes!public! !
!JavaStatic categoriesFor: #inheritanceString!displaying!public! !
!JavaStatic categoriesFor: #inheritanceStringWithStatus!displaying!public! !
!JavaStatic categoriesFor: #initialize!initializing!private! !
!JavaStatic categoriesFor: #initializeFrom:!initializing!private! !
!JavaStatic categoriesFor: #instanceClass!accessing!public! !
!JavaStatic categoriesFor: #instanceClass:!initializing!private! !
!JavaStatic categoriesFor: #instanceFields!public!reflection! !
!JavaStatic categoriesFor: #instanceMethods!public!reflection! !
!JavaStatic categoriesFor: #interfaces!Java class hierarchy!public!reflection! !
!JavaStatic categoriesFor: #interfacesWithAllSuperinterfaces!public!reflection! !
!JavaStatic categoriesFor: #isAbstract!public!testing! !
!JavaStatic categoriesFor: #isArray!public!testing! !
!JavaStatic categoriesFor: #isDerivedFrom:!Java class hierarchy!public!testing! !
!JavaStatic categoriesFor: #isFinal!public!testing! !
!JavaStatic categoriesFor: #isInterface!public!testing! !
!JavaStatic categoriesFor: #isPrimitive!public!testing! !
!JavaStatic categoriesFor: #isStrict!public!testing! !
!JavaStatic categoriesFor: #isSubclassOf:!public!testing! !
!JavaStatic categoriesFor: #isVoid!public!testing! !
!JavaStatic categoriesFor: #javaSuperclass!accessing!Java class hierarchy!public! !
!JavaStatic categoriesFor: #javaSuperclass:!initializing!private! !
!JavaStatic categoriesFor: #javaSuperclassDo:!Java class hierarchy!public! !
!JavaStatic categoriesFor: #javaSuperclassIfNone:!Java class hierarchy!public! !
!JavaStatic categoriesFor: #jniEnv!accessing!public! !
!JavaStatic categoriesFor: #jniSignature!accessing!public! !
!JavaStatic categoriesFor: #jvm!accessing!public! !
!JavaStatic categoriesFor: #jvm:!initializing!private! !
!JavaStatic categoriesFor: #knownDirectJavaSubclasses!Java class hierarchy!public! !
!JavaStatic categoriesFor: #knownJavaSubclasses!Java class hierarchy!public! !
!JavaStatic categoriesFor: #managedInstance!accessing!managed objects!public! !
!JavaStatic categoriesFor: #methods!public!reflection! !
!JavaStatic categoriesFor: #name!accessing!public! !
!JavaStatic categoriesFor: #nameAndModifiers!displaying!public! !
!JavaStatic categoriesFor: #newArray:!arrays!Java constructors!public! !
!JavaStatic categoriesFor: #newArrayWith:!arrays!Java constructors!public! !
!JavaStatic categoriesFor: #newArrayWith:with:!arrays!Java constructors!public! !
!JavaStatic categoriesFor: #newArrayWith:with:with:!arrays!Java constructors!public! !
!JavaStatic categoriesFor: #newArrayWithAll:!arrays!Java constructors!public! !
!JavaStatic categoriesFor: #notifyRegistered!initializing!public! !
!JavaStatic categoriesFor: #purge!private!purging! !
!JavaStatic categoriesFor: #replaceInstanceClass:by:!mutating!private! !
!JavaStatic categoriesFor: #replaceStaticClass:by:!mutating!private! !
!JavaStatic categoriesFor: #resolve!mutating!public! !
!JavaStatic categoriesFor: #setBooleanFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setByteFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setCharFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setDoubleFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setFloatFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setIntFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setLongFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setObjectFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setShortFID:to:!Java field access!public! !
!JavaStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !
!JavaStatic categoriesFor: #shallowCopy!copying!public! !
!JavaStatic categoriesFor: #static!accessing!public! !
!JavaStatic categoriesFor: #super!accessing!converting!public! !
!JavaStatic categoriesFor: #unsafeChangeInstanceClassTo:!mutating!private! !
!JavaStatic categoriesFor: #unsafeChangeStaticClassTo:!mutating!private! !
!JavaStatic categoriesFor: #withAllJavaSuperclasses!Java class hierarchy!public! !
!JavaStatic categoriesFor: #withAllJavaSuperclassesDo:!enumerating!Java class hierarchy!public! !
!JavaStatic categoriesFor: #writeDescriptionOn:!displaying!public! !
!JavaStatic categoriesFor: #writeInheritanceOn:!displaying!public! !
!JavaStatic categoriesFor: #writeStatusOn:!displaying!public! !

!JavaStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	self subclassResponsibility.
!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	self subclassResponsibility.
!

icon
	"answer an Icon representing the receiver"

	^ JVM icon: (self isGhostClass ifTrue: ['JavaStatic-Ghost'] ifFalse: ['JavaStatic']).!

new
	"private -- these should never be created except as a side effect of creating a JavaLangClass"

	self shouldNotImplement.!

newAsCopyOf: aJavaStatic
	"private -- answer a new instance that copies as much data as possible from
	aJavaStatic.
	This is called during bootstrapping and conversion of clases to ghosts, so aJavaStatic
	will typically be of an instance of a *superclass* of the receiver"

	^ (super new)
		initializeFrom: aJavaStatic;
		yourself.!

newWithJVM: aJVM
	"private -- these should never be created except as a side effect of registering a JavaLangClass"

	^ (super new)
		initialize;
		jvm: aJVM;
		yourself.!

registerWith: aClassFinder
	"this is called as JVM is initialised, or with a SupplementaryClassloader, and gives
	us the chance (should we wish to accept it) to register ourself with it as a wrapper
	class"

	"set the static class associated with our Java name to this class"
	[(aClassFinder findClass: self javaClassName) changeStaticClassTo: self]
		on: JavaException
		do: [:ex | ex notify].!

registryCapacity
	"answer the initial capacity to give registries used by instances"

	^ 10.!

registryClass
	"answer the kind of Object registry used by instances"

	^ JavaObjectRegistry.! !
!JavaStatic class categoriesFor: #arrayClass!constants!public! !
!JavaStatic class categoriesFor: #defaultInstanceClass!constants!public! !
!JavaStatic class categoriesFor: #icon!constants!ghost classes!public! !
!JavaStatic class categoriesFor: #new!instance creation!private! !
!JavaStatic class categoriesFor: #newAsCopyOf:!instance creation!private! !
!JavaStatic class categoriesFor: #newWithJVM:!instance creation!private! !
!JavaStatic class categoriesFor: #registerWith:!public!registering wrapper classes! !
!JavaStatic class categoriesFor: #registryCapacity!constants!public! !
!JavaStatic class categoriesFor: #registryClass!constants!public! !

JavaClassInstance guid: (GUID fromString: '{A545397F-1712-4758-B439-1E2C2B3A6B5E}')!
JavaClassInstance comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class.

Instances of sub-classes stand in a direct 1-1 relationship with instances of JNIObject (or its subclasses).  That is they wrap the JNI "pointer" to a Java object.

These are strongly managed in the sense that they are finalizable and will arrange that the owning JVM is told when they die.  This permits it to release the JNI "local ref" to that object.

Note that these are proxies for *references* to Java objects.  I.e. you can have more than one proxy for the same Java object.  The #= and #hash method are defined such that they test for identity of the underlying Java object.'!
!JavaClassInstance categoriesForClass!Unclassified! !
!JavaClassInstance methodsFor!

asParameter
	"answer the receiver in a form suitable for passing to an ExternalLibrary call,
	i.e. the ExternalAddress of our underlying Java object"

	^ jniObject.!

asYourself
	"answer the real JavaObject underlying whatever view we have of it"

	"we are that instance"
	^ self.!

beCanonical
	"set that this instance is 'canonical' -- i.e. is the sole object that will be
	used to represent the underlying Java object.  Turning on canonicalness
	does not affect any previously existing instances that also represent the
	same object.  If a previous canoncial object existed that is not this one, then
	it will be replaced as the canon.
	The Object registry will hold a *weak* reference to this object, so canonicalness
	will vanish after the last reference to this object has gone away"

	static makeCanonical: self.
!

beGlobalRef
	"if we are not already representing a global reference, change our underlying reference to global"

	| new old |

	self isLocalRef ifFalse: [^ self].

	new := jniObject
		getGlobalRef: self jniEnv
		onException: [:jex | self jvm throwJavaException: jex].

	old := jniObject.
	jniObject := new.

	self jvm notifyNowGlobal: self.

	"note: if this throws then we've changed anyway"
	old
		releaseRef: self jniEnv
		onException: [:jex | self jvm throwJavaException: jex].
!

beLocalRef
	"if we are not already representing a JNI local reference, change our underlying reference to local"

	| new old |

	self isGlobalRef ifFalse: [^ self].

	new := jniObject
		getLocalRef: self jniEnv
		onException: [:jex | self jvm throwJavaException: jex].

	old := jniObject.
	jniObject := new.

	self jvm notifyNowLocal: self.

	"note: if this throws then we've changed anyway"
	old
		releaseRef: self jniEnv
		onException: [:jex | self jvm throwJavaException: jex].
!

beNotCanonical
	"ensure that this instance is not 'canonical'"

	static makeNotCanonical: self.
!

callBooleanMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	| answer |

	answer := self jniEnv
			CallBooleanMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer == 1.
!

callByteMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallByteMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callCharMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			CallCharMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].
!

callDoubleMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^self jniEnv
			CallDoubleMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callFloatMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallFloatMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callIntMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallIntMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callLongMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallLongMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callObjectMID: aMethodID withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM, however if you know for
	sure (and *don't* get it wrong!!) what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			CallObjectMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

callShortMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallShortMethodA_obj: jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callVoidMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self jniEnv
		CallVoidMethodA_obj: jniObject
		methodID: aMethodID
		args: aJNIValueArray
		onException: [:ex | self jvm throwJavaException: ex].
!

die
	"called when our underlying Java object is no longer valid.
	This is intended for use after the underlying JVM has shiut down.  Do not
	use it to release references while the JVM is still running, #free is the man for
	that job"

	"discard the reference"
	jniObject := nil.

	"we no longer have any responsibility for cleaning up after ourselves"
	self beUnfinalizable.!

finalize
	"private -- called to let us clean up (if necessary) when we die"

	self free.!

findField: aStringName signature: aJNISignature
	"answer the JavaFieldID corresponding to our Java object's field named aStringName,
	with signature defined by aJNISignature"

	^ self jniEnv
		GetFieldID_class: self javaClassObject asParameter
		name: aStringName
		sig: aJNISignature
		onException: [:ex | self jvm throwJavaException: ex].
!

findMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java object's method named aStringName,
	and signature defined by aJNISignature"

	^ self jniEnv
		GetMethodID_class: self javaClassObject asParameter
		name: aStringName
		sig: aJNISignature
		onException: [:ex | self jvm throwJavaException: ex].

!

free
	"if we are not already free'd then tell our manager that we are no longer wanted and
	then die.
	NB: there's an argument that we should remove canonicalness at this point,
	however I think that would put too much load on the finaliser (remember
	that nearly all JavaClassInstaces reach here via finalisation), and in the
	vast bulk of cases we either won't be canonicial anyway, or will already
	have been removed from the (weak) collection of canonicial instances.
	Hence it is the caller's responsibility to de-canonicalise objects that are
	explicitly #free-d"

	self isLive ifTrue: [self jvm disown: self].

	jniObject ifNotNil: [:it | it releaseRef: self jniEnv onException: [:jex | ]].

	self die.!

getBooleanFID: aFieldID
	"get the value of the field defined by aFieldID"

	| answer |

	answer := self jniEnv
			GetBooleanField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer == 1.!

getByteFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetByteField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getCharFID: aFieldID
	"get the value of the field defined by aFieldID.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			GetCharField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].!

getDoubleFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetDoubleField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getFloatFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetFloatField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getIntFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetIntField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getLongFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetLongField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getObjectFID: aFieldID
	"get the value of the field defined by aFieldID"

	| answer |

	answer := self jniEnv
			GetObjectField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer asJavaObject: self jvm.!

getObjectFID: aFieldID wrapperFactory: aJVMOrJavaStatic
	"get the value of the field defined by aFieldID.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			GetObjectField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

getShortFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetShortField_obj: jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

isCanonical
	"answer true iff this instance is 'canonical' -- i.e. is the sole object that will be
	used to represent the underlying Java object"

	^ static isCanonical: self.!

isDead
	"answer whether this instance no longer represents a live connection to an object in a Java Virtual Machine"

	^ jniObject isNil.!

isGlobalRef
	"answer true if we are wrapping a JNI global ref"

	^ jniObject notNil and: [jniObject isGlobalRef].!

isLive
	"answer whether this instance still represents a live connection to a Java Virtual Machine"

	^ jniObject notNil.!

isLocalRef
	"answer true if we are wrapping a JNI local ref"

	^ jniObject notNil and: [jniObject isLocalRef].!

jniObject
	"answer the JNIObject which is our real handle on the underlying Java object"

	^ jniObject.!

jniObject: aJNIObject static: aJavaStatic
	"private -- initialise ourselves to wrap aJNIObject and be of the given Java class"

	self static: aJavaStatic.
	jniObject := aJNIObject.

	self beFinalizable.

	aJavaStatic jvm own: self.
!

managedInstance
	"answer the managed object (a JavaClassInstance) which really owns the javaObject we wrap"

	"we are that object"
	^ self.!

setBooleanFID: aFieldID to: aBool
	"set the value of the field defined by aFieldID to aBool"

	self jniEnv
			SetBooleanField_obj: jniObject
			fieldID: aFieldID
			val: (aBool ifTrue: [1] ifFalse: [0])
			onException: [:ex | self jvm throwJavaException: ex].

	^ aBool.!

setByteFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetByteField_obj: jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setCharFID: aFieldID to: aCharacterOrInteger
	"set the value of the field defined by aFieldID to aCharacterOrInteger"

	self jniEnv
			SetCharField_obj: jniObject
			fieldID: aFieldID
			val: aCharacterOrInteger asInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ aCharacterOrInteger.!

setDoubleFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetDoubleField_obj: jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | self jvm throwJavaException: ex].

	^ aFloat.!

setFloatFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetFloatField_obj: jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | self jvm throwJavaException: ex].

	^ aFloat.!

setIntFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetIntField_obj: jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setLongFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetLongField_obj: jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setObjectFID: aFieldID to: aJavaObjectOrNil
	"set the value of the field defined by aFieldID to aJavaObjectOrNil"

	self jniEnv
			SetObjectField_obj: jniObject
			fieldID: aFieldID
			val: aJavaObjectOrNil asParameter
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJavaObjectOrNil.!

setShortFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetShortField_obj: jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

shallowCopy
	"overridden since we stand in a 1-1 relationship to our wrapped JNIObject"

	^ self.! !
!JavaClassInstance categoriesFor: #asParameter!converting!public! !
!JavaClassInstance categoriesFor: #asYourself!converting!public! !
!JavaClassInstance categoriesFor: #beCanonical!managed objects!modes!public! !
!JavaClassInstance categoriesFor: #beGlobalRef!converting!public! !
!JavaClassInstance categoriesFor: #beLocalRef!converting!public! !
!JavaClassInstance categoriesFor: #beNotCanonical!managed objects!modes!public! !
!JavaClassInstance categoriesFor: #callBooleanMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callByteMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callCharMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callDoubleMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callFloatMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callIntMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callLongMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callObjectMID:withArguments:wrapperFactory:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callShortMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #callVoidMID:withArguments:!Java method calls!public! !
!JavaClassInstance categoriesFor: #die!managed objects!public! !
!JavaClassInstance categoriesFor: #finalize!finalizing!managed objects!private! !
!JavaClassInstance categoriesFor: #findField:signature:!Java field access!public! !
!JavaClassInstance categoriesFor: #findMethod:signature:!Java method calls!public! !
!JavaClassInstance categoriesFor: #free!finalizing!managed objects!public! !
!JavaClassInstance categoriesFor: #getBooleanFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getByteFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getCharFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getDoubleFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getFloatFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getIntFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getLongFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getObjectFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #getObjectFID:wrapperFactory:!Java field access!public! !
!JavaClassInstance categoriesFor: #getShortFID:!Java field access!public! !
!JavaClassInstance categoriesFor: #isCanonical!managed objects!public!testing! !
!JavaClassInstance categoriesFor: #isDead!public!testing! !
!JavaClassInstance categoriesFor: #isGlobalRef!public!testing! !
!JavaClassInstance categoriesFor: #isLive!public!testing! !
!JavaClassInstance categoriesFor: #isLocalRef!public!testing! !
!JavaClassInstance categoriesFor: #jniObject!accessing!public! !
!JavaClassInstance categoriesFor: #jniObject:static:!initializing!managed objects!public! !
!JavaClassInstance categoriesFor: #managedInstance!accessing!managed objects!public! !
!JavaClassInstance categoriesFor: #setBooleanFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setByteFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setCharFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setDoubleFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setFloatFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setIntFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setLongFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setObjectFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #setShortFID:to:!Java field access!public! !
!JavaClassInstance categoriesFor: #shallowCopy!copying!public! !

!JavaClassInstance class methodsFor!

freeInstancesOwnedBy: aJVM
	"private -- called by aJVM as it closes down to ensure that no instances still think they are alive"

	(self instancesOwnedBy: aJVM) do: [:each | each free].!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later).
	Override in subclasses, to force this"

	"default is no"
	^ false.!

jniObject: aJNIObject static: aJavaStatic
	"private -- instances should only be created by sending #asJavaObject: to a JNIObject, or #wrapJNIObject: to
	some object capable of wrapping it.
	Answer a new instance wrapping the underlying Java object pointer, aJNIObject, and of the
	specified Java class, and owned by aJVM.
	Note that this is the only way that managed objects get created (except for a couple during bootstrapping),
	the object will use its JavaStatic to reach its owning JVM at need"

	^ (super new)
		initialize;
		jniObject: aJNIObject static: aJavaStatic;
		yourself.!

killInstancesOwnedBy: aJVM
	"private -- called by aJVM as it closes down to ensure that no instances still think they are alive"

	(self instancesOwnedBy: aJVM) do: [:each | each die].!

new
	"private -- these should only be created by a JVM or by sending #asJavaClassInstance: to
	a JNIObject or by sending #as: to an existing instance"

	self shouldNotImplement.!

wrap: aJavaInstance asStatic: aJavaStatic
	"private -- if this is called it is because someone has attempted to use the #as: mechanism to get a view of
	aJavaInstance.  However this is illegal since JavaClassInstance is the root of the tree of managed
	objects and it is Just Not On to convert non-managed objects into managed ones (e,g, the underlying
	jniObject would get released more than once).
	To get from an JavaInterfaceInstance or JavaNonvirtual back to the real managed object, use #asYourself,
	or -- if you prefer -- #managedInstance"

	self shouldNotImplement.! !
!JavaClassInstance class categoriesFor: #freeInstancesOwnedBy:!managed objects!operations!private! !
!JavaClassInstance class categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!JavaClassInstance class categoriesFor: #jniObject:static:!instance creation!private! !
!JavaClassInstance class categoriesFor: #killInstancesOwnedBy:!managed objects!operations!private! !
!JavaClassInstance class categoriesFor: #new!instance creation!private! !
!JavaClassInstance class categoriesFor: #wrap:asStatic:!instance creation!private! !

JavaInterfaceInstance guid: (GUID fromString: '{C1B6E922-F588-4C3A-A0CB-89A4C721AB0A}')!
JavaInterfaceInstance comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances take a JavaInstance and a JavaStatic and allow you to see the object as if it were an instance of the class.

The point of this is to give some sort of organisation to how we wrap Java interfaces.

In JNI, interfaces are just classes, and as such we represent them as instances of JavaLangClass.  Thus, the static members (methods and fields) of an interface are all accessed via the usual class static mechanism (see the comment on JavaStatic for more detail).

However, it is common to want to provide helper methods which are defined against an interface.  E.g. we want to wrap the APIs of java.util.Collection and subinterfaces in such a way that they can be used with any object that implements those interfaces.  It''d be easy to add subclasses under JavaLangObject with defined helpers, but the problem is that those helper methods would only apply to instances of some specific Java class and its subclasses, rather than any class which implements the interface.

Therefore we define this class and its subclasses.  Each instance is build from a JavaInstance and a JavaStatic (which ought to be an interface class).  These are used to construct an object which acts as if the underlying Java object were a member of the corresponding class.

E.g. we can define a JavaUtilCollection subclass with helper methods to provide easy access to that API.  Now, when we have a Java object which implements that interface, we can wrap in in an instance of JavaUtilCollection, and invoke those methods directly on the new wrapper.

JavaInstance has a method #as: which takes a JavaClassStatic object as argument and answers a new instance of (a subclass of) JavaInterface which has the JavaClassStatic object as its javaClass, and the original object as its real Java object.  It is conventional -- but by no means required -- that you only use the #as: mechanism to "cast" objects to their actual interfaces.

E.g. given an object, hashMapObj, which wraps a Java instance of java.util.HashMap, and a JavaLangClass object for the java.util.Map interface:

	hashMapObj := ...
	mapInterface := jvm findClass: #''java.util.Map''.

We can now say:

	map := hashMapObj as: mapInterface.

We can now access the static members of java.util.Map via the usual static mechanism (actually Map has no statics),  e.g:

	xxx := map static callVoidMethod: ''anImaginaryStaticMethod'' signature: ''()V''.

If anyone had set up a special subclass of JavaStatic for java.util.Map (but there''d be no point) then map static would answer an instance of that class.

Similarly, if someone had set up a specialised subclass of JavaInterface for java.util.map with a #keysAndValuesDo: helper method on it (and there''d be a lot of point in doing so), then map would be an instance of that class and so we could say:

	map keysAndValuesDo: [:key :value | ...].
'!
!JavaInterfaceInstance categoriesForClass!Unclassified! !
!JavaInterfaceInstance methodsFor!

callBooleanMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	| answer |

	answer := self jniEnv
			CallBooleanMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer == 1.
!

callByteMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallByteMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callCharMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			CallCharMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].
!

callDoubleMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^self jniEnv
			CallDoubleMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callFloatMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallFloatMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callIntMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallIntMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callLongMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallLongMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callObjectMID: aMethodID withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			CallObjectMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

callShortMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallShortMethodA_obj: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callVoidMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self jniEnv
		CallVoidMethodA_obj: self jniObject
		methodID: aMethodID
		args: aJNIValueArray
		onException: [:ex | self jvm throwJavaException: ex].
!

findField: aStringName signature: aJNISignature
	"answer the JavaFieldID corresponding to our Java object's field named aStringName,
	with signature defined by aJNISignature"

	"Java interfaces can't have non-static fields"
	self shouldNotImplement.!

findMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java object's method named aStringName,
	and signature defined by aJNISignature"

	^ self jniEnv
		GetMethodID_class: self javaClassObject asParameter
		name: aStringName
		sig: aJNISignature
		onException: [:ex | self jvm throwJavaException: ex].

!

managedInstance
	"answer the managed object (a JavaClassInstance) which really owns the javaObject we wrap"

	^ subject.!

subject: aJavaClassInstance
	"private -- set the subject JavaClassInstance we wrap"

	subject := aJavaClassInstance.
! !
!JavaInterfaceInstance categoriesFor: #callBooleanMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callByteMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callCharMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callDoubleMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callFloatMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callIntMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callLongMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callObjectMID:withArguments:wrapperFactory:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callShortMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #callVoidMID:withArguments:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #findField:signature:!Java field access!public! !
!JavaInterfaceInstance categoriesFor: #findMethod:signature:!Java method calls!public! !
!JavaInterfaceInstance categoriesFor: #managedInstance!accessing!managed objects!public! !
!JavaInterfaceInstance categoriesFor: #subject:!initializing!private! !

!JavaInterfaceInstance class methodsFor!

jniObject: aJNIObject static: aJavaStatic
	"private -- overridden since the only time this should ever be called is if the system is
	totally buggered"

	^ self shouldNotImplement.!

new
	"private -- these should only be created by sending #as: to an existing JavaInstance"

	self shouldNotImplement.!

wrap: aJavaInstance asStatic: aJavaStatic
	"answer a new instance which wraps the same underlying object as aJavaInstance but which treats
	it as belonging to the given Java class.  The point is that the class is typically an interface and so it would
	never normally have any instances"

	^ (super new)
		initialize;
		subject: aJavaInstance;
		static: aJavaStatic;
		yourself.
		! !
!JavaInterfaceInstance class categoriesFor: #jniObject:static:!instance creation!private! !
!JavaInterfaceInstance class categoriesFor: #new!instance creation!private! !
!JavaInterfaceInstance class categoriesFor: #wrap:asStatic:!instance creation!public! !

JavaNonvirtual guid: (GUID fromString: '{2B7919D7-3CE6-481D-B30B-0EFA3213BCE3}')!
JavaNonvirtual comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

These are rather odd.  They are like the JavaInterfaceInstaces in that they make an existing JavaClassInstance act as if it were a member of some other class, which must be a superclass.  The easiest way to create these is to use the #super method on JavaClassInstance.  Once created the new object will circumvent Java''s method overrides.  E.g.

	str := ''Hello there'' asJavaString: jvm.

	"java.lang.String.toString() returns itself, so:"
	str toString_null asString.		--> ''Hello there''

	"but the overridding implementation in java.lang.Object.toString() returns a new String with
	the class name and an ID number, and so sending the same message to the ''super'' of the
	same string object will answer that instead:"
	str super toString_null asString.	--> ''java.lang.String@ce2be576''

You can send #super repeatedly to climb the inheritance chain, or you can use the class-side #wrap:asStatic: factory method directly.
'!
!JavaNonvirtual categoriesForClass!Unclassified! !
!JavaNonvirtual methodsFor!

callBooleanMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	| answer |

	answer := self jniEnv
			CallNonvirtualBooleanMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer == 1.
!

callByteMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualByteMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callCharMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			CallNonvirtualCharMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].
!

callDoubleMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualDoubleMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callFloatMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualFloatMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callIntMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualIntMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callLongMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualLongMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callObjectMID: aMethodID withArguments: aJNIValueArray wrapperFactory: aJVMOrJavaStatic
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			CallNonvirtualObjectMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

callShortMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	^ self jniEnv
			CallNonvirtualShortMethodA_obj: self jniObject
			class: static asParameter
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:ex | self jvm throwJavaException: ex].!

callVoidMID: aMethodID withArguments: aJNIValueArray
	"invoke the method defined by aMethodID on our Java object with the given
	JNIValueArray of arguments"

	self jniEnv
		CallNonvirtualVoidMethodA_obj: self jniObject
		class: static asParameter
		methodID: aMethodID
		args: aJNIValueArray
		onException: [:ex | self jvm throwJavaException: ex].
!

findField: aStringName signature: aJNISignature
	"answer the JavaFieldID corresponding to our Java object's field named aStringName,
	with signature defined by aJNISignature"

	^ self jniEnv
		GetFieldID_class: self javaClassObject asParameter
		name: aStringName
		sig: aJNISignature
		onException: [:ex | self jvm throwJavaException: ex].
!

findMethod: aStringName signature: aJNISignature
	"answer the JavaMethodID corresponding to our Java object's method named aStringName,
	and signature defined by aJNISignature"

	^ self jniEnv
		GetMethodID_class: static asParameter
		name: aStringName
		sig: aJNISignature
		onException: [:ex | self jvm throwJavaException: ex].

!

getBooleanFID: aFieldID
	"get the value of the field defined by aFieldID"

	| answer |

	answer := self jniEnv
			GetBooleanField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer == 1.!

getByteFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetByteField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getCharFID: aFieldID
	"get the value of the field defined by aFieldID.
	Answers either a Character or an Integer depending on whether the 16bit Java char can be
	represented as a Dolphin 8bit Character"

	| answer |

	answer := self jniEnv
			GetCharField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer < 256
		ifTrue: [Character value: answer]
		ifFalse: [answer].!

getDoubleFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetDoubleField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getFloatFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetFloatField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getIntFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetIntField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getLongFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetLongField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

getObjectFID: aFieldID
	"get the value of the field defined by aFieldID"

	| answer |

	answer := self jniEnv
			GetObjectField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ answer asJavaObject: self jvm.!

getObjectFID: aFieldID wrapperFactory: aJVMOrJavaStatic
	"get the value of the field defined by aFieldID.
	The last argument is an object that will be capable of wrapping (#wrapJNIObject:) the resulting
	object reference.  Normally this should just be the appropriate JVM (and *don't* get it wrong!!),
	however if you know *for sure* what the class static of the result will be, then you can pass that
	as the last argument, which saves quite a lot of logic (around half the method call overhead)
	of discovering what class static to use to generate the wrapper for the result"

	| answer |

	answer := self jniEnv
			GetObjectField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.!

getShortFID: aFieldID
	"get the value of the field defined by aFieldID"

	^ self jniEnv
			GetShortField_obj: self jniObject
			fieldID: aFieldID
			onException: [:ex | self jvm throwJavaException: ex].!

managedInstance
	"answer the managed object (a JavaClassInstance) which really owns the javaObject we wrap"

	^ subject.!

setBooleanFID: aFieldID to: aBool
	"set the value of the field defined by aFieldID to aBool"

	self jniEnv
			SetBooleanField_obj: self jniObject
			fieldID: aFieldID
			val: (aBool ifTrue: [1] ifFalse: [0])
			onException: [:ex | self jvm throwJavaException: ex].

	^ aBool.!

setByteFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetByteField_obj: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setCharFID: aFieldID to: aCharacterOrInteger
	"set the value of the field defined by aFieldID to aCharacterOrInteger"

	self jniEnv
			SetCharField_obj: self jniObject
			fieldID: aFieldID
			val: aCharacterOrInteger asInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ aCharacterOrInteger.!

setDoubleFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetDoubleField_obj: self jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | self jvm throwJavaException: ex].

	^ aFloat.!

setFloatFID: aFieldID to: aFloat
	"set the value of the field defined by aFieldID to aFloat"

	self jniEnv
			SetFloatField_obj: self jniObject
			fieldID: aFieldID
			val: aFloat
			onException: [:ex | self jvm throwJavaException: ex].

	^ aFloat.!

setIntFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetIntField_obj: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setLongFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetLongField_obj: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

setObjectFID: aFieldID to: aJavaObjectOrNil
	"set the value of the field defined by aFieldID to aJavaObjectOrNil"

	self jniEnv
			SetObjectField_obj: self jniObject
			fieldID: aFieldID
			val: aJavaObjectOrNil asParameter
			onException: [:ex | self jvm throwJavaException: ex].

	^ aJavaObjectOrNil.!

setShortFID: aFieldID to: anInteger
	"set the value of the field defined by aFieldID to anInteger"

	self jniEnv
			SetShortField_obj: self jniObject
			fieldID: aFieldID
			val: anInteger
			onException: [:ex | self jvm throwJavaException: ex].

	^ anInteger.!

subject: aJavaClassInstance
	"private -- set the subject JavaClassInstance we wrap"

	subject := aJavaClassInstance.
! !
!JavaNonvirtual categoriesFor: #callBooleanMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callByteMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callCharMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callDoubleMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callFloatMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callIntMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callLongMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callObjectMID:withArguments:wrapperFactory:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callShortMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #callVoidMID:withArguments:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #findField:signature:!Java field access!public! !
!JavaNonvirtual categoriesFor: #findMethod:signature:!Java method calls!public! !
!JavaNonvirtual categoriesFor: #getBooleanFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getByteFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getCharFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getDoubleFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getFloatFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getIntFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getLongFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getObjectFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getObjectFID:wrapperFactory:!Java field access!public! !
!JavaNonvirtual categoriesFor: #getShortFID:!Java field access!public! !
!JavaNonvirtual categoriesFor: #managedInstance!accessing!managed objects!public! !
!JavaNonvirtual categoriesFor: #setBooleanFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setByteFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setCharFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setDoubleFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setFloatFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setIntFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setLongFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setObjectFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #setShortFID:to:!Java field access!public! !
!JavaNonvirtual categoriesFor: #subject:!initializing!private! !

!JavaNonvirtual class methodsFor!

jniObject: aJNIObject static: aJavaStatic
	"private -- overridden since the only time this should ever be called is if the system is
	totally buggered"

	^ self shouldNotImplement.!

new
	"private -- these should only be created by sending #as: to an existing JavaInstance"

	self shouldNotImplement.!

wrap: aJavaInstance asStatic: aJavaStatic
	"answer a new instance which wraps the same underlying object as aJavaInstance but which treats
	it as belonging to aJavaLangClass"

	^ (super new)
		initialize;
		subject: aJavaInstance;
		static: aJavaStatic;
		yourself.! !
!JavaNonvirtual class categoriesFor: #jniObject:static:!instance creation!private! !
!JavaNonvirtual class categoriesFor: #new!instance creation!private! !
!JavaNonvirtual class categoriesFor: #wrap:asStatic:!instance creation!public! !

JavaPrimitiveInstance guid: (GUID fromString: '{555DB20F-3717-4DE8-9133-02B7B760BBCC}')!
JavaPrimitiveInstance comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

This class is never instantiated, but it makes the rest of the code more regular if we have it around.'!
!JavaPrimitiveInstance categoriesForClass!Unclassified! !
!JavaPrimitiveInstance class methodsFor!

jniObject: aJNIObject static: aJavaStatic
	"private -- overridden since the only time this should ever be called is if the system is
	totally buggered"

	^ self shouldNotImplement.!

wrap: aJavaInstance asStatic: aJavaStatic
	"private -- overridden since the only time this should ever be called is if the system is
	totally buggered"

	self shouldNotImplement.! !
!JavaPrimitiveInstance class categoriesFor: #jniObject:static:!instance creation!private! !
!JavaPrimitiveInstance class categoriesFor: #wrap:asStatic:!instance creation!private! !

JavaLangObject guid: (GUID fromString: '{0DB3D2A8-0249-4CA6-A100-482E9A0E808E}')!
JavaLangObject comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for instances of java.lang.Object, and other Java classes that are not given more specialised wrappers.

It is not necessary to derive wrappers for subclasses of java.lang.Object from this class, but it is normally tidier to try to keep the Java class hierarchy and the wrapper hierarchy recognisably similar.'!
!JavaLangObject categoriesForClass!Unclassified! !
!JavaLangObject class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	"these are all, in fact, inherited from JavaObject, but we may as well list them here"
	^ #(
		#equals_Object:
		#getClass_null
		#hashCode_null
		#notify_null
		#notifyAll_null
		#toString_null
		#wait_long:
		#wait_long:int:
		#wait_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Object'.
! !
!JavaLangObject class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangObject class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangObject class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangObject class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangObject class categoriesFor: #javaClassName!accessing!constants!public! !

JavaAggregate guid: (GUID fromString: '{B82DA5EA-96E1-4F27-9FA1-48202249EE55}')!
JavaAggregate comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class that collects together some behaviour of Java objects which can be mapped onto a Collections-like interface.

There are a few short-hand methods defined (#do: #select: #inject:into: and so on), but mostly you get a Collection-flavoured interface by asking for its Collection adaptor with #asCollection.'!
!JavaAggregate categoriesForClass!Unclassified! !
!JavaAggregate methodsFor!

allSatisfy: a1Block
	"shorthand for:"

	^ self asCollection allSatisfy: a1Block.!

anySatisfy: a1Block
	"shorthand for:"

	^ self asCollection anySatisfy: a1Block.
!

asArray
	"shorthand for:"

	^ self asCollection asArray.!

asBag
	"shorthand for:"

	^ self asCollection asBag.
!

asCollection
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Collection hierarchy.
	In fact, for these classes we answer a ArrayAdaptor wrapped around the aggregate.
	It acts in all (maybe!!) ways like a SequenceableCollection"

	adaptorCache isNil ifTrue: [adaptorCache := ArrayAdaptor for: self].

	^ adaptorCache.!

asIdentitySet
	"shorthand for:"

	^ self asCollection asIdentitySet.
!

asOrderedCollection
	"shorthand for:"

	^ self asCollection asOrderedCollection.
!

asSet
	"shorthand for:"

	^ self asCollection asSet.!

asSortedCollection
	"shorthand for:"

	^ self asCollection asSortedCollection.!

asSortedCollection: sortBlock
	"shorthand for:"

	^ self asCollection asSortedCollection: sortBlock.!

at: anInteger
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

at: anIndex put: anObject
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

collect: a1Block
	"shorthand for:"

	^ self asCollection collect: a1Block.!

detect: a1Block
	"shorthand for:"

	^ self asCollection detect: a1Block.!

detect: a1Block ifNone: a0Block
	"shorthand for:"

	^ self asCollection detect: a1Block ifNone: a0Block.!

do: a1Block
	"shorthand for:"

	^ self asCollection do: a1Block.!

includes: anObject
	"shorthand for:"

	^ self asCollection includes: anObject.!

inject: anObject into: a2Block
	"shorthand for:"

	^ self asCollection inject: anObject into: a2Block.

!

isEmpty
	"answer whether the receiver contains no elements."

	^ self size == 0.
!

notEmpty
	"answer whether the receiver contains any elements"

	^ self isEmpty not.!

readStream
	"shorthand for:"

	^ self asCollection readStream.
!

reject: a1Block
	"shorthand for:"

	^ self asCollection reject: a1Block.!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements
	"shorthand for:"

	^ self asCollection replaceFrom: aStartIndex to: aStopIndex with: replacementElements.
!

select: a1Block
	"shorthand for:"

	^ self asCollection select: a1Block.!

size
	"answer the number of elements in the underlying Java aggregate"

	self subclassResponsibility.!

writeStream
	"shorthand for:"

	^ self asCollection writeStream.
! !
!JavaAggregate categoriesFor: #allSatisfy:!enumerating!public! !
!JavaAggregate categoriesFor: #anySatisfy:!enumerating!public! !
!JavaAggregate categoriesFor: #asArray!converting!public! !
!JavaAggregate categoriesFor: #asBag!converting!public! !
!JavaAggregate categoriesFor: #asCollection!converting!public! !
!JavaAggregate categoriesFor: #asIdentitySet!converting!public! !
!JavaAggregate categoriesFor: #asOrderedCollection!converting!public! !
!JavaAggregate categoriesFor: #asSet!converting!public! !
!JavaAggregate categoriesFor: #asSortedCollection!converting!public! !
!JavaAggregate categoriesFor: #asSortedCollection:!converting!public! !
!JavaAggregate categoriesFor: #at:!accessing!public! !
!JavaAggregate categoriesFor: #at:put:!accessing!public! !
!JavaAggregate categoriesFor: #collect:!enumerating!public! !
!JavaAggregate categoriesFor: #detect:!public!searching! !
!JavaAggregate categoriesFor: #detect:ifNone:!public!searching! !
!JavaAggregate categoriesFor: #do:!enumerating!public! !
!JavaAggregate categoriesFor: #includes:!public!searching! !
!JavaAggregate categoriesFor: #inject:into:!enumerating!public! !
!JavaAggregate categoriesFor: #isEmpty!public!testing! !
!JavaAggregate categoriesFor: #notEmpty!public!testing! !
!JavaAggregate categoriesFor: #readStream!converting!public! !
!JavaAggregate categoriesFor: #reject:!enumerating!public! !
!JavaAggregate categoriesFor: #replaceFrom:to:with:!public!replacing! !
!JavaAggregate categoriesFor: #select:!enumerating!public! !
!JavaAggregate categoriesFor: #size!accessing!public! !
!JavaAggregate categoriesFor: #writeStream!converting!public! !

JavaLangClassLoader guid: (GUID fromString: '{1D108B4A-26C8-4433-8CBB-16DDDF8BDE6D}')!
JavaLangClassLoader comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.ClassLoader.

We add a (lazily created) JavaClassIndex to each instance, so that we can work as a <JavaClassFinder> with reasonable efficiency.  For this reason, we have to use canonical instances.
'!
!JavaLangClassLoader categoriesForClass!Unclassified! !
!JavaLangClassLoader methodsFor!

classIndex
	"answer the class index that we use to cache class lookups in
	our role as a <javaClassFinder>"

	^ self sharedMutex critical:
		[classIndex ifNil: [classIndex := JavaClassIndex newFor: self]].
!

classloader
	"answer the actual JavaLangClassLoader used by this <javaClassFinder>"

	"we are that classloader"
	^ self.!

clearAssertionStatus_null
	"invoke the receiver's public synchronized clearAssertionStatus() Java method"

	self callVoidMethod: 'clearAssertionStatus'.
!

defineClass: aString fromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a Class Static corresponding to the newly defined class.
	The class will 'know' that it was loaded by this classloader.
	Will throw an error if the class definition does not define a class named by
	aString (which can be in JNI or Java format)."

	#CUtodo. "should this use our own defineClass(String, ByteBuffer, ProtectionDomain) method ?"

	^ self jvm defineClass: aString fromBytes: aByteArray classloader: self.!

defineClassFromBytes: aByteArray
	"passes the data in aByteArray to the JVM as a 'classfile' (a JVM-native class definition)
	Answers a Class Static corresponding to the newly defined class.
	The class will 'know' that it was loaded by this classloader"

	#CUtodo. "should this use our own defineClass(String, ByteBuffer, ProtectionDomain) method ?"

	^ self jvm defineClassFromBytes: aByteArray classloader: self.!

dependentClasses
	"private -- answer a collection of the *known* dependent classes"

	^ self classRegistry allClasses select: [:each | each classloader = self].
!

dependentClassloaders
	"private -- answer a collection of the *known* dependent classloaders"

	^ self static canonicalInstances select: [:each | each parentClassloader = self].!

findClass: aStringOrSymbol
	"answer a JavaStatic object corresponding to aString, it will be loaded
	and/or looked up via this class loader.
	NB: the argument should be a *Smalltalk* string"

	"this will bounce back to our own #findClassObject: if the index doesn't have it already"
	^ self classIndex findClass: aStringOrSymbol.
!

findClassObject: aJNIClassName
	"answer JavaLangClass object corresponding to aJNIClassName -- the name will be looked up in the
	normal Java way startng at our Java classloader, but delagating /first/ to its parent
	classloader and the system classloaders"

	"NB: since classloaders don't use normal Java class names, /nor/ do they
	use JNI format names, we choose to accept JNI names as our parameter
	and hack it into acceptable shape here.
	The loadClass()/findClass() format is to use normal Java class names (remembering
	that nested classes have names with $ in them, not dots as they appear in the Java
	source), but uses JNI format to mark array classes.
	Makes it slow, and its kinda 	kludgy, but that's Java for you..."

	^ self loadClass_String: (aJNIClassName copyReplacing: $/ withObject: $.).
!

getParent_null
	"answer the result of calling the receiver's public final getParent() Java method"

	^ self callObjectMethod: 'getParent' signature: '()Ljava/lang/ClassLoader;'.
!

getResource_String: aString1
	"answer the result of calling the receiver's public getResource(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getResource' signature: '(Ljava/lang/String;)Ljava/net/URL;' withArguments: args.
!

getResourceAsStream_String: aString1
	"answer the result of calling the receiver's public getResourceAsStream(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getResourceAsStream' signature: '(Ljava/lang/String;)Ljava/io/InputStream;' withArguments: args.
!

getResources_String: aString1
	"answer the result of calling the receiver's public final getResources(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getResources' signature: '(Ljava/lang/String;)Ljava/util/Enumeration;' withArguments: args.
!

loadClass_String: aString1
	"answer the result of calling the receiver's public loadClass(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'loadClass' signature: '(Ljava/lang/String;)Ljava/lang/Class;' withArguments: args.
!

parentClassloader
	"answer the parent classloader -- that is to say, the one that the
	.java.lang.Classloader instace we correspond to condsiders to be
	its parent (and hence will delagate to).
	May be nil"

	^ self getParent_null.!

purge
	"purge this classloader; remove any classes from the main class registries that are owned
	by this classloader.
	The idea is that we remove all refernces to this classloader from JNIPort, and so it can be reclaimed
	(and its classes unloaded) by the Java GC.
	NB: it is the caller's responsibility to ensure that any relevant callbacks have been cleared first"

	"remove dependent classes and classloaders -- the loop is because tracking down
	dependencies may cause new clasees or loaders to be registered"
	[self purgeDependents > 0] whileTrue.

	"remove ourself from the canonical list"
	self beNotCanonical.

	"and lastly relase our own reference"
	self free.
!

purgeDependentClasses
	"private -- purge any classes from the class registry and index that are owned
	by this classloader.
	Answer how many were removed"

	| dependents |

	dependents := self dependentClasses.

	dependents do: [:each | each purge].

	^ dependents size.

!

purgeDependentClassloaders
	"private -- purge any classloader from the canonical list that are dependent on
	this classloader.
	Answer how many were removed"

	| dependents |

	dependents := self dependentClassloaders.

	dependents do: [:each | each purge].

	^ dependents size.
!

purgeDependents
	"private -- purge any classes from the class registry and index that are owned
	by this classloader.
	Answer how many were removed"

	"NB: the order is important here -- we must purge the classloaders
	first since they may themselves of a class that is dependent on this
	classloader, and hence which is just about to be discarded"

	^ self purgeDependentClassloaders + self purgeDependentClasses.
!

setClassAssertionStatus_String: aString1 boolean: boolean1
	"invoke the receiver's public synchronized setClassAssertionStatus(java.lang.String, boolean) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			booleanAt: 2 put: boolean1;
			yourself.

	self callVoidMethod: 'setClassAssertionStatus' signature: '(Ljava/lang/String;Z)V' withArguments: args.
!

setDefaultAssertionStatus_boolean: boolean1
	"invoke the receiver's public synchronized setDefaultAssertionStatus(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'setDefaultAssertionStatus' signature: '(Z)V' withArguments: args.
!

setPackageAssertionStatus_String: aString1 boolean: boolean1
	"invoke the receiver's public synchronized setPackageAssertionStatus(java.lang.String, boolean) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			booleanAt: 2 put: boolean1;
			yourself.

	self callVoidMethod: 'setPackageAssertionStatus' signature: '(Ljava/lang/String;Z)V' withArguments: args.
! !
!JavaLangClassLoader categoriesFor: #classIndex!accessing!public! !
!JavaLangClassLoader categoriesFor: #classloader!accessing!Java classes!public! !
!JavaLangClassLoader categoriesFor: #clearAssertionStatus_null!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangClassLoader categoriesFor: #defineClass:fromBytes:!Java classes!public! !
!JavaLangClassLoader categoriesFor: #defineClassFromBytes:!Java classes!public! !
!JavaLangClassLoader categoriesFor: #dependentClasses!private!purging! !
!JavaLangClassLoader categoriesFor: #dependentClassloaders!private!purging! !
!JavaLangClassLoader categoriesFor: #findClass:!Java classes!public! !
!JavaLangClassLoader categoriesFor: #findClassObject:!Java classes!public! !
!JavaLangClassLoader categoriesFor: #getParent_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangClassLoader categoriesFor: #getResource_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangClassLoader categoriesFor: #getResourceAsStream_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangClassLoader categoriesFor: #getResources_String:!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangClassLoader categoriesFor: #loadClass_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangClassLoader categoriesFor: #parentClassloader!accessing!public! !
!JavaLangClassLoader categoriesFor: #purge!public!purging! !
!JavaLangClassLoader categoriesFor: #purgeDependentClasses!private!purging! !
!JavaLangClassLoader categoriesFor: #purgeDependentClassloaders!private!purging! !
!JavaLangClassLoader categoriesFor: #purgeDependents!private!purging! !
!JavaLangClassLoader categoriesFor: #setClassAssertionStatus_String:boolean:!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangClassLoader categoriesFor: #setDefaultAssertionStatus_boolean:!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangClassLoader categoriesFor: #setPackageAssertionStatus_String:boolean:!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !

JavaLangClassLoader methodProtocol: #javaClassFinder attributes: #() selectors: #(#classIndex #classloader #defineClass:fromBytes: #defineClassFromBytes: #findClass: #findClassObject: #jvm #sharedMutex)!

!JavaLangClassLoader class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#clearAssertionStatus_null
		#getParent_null
		#getResource_String:
		#getResourceAsStream_String:
		#getResources_String:
		#loadClass_String:
		#setClassAssertionStatus_String:boolean:
		#setDefaultAssertionStatus_boolean:
		#setPackageAssertionStatus_String:boolean:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.ClassLoader'.
! !
!JavaLangClassLoader class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangClassLoader class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangClassLoader class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangClassLoader class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangClassLoader class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaLangReflectAccessibleObject guid: (GUID fromString: '{6158212E-E3B5-4582-A53D-57C9F85A4E66}')!
JavaLangReflectAccessibleObject comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

I''ve defined #getDeclaringClass, #getModifiers, and against #getName this class, although the Java doc doesn''t require java.lang.reflect.AccessibleMember to implement java.lang.reflect.Member.  All the instantiable subclasses do implement it, so it''s better to put the method short-cuts here.

Similarly java.lang.Class isn''t really a subclass of java,lang.reflect.AccessibleObject, but it is convenient to treat it as if it were.'!
!JavaLangReflectAccessibleObject categoriesForClass!Unclassified! !
!JavaLangReflectAccessibleObject methodsFor!

declaredIn
	"answer the class static corresponding to our the Java getDeclaringClass() method"

	declaredInCache isNil ifTrue:
		[declaredInCacheIsValid = true ifFalse:
			[declaredInCache := self getDeclaringClass_null ifNotNil: [:it | it classStatic].
			declaredInCacheIsValid := true.	"since declaredIn may be nil, even after being computed"]].

	^ declaredInCache.!

declaredIn: aJavaStatic
	"answer whether we were declared in the given class static"

	^ self declaredIn == aJavaStatic.!

description
	"answer a short textual description of this object, suitable for use as, say, an info tip"

	^ self isDead
		ifTrue: ['']
		ifFalse: [self toString asString].!

getDeclaringClass_null
	"answer the result of calling the receiver's public getDeclaringClass() Java method"

	^ self callObjectMethod: 'getDeclaringClass' signature: '()Ljava/lang/Class;'.
!

getModifiers_null
	"answer the result of calling the receiver's public getModifiers() Java method"

	^ self callIntMethod: 'getModifiers'.
!

getName_null
	"answer the result of calling the receiver's public getName() Java method"

	^ self callObjectMethod: 'getName' signature: '()Ljava/lang/String;'.
!

isAbstract
	"answer whether the receiver represents a abstract member"

	^ self modifiers allMask: MODIFIER_ABSTRACT.!

isAccessible_null
	"answer the result of calling the receiver's public isAccessible() Java method"

	^ self callBooleanMethod: 'isAccessible'.
!

isDefaultAccess
	"answer whether the receiver represents a protected member"

	^ self modifiers anyMask: ##(MODIFIER_PROTECTED | MODIFIER_PRIVATE | MODIFIER_PUBLIC).
!

isFinal
	"answer whether the receiver represents a final member"

	^ self modifiers allMask: MODIFIER_FINAL.!

isInterface
	"answer whether the receiver represents an interface"

	^ self modifiers allMask: MODIFIER_INTERFACE.!

isNative
	"answer whether the receiver represents a native method"

	^ self modifiers allMask: MODIFIER_NATIVE.!

isPrivate
	"answer whether the receiver represents a private member"

	^ self modifiers allMask: MODIFIER_PRIVATE.!

isProtected
	"answer whether the receiver represents a protected member"

	^ self modifiers allMask: MODIFIER_PROTECTED.!

isPublic
	"answer whether the receiver represents a public member"

	^ self modifiers allMask: MODIFIER_PUBLIC.!

isStatic
	"answer whether the receiver represents a static member"

	^ self modifiers allMask: MODIFIER_STATIC.!

isStaticOrPrivate
	"answer whether the receiver represents a static or private member"

	^ self modifiers anyMask: ##(MODIFIER_STATIC bitOr: MODIFIER_PRIVATE).!

isStrict
	"answer whether the receiver represents a strict member"

	^ self modifiers allMask: MODIFIER_STRICT.!

isSynchronized
	"answer whether the receiver represents a synchronized memthod.
	(NB: this concept is an error in the Java language, synchronized is properly
	seen as a property of the code inside the method, not of the method itself)"

	^ self modifiers allMask: MODIFIER_SYNCHRONIZED.!

isSynthetic
	"answer whether we are deemed to be 'synthetic'
	Note that this method is essentially useless because the Java compiler
	is lax (to say the least) about setting the appropriate flag"

	"the method doesn't exist pre-Java 5"
	^ self jvm hasJava5Extensions and: [self isSynthetic].
!

isSynthetic_null
	"answer the result of calling the receiver's public isSynthetic() Java method"

	^ self callBooleanMethod: 'isSynthetic'.
!

isTransient
	"answer whether the receiver represents a transient member"

	^ self modifiers allMask: MODIFIER_TRANSIENT.!

isVolatile
	"answer whether the receiver represents a volatile member"

	^ self modifiers allMask: MODIFIER_VOLATILE.!

jniSignature
	"answer a JNI-style signature for this member"

	self subclassResponsibility.!

modifiers
	"answer the result of calling the Java method getModifiers)"

	modifiersCache isNil ifTrue: [modifiersCache := self getModifiers_null].

	^ modifiersCache.!

modifiersString
	"answer a String describing our modifier flags"

	^ ((self jvm findClass: #'java.lang.reflect.Modifier') toString_int: self getModifiers_null) asString.!

name
	"answer the name of the member as a String"

	nameCache isNil ifTrue: [nameCache := self getName_null asString].

	^ nameCache.!

setAccessible_boolean: boolean1
	"invoke the receiver's public setAccessible(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'setAccessible' signature: '(Z)V' withArguments: args.
! !
!JavaLangReflectAccessibleObject categoriesFor: #declaredIn!accessing!public! !
!JavaLangReflectAccessibleObject categoriesFor: #declaredIn:!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #description!displaying!public! !
!JavaLangReflectAccessibleObject categoriesFor: #getDeclaringClass_null!Java-methods!Java-public!public! !
!JavaLangReflectAccessibleObject categoriesFor: #getModifiers_null!Java-methods!Java-public!public! !
!JavaLangReflectAccessibleObject categoriesFor: #getName_null!Java-methods!Java-public!public! !
!JavaLangReflectAccessibleObject categoriesFor: #isAbstract!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isAccessible_null!Java-methods!Java-public!public! !
!JavaLangReflectAccessibleObject categoriesFor: #isDefaultAccess!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isFinal!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isInterface!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isNative!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isPrivate!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isProtected!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isPublic!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isStatic!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isStaticOrPrivate!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isStrict!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isSynchronized!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isSynthetic!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isSynthetic_null!Java-methods!Java-public!public! !
!JavaLangReflectAccessibleObject categoriesFor: #isTransient!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #isVolatile!public!testing! !
!JavaLangReflectAccessibleObject categoriesFor: #jniSignature!accessing!public! !
!JavaLangReflectAccessibleObject categoriesFor: #modifiers!accessing!public! !
!JavaLangReflectAccessibleObject categoriesFor: #modifiersString!accessing!public! !
!JavaLangReflectAccessibleObject categoriesFor: #name!accessing!public! !
!JavaLangReflectAccessibleObject categoriesFor: #setAccessible_boolean:!Java-methods!Java-public!public! !

!JavaLangReflectAccessibleObject class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#isAccessible_null
		#setAccessible_boolean:

		#getDeclaringClass_null
		#getModifiers_null
		#getName_null

		#isSynthetic_null
	).
!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	Overriden to force this for all reflections"

	"this is true for all subclasses, but nor for ourself (we don't need
	canonical instances 'cos we don't never have any"
	^ self inheritsFrom: ##(self).!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.reflect.AccessibleObject'.
!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"

	| modifiers |

	modifiers := JVM current findClass: 'java.lang.reflect.Modifiers'.

	"NB: we could use something like:
		modifiers populate: JavaReflectionConstants keyBase: 'MODIFIER_'.
	except that method needs this dictionary to be in place before it'll work"

	(Smalltalk at: #JavaReflectionConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'MODIFIER_ABSTRACT' put: modifiers get_ABSTRACT;
		at: 'MODIFIER_FINAL' put: modifiers get_FINAL;
		at: 'MODIFIER_INTERFACE' put: modifiers get_INTERFACE;
		at: 'MODIFIER_NATIVE' put: modifiers get_NATIVE;
		at: 'MODIFIER_PRIVATE' put: modifiers get_PRIVATE;
		at: 'MODIFIER_PROTECTED' put: modifiers get_PROTECTED;
		at: 'MODIFIER_PUBLIC' put: modifiers get_PUBLIC;
		at: 'MODIFIER_STATIC' put: modifiers get_STATIC;
		at: 'MODIFIER_STRICT' put: modifiers get_STRICT;
		at: 'MODIFIER_SYNCHRONIZED' put: modifiers get_SYNCHRONIZED;
		at: 'MODIFIER_TRANSIENT' put: modifiers get_TRANSIENT;
		at: 'MODIFIER_VOLATILE' put: modifiers get_VOLATILE;

		shrink.! !
!JavaLangReflectAccessibleObject class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaLangReflectAccessibleObject class categoriesFor: #rebuildPoolConstants!initializing!private! !

JavaLangThread guid: (GUID fromString: '{46BB8D9E-91E8-4841-B6E9-E51EA013E906}')!
JavaLangThread comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.Thread'!
!JavaLangThread categoriesForClass!Unclassified! !
!JavaLangThread methodsFor!

checkAccess_null
	"invoke the receiver's public final checkAccess() Java method"

	self callVoidMethod: 'checkAccess'.
!

countStackFrames_null
	"answer the result of calling the receiver's public native countStackFrames() Java method"

	^ self callIntMethod: 'countStackFrames'.
!

destroy_null
	"invoke the receiver's public destroy() Java method"

	self callVoidMethod: 'destroy'.
!

getContextClassLoader_null
	"answer the result of calling the receiver's public getContextClassLoader() Java method"

	^ self callObjectMethod: 'getContextClassLoader' signature: '()Ljava/lang/ClassLoader;'.
!

getName_null
	"answer the result of calling the receiver's public final getName() Java method"

	^ self callObjectMethod: 'getName' signature: '()Ljava/lang/String;'.
!

getPriority_null
	"answer the result of calling the receiver's public final getPriority() Java method"

	^ self callIntMethod: 'getPriority'.
!

getThreadGroup_null
	"answer the result of calling the receiver's public final getThreadGroup() Java method"

	^ self callObjectMethod: 'getThreadGroup' signature: '()Ljava/lang/ThreadGroup;'.
!

interrupt_null
	"invoke the receiver's public interrupt() Java method"

	self callVoidMethod: 'interrupt'.
!

isAlive_null
	"answer the result of calling the receiver's public final native isAlive() Java method"

	^ self callBooleanMethod: 'isAlive'.
!

isDaemon_null
	"answer the result of calling the receiver's public final isDaemon() Java method"

	^ self callBooleanMethod: 'isDaemon'.
!

isInterrupted_null
	"answer the result of calling the receiver's public isInterrupted() Java method"

	^ self callBooleanMethod: 'isInterrupted'.
!

join_long: long1
	"invoke the receiver's public final synchronized join(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	self callVoidMethod: 'join' signature: '(J)V' withArguments: args.
!

join_long: long1 int: int1
	"invoke the receiver's public final synchronized join(long, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			longAt: 1 put: long1;
			intAt: 2 put: int1;
			yourself.

	self callVoidMethod: 'join' signature: '(JI)V' withArguments: args.
!

join_null
	"invoke the receiver's public final join() Java method"

	self callVoidMethod: 'join'.
!

resume_null
	"invoke the receiver's public final resume() Java method"

	self callVoidMethod: 'resume'.
!

run_null
	"invoke the receiver's public run() Java method"

	self callVoidMethod: 'run'.
!

setContextClassLoader_ClassLoader: aClassLoader1
	"invoke the receiver's public setContextClassLoader(java.lang.ClassLoader) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClassLoader1;
			yourself.

	self callVoidMethod: 'setContextClassLoader' signature: '(Ljava/lang/ClassLoader;)V' withArguments: args.
!

setDaemon_boolean: boolean1
	"invoke the receiver's public final setDaemon(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'setDaemon' signature: '(Z)V' withArguments: args.
!

setName_String: aString1
	"invoke the receiver's public final setName(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'setName' signature: '(Ljava/lang/String;)V' withArguments: args.
!

setPriority_int: int1
	"invoke the receiver's public final setPriority(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'setPriority' signature: '(I)V' withArguments: args.
!

start_null
	"invoke the receiver's public synchronized native start() Java method"

	self callVoidMethod: 'start'.
!

stop_null
	"invoke the receiver's public final stop() Java method"

	self callVoidMethod: 'stop'.
!

stop_Throwable: aThrowable1
	"invoke the receiver's public final synchronized stop(java.lang.Throwable) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThrowable1;
			yourself.

	self callVoidMethod: 'stop' signature: '(Ljava/lang/Throwable;)V' withArguments: args.
!

suspend_null
	"invoke the receiver's public final suspend() Java method"

	self callVoidMethod: 'suspend'.
!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
! !
!JavaLangThread categoriesFor: #checkAccess_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #countStackFrames_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangThread categoriesFor: #destroy_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #getContextClassLoader_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #getName_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #getPriority_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #getThreadGroup_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #interrupt_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #isAlive_null!**auto generated**!Java-final!Java-methods!Java-native!Java-public!public! !
!JavaLangThread categoriesFor: #isDaemon_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #isInterrupted_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #join_long:!**auto generated**!Java-final!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangThread categoriesFor: #join_long:int:!**auto generated**!Java-final!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangThread categoriesFor: #join_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #resume_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #run_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #setContextClassLoader_ClassLoader:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #setDaemon_boolean:!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #setName_String:!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #setPriority_int:!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #start_null!**auto generated**!Java-methods!Java-native!Java-public!Java-synchronized!public! !
!JavaLangThread categoriesFor: #stop_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #stop_Throwable:!**auto generated**!Java-final!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangThread categoriesFor: #suspend_null!**auto generated**!Java-final!Java-methods!Java-public!public! !
!JavaLangThread categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !

!JavaLangThread class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#checkAccess_null
		#countStackFrames_null
		#destroy_null
		#getContextClassLoader_null
		#getName_null
		#getPriority_null
		#getThreadGroup_null
		#interrupt_null
		#isAlive_null
		#isDaemon_null
		#isInterrupted_null
		#join_long:
		#join_long:int:
		#join_null
		#resume_null
		#run_null
		#setContextClassLoader_ClassLoader:
		#setDaemon_boolean:
		#setName_String:
		#setPriority_int:
		#start_null
		#stop_null
		#stop_Throwable:
		#suspend_null
		#toString_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Thread'.
! !
!JavaLangThread class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThread class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThread class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThread class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThread class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaLangThrowable guid: (GUID fromString: '{66FFD6D2-B714-4E9C-8420-0D2687B8E6DB}')!
JavaLangThrowable comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Class used to wrap instances of java.lang.Throwable.  

When instances are thrown, the JNIPort machinery will actually throw an instance of JavaException, which contains the instance.  One effect of this is that you can trap Java exceptions (in Smalltalk) by doing something like:

	[...blah blah...]
		on: XXX
		do: [:err | ...]

where XXX can be any of:
	JavaException (or a superclass, i.e. normal Smalltalk exception trapping).
	This class, or a subclass.
	The class Static of the Java class of the Java exception object.'!
!JavaLangThrowable categoriesForClass!Unclassified! !
!JavaLangThrowable methodsFor!

cause
	"answer the 'cause' (another exception -- see the Java doc) associated with this exception"

	^ self getCause_null.!

fillInStackTrace_null
	"answer the result of calling the receiver's public synchronized native fillInStackTrace() Java method"

	^ self callObjectMethod: 'fillInStackTrace' signature: '()Ljava/lang/Throwable;'.
!

getCause_null
	"answer the result of calling the receiver's public getCause() Java method"

	^ self callObjectMethod: 'getCause' signature: '()Ljava/lang/Throwable;'.
!

getLocalizedMessage_null
	"answer the result of calling the receiver's public getLocalizedMessage() Java method"

	^ self callObjectMethod: 'getLocalizedMessage' signature: '()Ljava/lang/String;'.
!

getMessage_null
	"answer the result of calling the receiver's public getMessage() Java method"

	^ self callObjectMethod: 'getMessage' signature: '()Ljava/lang/String;'.
!

getStackTrace_null
	"answer the result of calling the receiver's public getStackTrace() Java method"

	^ self callObjectMethod: 'getStackTrace' signature: '()[Ljava/lang/StackTraceElement;'.
!

initCause_Throwable: aThrowable1
	"answer the result of calling the receiver's public synchronized initCause(java.lang.Throwable) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThrowable1;
			yourself.

	^ self callObjectMethod: 'initCause' signature: '(Ljava/lang/Throwable;)Ljava/lang/Throwable;' withArguments: args.
!

message
	"answer the String associated with this exception"

	^ self getMessage_null asString.!

printStackTrace_null
	"invoke the receiver's public printStackTrace() Java method"

	self callVoidMethod: 'printStackTrace'.
!

printStackTrace_PrintStream: aPrintStream1
	"invoke the receiver's public printStackTrace(java.io.PrintStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aPrintStream1;
			yourself.

	self callVoidMethod: 'printStackTrace' signature: '(Ljava/io/PrintStream;)V' withArguments: args.
!

printStackTrace_PrintWriter: aPrintWriter1
	"invoke the receiver's public printStackTrace(java.io.PrintWriter) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aPrintWriter1;
			yourself.

	self callVoidMethod: 'printStackTrace' signature: '(Ljava/io/PrintWriter;)V' withArguments: args.
!

setStackTrace_StackTraceElementArray: aStackTraceElements1
	"invoke the receiver's public setStackTrace(java.lang.StackTraceElement[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aStackTraceElements1;
			yourself.

	self callVoidMethod: 'setStackTrace' signature: '([Ljava/lang/StackTraceElement;)V' withArguments: args.
!

signal
	"cause ourself to be thrown as an exception within Smalltalk.
	If this is invoked from within a callback, and is not caught within
	the callback, then this exception object will be thrown in the Java
	space"

	JavaException
		signal: 'Uncaught Java exception'
		with: self.!

stackTrace
	"answer the result of invoking the Java getStackTrace() method as a Smalltalk collection"

	^ self getStackTrace_null asCollection.!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
! !
!JavaLangThrowable categoriesFor: #cause!accessing!public! !
!JavaLangThrowable categoriesFor: #fillInStackTrace_null!**auto generated**!Java-methods!Java-native!Java-public!Java-synchronized!public! !
!JavaLangThrowable categoriesFor: #getCause_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #getLocalizedMessage_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #getMessage_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #getStackTrace_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #initCause_Throwable:!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!JavaLangThrowable categoriesFor: #message!accessing!public! !
!JavaLangThrowable categoriesFor: #printStackTrace_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #printStackTrace_PrintStream:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #printStackTrace_PrintWriter:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #setStackTrace_StackTraceElementArray:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangThrowable categoriesFor: #signal!public!signalling! !
!JavaLangThrowable categoriesFor: #stackTrace!accessing!public! !
!JavaLangThrowable categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !

!JavaLangThrowable class methodsFor!

, anExceptionFilter
	"answer a new ExceptionSet containing the receiver and anExceptionFilter (typically a Class
	under Exception"

	^ ExceptionSet with: self with: anExceptionFilter.!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#fillInStackTrace_null
		#getCause_null
		#getLocalizedMessage_null
		#getMessage_null
		#getStackTrace_null
		#initCause_Throwable:
		#printStackTrace_null
		#printStackTrace_PrintStream:
		#printStackTrace_PrintWriter:
		#setStackTrace_StackTraceElementArray:
		#toString_null
	).
!

handles: anException
	"answer whether the receiver matches anException.
	This is provided so that instances of JavaException can be caught
	with exception on: blocks using this class or its subclasses"

	^ (anException isKindOf: JavaException) and: [anException tag isKindOf: self].!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.Throwable'.
! !
!JavaLangThrowable class categoriesFor: #,!exception filtering!public! !
!JavaLangThrowable class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThrowable class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThrowable class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThrowable class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangThrowable class categoriesFor: #handles:!exception filtering!public! !
!JavaLangThrowable class categoriesFor: #javaClassName!accessing!constants!public! !

JavaLangString guid: (GUID fromString: '{E014A129-E09A-440F-802F-1E2ED076C686}')!
JavaLangString comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.String.

Note that not all Java Strings can be represented as Smalltalk Strings, since Java allows a slightly larger range of values from the space of Unicode characters (i.e it allows 16bit chars, rather than 8bit -- neither can handle the full range of Unicode).'!
!JavaLangString categoriesForClass!Unclassified! !
!JavaLangString methodsFor!

asByteArray
	"answer a Smalltalk ByteArray with the same bytes as the Java String we stand for (as far as possible, remembering
	that Java chars are 16-bit).

WARNING!!
	I may change the meaning of this method !!"

	"it's not clear exactly how we should handle Java's quasi-UTF8 encoding in this case.

	We could take the view that someone asking for the java.lang.String as a ByteArray wants
	the exact bytes as returned by the JVM:

		^ self size = 0
			ifTrue: [#[]]
			ifFalse: [self withBytesDo: [bytes: | ByteArray fromAddress: bytes length: self size]].

	Or maybe it would make more sense to return a UTF-16 encoded byte array representing the
	same characters:

		^ self size = 0
			ifTrue: [#[]]
			ifFalse: [self withElementsDo: [words: | ByteArray fromAddress: bytes length: self size * 2]].

	For now, we take the view that they want the nearest they can get to a ByteArray with the same elements
	as the Smalltalk String (which may fail, since neither ByteArrays nor Strings can hold 16bit chars).
	And (since this is not likely to be used much anyway) we do it the quick 'n dirty way.
	"
	#CUtodo.  "decide what's best and do it !!"
	^ self asString asByteArray.!

asJavaString: aJVM
	"answer a JavaLangString object which is owned by aJVM"

	^ aJVM == self jvm
		ifTrue: [self]
		ifFalse: [self asString asJavaString: aJVM].!

asString
	"answer a Smalltalk String with the same characters as the Java String we stand for"

	"HACK: there's an annoying bit of overcleverness in the way Dolphin works here.  If the length
	is 0, then when we try to create a BYTEArray, Dolphin will refuse to wrap the given (valid) address
	in an external structure, instead it insists on creating a blank which consideres itself to be #isNull and
	to have length = 0.  Which breaks #fromJavaQuasiUTF8EncodedByteArray.  We put in a hack here to avoid it"
	^ self size = 0
		ifTrue: ['']
		ifFalse: [self withBytesDo: [:bytes | String fromJavaQuasiUTF8EncodedByteArray: bytes]].
!

asUnicodeString
	"answer a Smalltalk UnicodeString with the same characters as the Java String we stand for"

	^ self withElementsDo: [:words | UnicodeString fromAddress: words yourAddress length: words length].
!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based
	Answers a Character or an Integer depending on whether the answer will fit in an 8-bit
	Smalltalk Character"

	| word ch |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	word := WORD new. 
	self jniEnv
			GetStringRegion_str: jniObject
			start: anIndex-1
			len: 1
			buf: word
			onException: [:jex | self jvm throwJavaException: jex].
	ch := word value.

	^ ch < 256 ifTrue: [Character value: ch] ifFalse: [ch].!

at: anIndex put: aCharacterOrInteger
	"one of the root methods for <SequenceableCollection>"

	"strings are immutable"
	self shouldNotImplement.
!

charAt_int: int1
	"answer the result of calling the receiver's public charAt(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callCharMethod: 'charAt' signature: '(I)C' withArguments: args.
!

compareTo_Object: anObject1
	"answer the result of calling the receiver's public compareTo(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callIntMethod: 'compareTo' signature: '(Ljava/lang/Object;)I' withArguments: args.
!

compareTo_String: aString1
	"answer the result of calling the receiver's public compareTo(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callIntMethod: 'compareTo' signature: '(Ljava/lang/String;)I' withArguments: args.
!

compareToIgnoreCase_String: aString1
	"answer the result of calling the receiver's public compareToIgnoreCase(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callIntMethod: 'compareToIgnoreCase' signature: '(Ljava/lang/String;)I' withArguments: args.
!

concat_String: aString1
	"answer the result of calling the receiver's public concat(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'concat' signature: '(Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

contentEquals_StringBuffer: aStringBuffer1
	"answer the result of calling the receiver's public contentEquals(java.lang.StringBuffer) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aStringBuffer1;
			yourself.

	^ self callBooleanMethod: 'contentEquals' signature: '(Ljava/lang/StringBuffer;)Z' withArguments: args.
!

displayOn: aStream
	"append a user-oriented representation of our underlying Java object to aStream"

	"we check liveness here (although it's usually not necessary) since it stops problems
	with debuging in the presence of a dead JVM"
	self isLive ifTrue: [self writeOn: aStream].!

endsWith_String: aString1
	"answer the result of calling the receiver's public endsWith(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callBooleanMethod: 'endsWith' signature: '(Ljava/lang/String;)Z' withArguments: args.
!

equals_Object: anObject1
	"answer the result of calling the receiver's public equals(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'equals' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

equalsIgnoreCase_String: aString1
	"answer the result of calling the receiver's public equalsIgnoreCase(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callBooleanMethod: 'equalsIgnoreCase' signature: '(Ljava/lang/String;)Z' withArguments: args.
!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, note that the indexes are Smalltalk
	indexes and, as such, 1-based"

	| stop |

	aStartIndex > aStopIndex ifTrue: [^ self].
	aStartIndex < 1 ifTrue: [self errorSubscriptBounds: aStartIndex].
	aStartIndex > self size ifTrue: [self errorSubscriptBounds: aStartIndex].

	stop := self size min: aStopIndex.
	self withElementsDo:
		[:words | words from: aStartIndex to: stop keysAndValuesDo:
			[:key :value || ch |
			ch := value.
			ch < 256 ifTrue: [ch := Character value: ch].
			a2Block value: key value: ch]].
	stop < aStopIndex ifTrue: [self errorSubscriptBounds: stop + 1].
!

getBytes_int: int1 int: int2 byteArray: bytes1 int: int3
	"invoke the receiver's public getBytes(int, int, byte[], int) Java method"

	| args |

	args := (JNIValueArray new: 4)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			objectAt: 3 put: bytes1;
			intAt: 4 put: int3;
			yourself.

	self callVoidMethod: 'getBytes' signature: '(II[BI)V' withArguments: args.
!

getBytes_null
	"answer the result of calling the receiver's public getBytes() Java method"

	^ self callObjectMethod: 'getBytes' signature: '()[B'.
!

getBytes_String: aString1
	"answer the result of calling the receiver's public getBytes(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getBytes' signature: '(Ljava/lang/String;)[B' withArguments: args.
!

getChars_int: int1 int: int2 charArray: chars1 int: int3
	"invoke the receiver's public getChars(int, int, char[], int) Java method"

	| args |

	args := (JNIValueArray new: 4)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			objectAt: 3 put: chars1;
			intAt: 4 put: int3;
			yourself.

	self callVoidMethod: 'getChars' signature: '(II[CI)V' withArguments: args.
!

hashCode_null
	"answer the result of calling the receiver's public hashCode() Java method"

	^ self callIntMethod: 'hashCode'.
!

indexOf_int: int1
	"answer the result of calling the receiver's public indexOf(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callIntMethod: 'indexOf' signature: '(I)I' withArguments: args.
!

indexOf_int: int1 int: int2
	"answer the result of calling the receiver's public indexOf(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callIntMethod: 'indexOf' signature: '(II)I' withArguments: args.
!

indexOf_String: aString1
	"answer the result of calling the receiver's public indexOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callIntMethod: 'indexOf' signature: '(Ljava/lang/String;)I' withArguments: args.
!

indexOf_String: aString1 int: int1
	"answer the result of calling the receiver's public indexOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callIntMethod: 'indexOf' signature: '(Ljava/lang/String;I)I' withArguments: args.
!

intern_null
	"answer the result of calling the receiver's public native intern() Java method"

	^ self callObjectMethod: 'intern' signature: '()Ljava/lang/String;'.
!

lastIndexOf_int: int1
	"answer the result of calling the receiver's public lastIndexOf(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callIntMethod: 'lastIndexOf' signature: '(I)I' withArguments: args.
!

lastIndexOf_int: int1 int: int2
	"answer the result of calling the receiver's public lastIndexOf(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callIntMethod: 'lastIndexOf' signature: '(II)I' withArguments: args.
!

lastIndexOf_String: aString1
	"answer the result of calling the receiver's public lastIndexOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callIntMethod: 'lastIndexOf' signature: '(Ljava/lang/String;)I' withArguments: args.
!

lastIndexOf_String: aString1 int: int1
	"answer the result of calling the receiver's public lastIndexOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callIntMethod: 'lastIndexOf' signature: '(Ljava/lang/String;I)I' withArguments: args.
!

length_null
	"answer the result of calling the receiver's public length() Java method"

	^ self callIntMethod: 'length'.
!

matches_String: aString1
	"answer the result of calling the receiver's public matches(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callBooleanMethod: 'matches' signature: '(Ljava/lang/String;)Z' withArguments: args.
!

regionMatches_boolean: boolean1 int: int1 String: aString1 int: int2 int: int3
	"answer the result of calling the receiver's public regionMatches(boolean, int, java.lang.String, int, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 5)
			booleanAt: 1 put: boolean1;
			intAt: 2 put: int1;
			objectAt: 3 put: aString1Ref;
			intAt: 4 put: int2;
			intAt: 5 put: int3;
			yourself.

	^ self callBooleanMethod: 'regionMatches' signature: '(ZILjava/lang/String;II)Z' withArguments: args.
!

regionMatches_int: int1 String: aString1 int: int2 int: int3
	"answer the result of calling the receiver's public regionMatches(int, java.lang.String, int, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 4)
			intAt: 1 put: int1;
			objectAt: 2 put: aString1Ref;
			intAt: 3 put: int2;
			intAt: 4 put: int3;
			yourself.

	^ self callBooleanMethod: 'regionMatches' signature: '(ILjava/lang/String;II)Z' withArguments: args.
!

replace_char: char1 char: char2
	"answer the result of calling the receiver's public replace(char, char) Java method"

	| args |

	args := (JNIValueArray new: 2)
			charAt: 1 put: char1;
			charAt: 2 put: char2;
			yourself.

	^ self callObjectMethod: 'replace' signature: '(CC)Ljava/lang/String;' withArguments: args.
!

replaceAll_String: aString1 String: aString2
	"answer the result of calling the receiver's public replaceAll(java.lang.String, java.lang.String) Java method"

	| args aString1Ref aString2Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	aString2Ref := aString2 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aString2Ref;
			yourself.

	^ self callObjectMethod: 'replaceAll' signature: '(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

replaceFirst_String: aString1 String: aString2
	"answer the result of calling the receiver's public replaceFirst(java.lang.String, java.lang.String) Java method"

	| args aString1Ref aString2Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	aString2Ref := aString2 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aString2Ref;
			yourself.

	^ self callObjectMethod: 'replaceFirst' signature: '(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"one of the root methods for <SequenceableCollection>"

	"Java Strings are immutable"
	self shouldNotImplement.!

size
	"answer the number of chars in the Java string"

	sizeCache isNil ifFalse: [^ sizeCache].

	sizeCache := self jniEnv
				GetStringLength_str: jniObject
				onException: [:jex | self jvm throwJavaException: jex].

	^ sizeCache.!

split_String: aString1
	"answer the result of calling the receiver's public split(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'split' signature: '(Ljava/lang/String;)[Ljava/lang/String;' withArguments: args.
!

split_String: aString1 int: int1
	"answer the result of calling the receiver's public split(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'split' signature: '(Ljava/lang/String;I)[Ljava/lang/String;' withArguments: args.
!

startsWith_String: aString1
	"answer the result of calling the receiver's public startsWith(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callBooleanMethod: 'startsWith' signature: '(Ljava/lang/String;)Z' withArguments: args.
!

startsWith_String: aString1 int: int1
	"answer the result of calling the receiver's public startsWith(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callBooleanMethod: 'startsWith' signature: '(Ljava/lang/String;I)Z' withArguments: args.
!

subSequence_int: int1 int: int2
	"answer the result of calling the receiver's public subSequence(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callObjectMethod: 'subSequence' signature: '(II)Ljava/lang/CharSequence;' withArguments: args.
!

substring_int: int1
	"answer the result of calling the receiver's public substring(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'substring' signature: '(I)Ljava/lang/String;' withArguments: args.
!

substring_int: int1 int: int2
	"answer the result of calling the receiver's public substring(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callObjectMethod: 'substring' signature: '(II)Ljava/lang/String;' withArguments: args.
!

toCharArray_null
	"answer the result of calling the receiver's public toCharArray() Java method"

	^ self callObjectMethod: 'toCharArray' signature: '()[C'.
!

toLowerCase_Locale: aLocale1
	"answer the result of calling the receiver's public toLowerCase(java.util.Locale) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aLocale1;
			yourself.

	^ self callObjectMethod: 'toLowerCase' signature: '(Ljava/util/Locale;)Ljava/lang/String;' withArguments: args.
!

toLowerCase_null
	"answer the result of calling the receiver's public toLowerCase() Java method"

	^ self callObjectMethod: 'toLowerCase' signature: '()Ljava/lang/String;'.
!

toString
	"answer the result of invoking our Java object's toString() method.
	Overriden to avoid creating too many objects while displaying strings"

	^ self.
!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
!

toUpperCase_Locale: aLocale1
	"answer the result of calling the receiver's public toUpperCase(java.util.Locale) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aLocale1;
			yourself.

	^ self callObjectMethod: 'toUpperCase' signature: '(Ljava/util/Locale;)Ljava/lang/String;' withArguments: args.
!

toUpperCase_null
	"answer the result of calling the receiver's public toUpperCase() Java method"

	^ self callObjectMethod: 'toUpperCase' signature: '()Ljava/lang/String;'.
!

trim_null
	"answer the result of calling the receiver's public trim() Java method"

	^ self callObjectMethod: 'trim' signature: '()Ljava/lang/String;'.
!

withBytesDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary BYTEArray
	of bytes in Java's wierd pseudo-UTF8 format.
	NB: these are unsigned bytes, not Java's signed bytes.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	"can't use #size because that's the size in characters"
	length := self jniEnv
			GetStringUTFLength_str: jniObject
			onException: [:jex | self jvm throwJavaException: jex].

	ptr := self jniEnv
			GetStringUTFChars_str: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| bytes |
	bytes := BYTEArray fromAddress: ptr length: length.
	^ a1Block value: bytes]
		ensure: [self jniEnv
				ReleaseStringUTFChars_str: jniObject
				chars: ptr
				onException: [:jex | self jvm throwJavaException: jex]].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary WORDArray
	of chars (unsigned 16bit words) in Java's native format (as far as I can tell, this is
	some sort of variant on UTF16, but I can't find the details) as its parameter.
	DO NOT KEEP A REFERENCE TO THE ARRAY.

	Note: as of Java 5, the format is defined to be UTF-16, but I need to check the
	byte-order"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetStringChars_str: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| chars |
	chars := WORDArray fromAddress: ptr length: length.
	^ a1Block value: chars]
		ensure: [self jniEnv
				ReleaseStringChars_str: jniObject
				chars: ptr
				onException: [:jex | self jvm throwJavaException: jex]].
!

writeOn: aStream
	"write the characters from the Java String onto aStream"

	aStream nextPutAll: self asString.
! !
!JavaLangString categoriesFor: #asByteArray!converting!public! !
!JavaLangString categoriesFor: #asJavaString:!converting!public! !
!JavaLangString categoriesFor: #asString!converting!public! !
!JavaLangString categoriesFor: #asUnicodeString!converting!public! !
!JavaLangString categoriesFor: #at:!accessing!public! !
!JavaLangString categoriesFor: #at:put:!accessing!public! !
!JavaLangString categoriesFor: #charAt_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #compareTo_Object:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #compareTo_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #compareToIgnoreCase_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #concat_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #contentEquals_StringBuffer:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #displayOn:!displaying!public! !
!JavaLangString categoriesFor: #endsWith_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #equals_Object:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #equalsIgnoreCase_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!JavaLangString categoriesFor: #getBytes_int:int:byteArray:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #getBytes_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #getBytes_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #getChars_int:int:charArray:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #hashCode_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #indexOf_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #indexOf_int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #indexOf_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #indexOf_String:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #intern_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangString categoriesFor: #lastIndexOf_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #lastIndexOf_int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #lastIndexOf_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #lastIndexOf_String:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #length_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #matches_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #regionMatches_boolean:int:String:int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #regionMatches_int:String:int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #replace_char:char:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #replaceAll_String:String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #replaceFirst_String:String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing! !
!JavaLangString categoriesFor: #size!accessing!public! !
!JavaLangString categoriesFor: #split_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #split_String:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #startsWith_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #startsWith_String:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #subSequence_int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #substring_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #substring_int:int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toCharArray_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toLowerCase_Locale:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toLowerCase_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toString!converting!public! !
!JavaLangString categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toUpperCase_Locale:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #toUpperCase_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #trim_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangString categoriesFor: #withBytesDo:!accessing!public! !
!JavaLangString categoriesFor: #withElementsDo:!operations!public! !
!JavaLangString categoriesFor: #writeOn:!operations!public! !

!JavaLangString class methodsFor!

fromByteArray: aByteArray jvm: aJVM
	"answer a new JavaObject wrapping a java.lang.String which has been created by copying the 'characters'
	in aByteArray"

	| answer |

	"NB: this will only work properly (i.e give the chars you expect) if aByteArray is encoded in the wierd modified version
	of UTF8 that the JVM uses"

	answer := aJVM jniEnv
			NewStringUTF_utf: aByteArray asParameter
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
!

fromString: aString jvm: aJVM
	"answer a new JavaObject wrapping a java.lang.String which has been created by copying the characters
	in aString"

	| answer |

	answer := aJVM jniEnv
			NewStringUTF_utf: aString asJavaQuasiUTF8EncodedString asParameter
			onException: [:jex | aJVM throwJavaException: jex].

	"we can use the class static for java.lang.String directly as the wrapper here"
	^ aJVM wrapJNIString: answer.
!

fromUnicodeString: aUnicodeString jvm: aJVM
	"answer a new JavaObject wrapping a java.lang.String which has been created by copying the characters
	in aUnicodeString"

	| answer |

	#CUtodo. "there may be UTF16 conversion issues here, depending on how Dolphin's (claimed)
			UnicodeStrings actually work"

	answer := aJVM jniEnv
			NewString_unicode: aUnicodeString asParameter
			len: aUnicodeString size
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#charAt_int:
		#compareTo_Object:
		#compareTo_String:
		#compareToIgnoreCase_String:
		#concat_String:
		#contentEquals_StringBuffer:
		#endsWith_String:
		#equals_Object:
		#equalsIgnoreCase_String:
		#getBytes_int:int:byteArray:int:
		#getBytes_null
		#getBytes_String:
		#getChars_int:int:charArray:int:
		#hashCode_null
		#indexOf_int:
		#indexOf_int:int:
		#indexOf_String:
		#indexOf_String:int:
		#intern_null
		#lastIndexOf_int:
		#lastIndexOf_int:int:
		#lastIndexOf_String:
		#lastIndexOf_String:int:
		#length_null
		#matches_String:
		#regionMatches_boolean:int:String:int:int:
		#regionMatches_int:String:int:int:
		#replace_char:char:
		#replaceAll_String:String:
		#replaceFirst_String:String:
		#split_String:
		#split_String:int:
		#startsWith_String:
		#startsWith_String:int:
		#subSequence_int:int:
		#substring_int:
		#substring_int:int:
		#toCharArray_null
		#toLowerCase_Locale:
		#toLowerCase_null
		#toString_null
		#toUpperCase_Locale:
		#toUpperCase_null
		#trim_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.String'.
! !
!JavaLangString class categoriesFor: #fromByteArray:jvm:!instance creation!public! !
!JavaLangString class categoriesFor: #fromString:jvm:!instance creation!public! !
!JavaLangString class categoriesFor: #fromUnicodeString:jvm:!instance creation!public! !
!JavaLangString class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangString class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangString class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangString class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangString class categoriesFor: #javaClassName!accessing!constants!public! !

JavaObjectArray guid: (GUID fromString: '{A2792F2B-B9C5-4BA8-B218-E275782D9A90}')!
JavaObjectArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java object arrays (for any subclass of java.lang.Object).'!
!JavaObjectArray categoriesForClass!Unclassified! !
!JavaObjectArray methodsFor!

alternateInspectorClass
	"answer the class of fallback inspector to be used to inspect the receiver."

	"want to answer the same as Array"
	^ #() alternateInspectorClass.!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	"since the element class is fixed for all instances of our class static, we can cache a
	default 'wrapper' in the class static. The wrapper is either the jvm, or our/its element class
	if that class is non-substitutable (such as String or Integer)"
	^ self at: anIndex wrapper: static instanceElementWrapper.!

at: anIndex put: aJavaObject
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	self jniEnv
		SetObjectArrayElement_array: jniObject
		index: anIndex-1
		val: aJavaObject asParameter
		onException: [:jex | self jvm throwJavaException: jex].
!

at: anIndex wrapper: aJVMOrJavaStatic
	"answer the element at the 1-based index.
	This is an optimised form for use when you know for sure that the retured object will
	be of a specific class (not a subclass).  See JavaObject>>callObjectMID:withArguments:wrapper
	for more details"

	| answer |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	answer := self jniEnv
			GetObjectArrayElement_array: jniObject
			index: anIndex-1
			onException: [:jex | self jvm throwJavaException: jex].

	^ aJVMOrJavaStatic wrapJNIObject: answer.
!

clone
	"answer a shallow copy of this array"

	^ self clone_null.!

clone_null
	"answer the result of calling the (public for arrays) Java method clone()"

	"since the answer is fixed by Java semantics, and not subject to programmer vagaries,
	we can assume that we know that the new object will be of exactly the same class
	as ourself (this is not true of the Object.clone() method in general)"
	^ self callObjectMethod: 'clone' signature: '()Ljava/lang/Object;' wrapperFactory: static.
!

elementClass
	"answer the class static for the type of our elements"

	^ static elementClass.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, note that the indexes are Smalltalk
	indexes and, as such, 1-based"

	| stop |

	aStartIndex > aStopIndex ifTrue: [^ self].

	#CUtodo.  "is it worth optimising this ?"
	aStartIndex to: aStopIndex do: [:i | a2Block value: i value: (self at: i)].!

size
	"answer the number of elements in the underlying Java array"

	sizeCache isNil ifFalse: [^ sizeCache].

	sizeCache := self jniEnv
				GetArrayLength_array: jniObject
				onException: [:jex | self jvm throwJavaException: jex].

	^ sizeCache.! !
!JavaObjectArray categoriesFor: #alternateInspectorClass!constants!development!public! !
!JavaObjectArray categoriesFor: #at:!accessing!public! !
!JavaObjectArray categoriesFor: #at:put:!accessing!public! !
!JavaObjectArray categoriesFor: #at:wrapper:!accessing!public! !
!JavaObjectArray categoriesFor: #clone!copying!public! !
!JavaObjectArray categoriesFor: #clone_null!Java-methods!public! !
!JavaObjectArray categoriesFor: #elementClass!accessing!public! !
!JavaObjectArray categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!JavaObjectArray categoriesFor: #size!accessing!public! !

!JavaObjectArray class methodsFor!

from: aList jvm: aJVM
	"answer a new JavaObject wrapping an array initialised by copying the elements of aList"

	| new |

	new := self new: aList size jvm: aJVM.
	new asCollection replaceFrom: 1 to: aList size with: aList.

	^ new.!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[Ljava/lang/Object;'.!

new: length elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and the elements are initalised to nil"

	^ self
		new: length
		elementClass: aJavaStatic
		initialValue: nil.
!

new: length elementClass: aJavaStatic initialValue: aJavaObject
	"answer a new object array where the element type is defined by aJavaStatic
	and the elements are initalised to aJavaObject"

	| answer jvm |

	jvm := aJavaStatic jvm.
	answer := jvm jniEnv
			NewObjectArray_len: length
			class: aJavaStatic asParameter
			init: aJavaObject asParameter
			onException: [:jex | jvm throwJavaException: jex].

	^ answer asJavaObject: jvm.!

new: length jvm: aJVM
	"answer a new java.lang.Object[] of the specified size and initialised to nils"

	| answer |

	answer := aJVM jniEnv
			NewObjectArray_len: length
			class: (aJVM findClass: #'java.lang.Object') asParameter
			init: nil
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
!

newWith: aJavaObject elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding aJavaObject"

	^ self
		new: 1
		elementClass: aJavaStatic
		initialValue: aJavaObject.
!

newWith: aJavaObject1 with: aJavaObject2 elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given JavaObjects"

	^ (self new: 2 elementClass: aJavaStatic initialValue: aJavaObject1)
		at: 2 put: aJavaObject2;
		yourself.
!

newWith: aJavaObject1 with: aJavaObject2 with: aJavaObject3 elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given JavaObjects"

	^ (self new: 3 elementClass: aJavaStatic initialValue: aJavaObject1)
		at: 2 put: aJavaObject2;
		at: 3 put: aJavaObject3; 
		yourself.
!

newWithAll: aList elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given JavaObjects"

	| new |

	new := self new: aList size elementClass: aJavaStatic.
	new asCollection replaceFrom: 1 to: aList size with: aList.

	^ new.
! !
!JavaObjectArray class categoriesFor: #from:jvm:!instance creation!public! !
!JavaObjectArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaObjectArray class categoriesFor: #new:elementClass:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #new:elementClass:initialValue:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #newWith:elementClass:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #newWith:with:elementClass:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #newWith:with:with:elementClass:!instance creation!Java constructors!public! !
!JavaObjectArray class categoriesFor: #newWithAll:elementClass:!instance creation!Java constructors!public! !

JavaPrimitiveArray guid: (GUID fromString: '{5062BE45-7840-4004-A94F-6F7EFC33A0E7}')!
JavaPrimitiveArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class that gathers together the common behaviour of Java''s various kinds of arrays.

Note that there is no defined Java class corresponding to *any* array.  Java arrays are always objects, and they have java.lang.Class objects, they all have a public clone() method, and they all have the readonly instance-side field, *but* there is no java.lang.Array class of which they are all (sub)instances.

That may make sense to you; it doesn''t to me.  Sigh...'!
!JavaPrimitiveArray categoriesForClass!Unclassified! !
!JavaPrimitiveArray methodsFor!

alternateInspectorClass
	"answer the class of fallback inspector to be used to inspect the receiver."

	"want to answer the same as Array"
	^ #() alternateInspectorClass.!

at: anInteger
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

at: anIndex put: anObject
	"one of the root methods for <SequenceableCollection>"

	self subclassResponsibility.!

clone
	"answer a shallow copy of this array"

	^ self clone_null.!

clone_null
	"answer the result of calling the (public for arrays) Java method clone()"

	^ self callObjectMethod: 'clone' signature: '()Ljava/lang/Object;'.
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer an ExternalArray with the 'same' data as the closed sub-interval of aCollection"

	self subclassResponsibility.!

elementClass
	"answer the class static for the type of our elements"

	^ static elementClass.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, note that the indexes are Smalltalk
	indexes and, as such, 1-based"

	| stop |

	aStartIndex > aStopIndex ifTrue: [^ self].
	aStartIndex < 1 ifTrue: [self errorSubscriptBounds: aStartIndex].
	aStartIndex > self size ifTrue: [self errorSubscriptBounds: aStartIndex].

	stop := self size min: aStopIndex.
	self withElementsDo: [:elements |
		elements from: aStartIndex to: stop keysAndValuesDo: a2Block].
	stop < aStopIndex ifTrue: [self errorSubscriptBounds: stop + 1].!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements startingAt: aReplacementIndex
	"destructively replace the elements of our Java array  in the range [aStartIndex, aStopIndex]
	with the values contained in the Smalltalk <sequencedReadableCollection>, replacementElements,
	starting at aReplacementIndex.
	For compatability with the Collection implementation, this allows overlapping moves (easy since we
	make a copy of the replacement anyway), and replacement of empty ranges ourside the limits of
	the receiver.
	Answers the receiver"

	| tmp |

	aStartIndex > aStopIndex ifTrue: [^ self].

	tmp := self
		copyForReplace: replacementElements
		from: aReplacementIndex
		to: (aReplacementIndex + aStopIndex - aStartIndex).

	self
		setElements: tmp
		offset: aStartIndex - 1
		length: aStopIndex - aStartIndex + 1.
!

setElements: anExternalArray offset: anOffset length: anInteger
	"copy anInteger elements from anExternalArray into our Java array,
	starting at the zero-based anOffset in the target"

	self subclassResponsibility.
!

size
	"answer the number of elements in the underlying Java array"

	sizeCache isNil ifFalse: [^ sizeCache].

	sizeCache := self jniEnv
				GetArrayLength_array: jniObject
				onException: [:jex | self jvm throwJavaException: jex].

	^ sizeCache.! !
!JavaPrimitiveArray categoriesFor: #alternateInspectorClass!constants!development!public! !
!JavaPrimitiveArray categoriesFor: #at:!accessing!public! !
!JavaPrimitiveArray categoriesFor: #at:put:!accessing!public! !
!JavaPrimitiveArray categoriesFor: #clone!copying!public! !
!JavaPrimitiveArray categoriesFor: #clone_null!Java-methods!public! !
!JavaPrimitiveArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaPrimitiveArray categoriesFor: #elementClass!accessing!public! !
!JavaPrimitiveArray categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!JavaPrimitiveArray categoriesFor: #replaceFrom:to:with:startingAt:!public!replacing! !
!JavaPrimitiveArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaPrimitiveArray categoriesFor: #size!accessing!public! !

!JavaPrimitiveArray class methodsFor!

from: aList jvm: aJVM
	"answer a new JavaObject wrapping an array initialised by copying the elements of aList"

	| new |

	new := self new: aList size jvm: aJVM.
	new replaceFrom: 1 to: aList size with: aList.

	^ new.!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#clone_null
	).
!

new: length elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and the elements are initalised to nil.  This is provided for consistancy with JavaObjectArray,
	and -- in fact -- the class static argument is ignored except to define the JVM to use"

	^ self new: length jvm: aJavaStatic jvm.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	self subclassResponsibility.!

newWith: anObject elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding anObject.
	This is provided for consistancy with JavaObjectArray"

	^ (self new: 1 jvm: aJavaStatic)
		at: 1 put: anObject;
		yourself.!

newWith: anObject1 with: anObject2 elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given JavaObjects.
	This is provided for consistancy with JavaObjectArray"

	^ (self new: 2 jvm: aJavaStatic)
		at: 1 put: anObject1;
		at: 2 put: anObject2;
		yourself.
!

newWith: anObject1 with: anObject2 with: anObject3 elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given JavaObjects.
	This is provided for consistancy with JavaObjectArray"

	^ (self new: 3 jvm: aJavaStatic)
		at: 1 put: anObject1;
		at: 2 put: anObject2;
		at: 3 put: anObject3; 
		yourself.
!

newWithAll: aList elementClass: aJavaStatic
	"answer a new object array where the element type is defined by aJavaStatic
	and holding the given Objects.
	This is provided for consistancy with JavaObjectArray"

	^ self from: aList jvm: aJavaStatic jvm.! !
!JavaPrimitiveArray class categoriesFor: #from:jvm:!instance creation!public! !
!JavaPrimitiveArray class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaPrimitiveArray class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaPrimitiveArray class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaPrimitiveArray class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaPrimitiveArray class categoriesFor: #new:elementClass:!instance creation!Java constructors!public! !
!JavaPrimitiveArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !
!JavaPrimitiveArray class categoriesFor: #newWith:elementClass:!instance creation!Java constructors!public! !
!JavaPrimitiveArray class categoriesFor: #newWith:with:elementClass:!instance creation!Java constructors!public! !
!JavaPrimitiveArray class categoriesFor: #newWith:with:with:elementClass:!instance creation!Java constructors!public! !
!JavaPrimitiveArray class categoriesFor: #newWithAll:elementClass:!instance creation!Java constructors!public! !

JavaBooleanArray guid: (GUID fromString: '{8C379032-A156-40D4-B065-79AC905180D3}')!
JavaBooleanArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java boolean arrays.'!
!JavaBooleanArray categoriesForClass!Unclassified! !
!JavaBooleanArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| byte |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	byte := BYTE new. 
	self jniEnv
			GetBooleanArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: byte
			onException: [:jex | self jvm throwJavaException: jex].
	^ byte value == 1.!

at: anIndex put: aBool
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| byte |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	byte := BYTE new.
	byte value: (aBool ifTrue: [1] ifFalse: [0]).

	self jniEnv
			SetBooleanArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: byte
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer a ByteArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := ByteArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i || val |
		val := (aCollection at: i) ifTrue: [1] ifFalse: [0].
		 copy at: (copyIndex := copyIndex + 1) put: val].

	^ copy.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, note that the indexes are Smalltalk
	indexes and, as such, 1-based"

	| stop |

	aStartIndex > aStopIndex ifTrue: [^ self].
	aStartIndex < 1 ifTrue: [self errorSubscriptBounds: aStartIndex].
	aStartIndex > self size ifTrue: [self errorSubscriptBounds: aStartIndex].

	stop := self size min: aStopIndex.
	self withElementsDo:
		[:words | words from: aStartIndex to: stop keysAndValuesDo:
			[:key :value | a2Block value: key value: (value == 1)]].
	stop < aStopIndex ifTrue: [self errorSubscriptBounds: stop + 1].
!

setElements: aBYTEArray offset: anOffset length: anInteger
	"copy anInteger elements from BYTEArray into our Java array,
	starting at the zero-based anOffset in the target.  Note that the
	array should contain only the values 0 and 1"

	self jniEnv
			SetBooleanArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aBYTEArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary BYTEArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetBooleanArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| bytes |
	bytes := BYTEArray fromAddress: ptr length: length.
	^ a1Block value: bytes]
		ensure: [self jniEnv
				ReleaseBooleanArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaBooleanArray categoriesFor: #at:!accessing!public! !
!JavaBooleanArray categoriesFor: #at:put:!accessing!public! !
!JavaBooleanArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaBooleanArray categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!JavaBooleanArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaBooleanArray categoriesFor: #withElementsDo:!operations!public! !

!JavaBooleanArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[Z'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewBooleanArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaBooleanArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaBooleanArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaByteArray guid: (GUID fromString: '{2B43A65A-3FA4-4082-9A8D-AB6CE3FE251A}')!
JavaByteArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java byte arrays.

Note that Java byte is a *signed* quantity.'!
!JavaByteArray categoriesForClass!Unclassified! !
!JavaByteArray methodsFor!

asByteArray
	"answer a Smalltalk ByteArray with the same bytes (but interpreted as unsigned)
	as the Java byte[] we stand for"

	"HACK: there's an anoying bit of overcleverness in the way Dolphin works here.  If the length
	is 0, then when we try to create a BYTEArray, Dolphin will refuse to wrap the given (valid) address
	in an external structure, instead it insists on creating a blank which consideres itself to be #isNull and
	to have length = 0.  So far this is not a problem.  However, ByteArray>>fromAddress:length: and
	String>>fromAddress:length: both test the address passed and if it's NULL, then they answer nil.  The
	end result is that a perfectly valid 0-length data buffer gets converted into nil rather than an object of
	the required type.  We put in a hack here to avoid it"
	^ self size = 0
		ifTrue: ['']
		ifFalse: [self withBytesDo: [:bytes | bytes asByteArray]].!

asString
	"answer a Smalltalk String with the same bytes (but interpreted as characters)
	as the Java byte[] we stand for"

	"HACK: there's an anoying bit of overcleverness in the way Dolphin works here.  If the length
	is 0, then when we try to create a BYTEArray, Dolphin will refuse to wrap the given (valid) address
	in an external structure, instead it insists on creating a blank which consideres itself to be #isNull and
	to have length = 0.  So far this is not a problem.  However, ByteArray>>fromAddress:length: and
	String>>fromAddress:length: both test the address passed and if it's NULL, then they answer nil.  The
	end result is that a perfectly valid 0-length data buffer gets converted into nil rather than an object of
	the required type.  We put in a hack here to avoid it"

	^ self size = 0
		ifTrue: [^ '']
		ifFalse: [self withBytesDo: [:bytes | String fromAddress: bytes yourAddress length: bytes size]].
!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| sbyte |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sbyte := SBYTE new. 
	self jniEnv
			GetByteArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sbyte
			onException: [:jex | self jvm throwJavaException: jex].
	^ sbyte value.!

at: anIndex put: anInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based, and that the integer should
	be representable as a signed byte (i.e. in the range [-128, 127])"

	| sbyte |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	"SBYTE>>value: doesn't check, so we have to do it manually"
	(anInteger < -128 or: [anInteger > 127]) ifTrue: [self errorCantHold: anInteger].

	sbyte := SBYTE new.
	sbyte value: anInteger.

	self jniEnv
			SetByteArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sbyte
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer an SBYTEArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := SBYTEArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i) asInteger].

	^ copy.!

setElements: anSBYTEArray offset: anOffset length: anInteger
	"copy anInteger elements from anSBYTEArray into our Java array,
	starting at the zero-based anOffset in the target.  (Actually this will
	also work any byte object such as a ByteArray or String)"

	self jniEnv
			SetByteArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: anSBYTEArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withBytesDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary BYTEArray.
	Changes made to the array will be copied back into Java space.
	NB: these are unsigned bytes, not Java's signed bytes.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetByteArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| bytes |
	bytes := BYTEArray fromAddress: ptr length: length.
	^ a1Block value: bytes]
		ensure: [self jniEnv
				ReleaseByteArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary SBYTEArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.

	"BUG: see note in JavaLangString>>withBytesDo:"
	length = 0 ifTrue: [^ a1Block value: #[]].

	ptr := self jniEnv
			GetByteArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| sbytes |
	sbytes := SBYTEArray fromAddress: ptr length: length.
	^ a1Block value: sbytes]
		ensure: [self jniEnv
				ReleaseByteArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaByteArray categoriesFor: #asByteArray!converting!public! !
!JavaByteArray categoriesFor: #asString!converting!public! !
!JavaByteArray categoriesFor: #at:!accessing!public! !
!JavaByteArray categoriesFor: #at:put:!accessing!public! !
!JavaByteArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaByteArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaByteArray categoriesFor: #withBytesDo:!accessing!public! !
!JavaByteArray categoriesFor: #withElementsDo:!operations!public! !

!JavaByteArray class methodsFor!

fromByteArray: aByteArray jvm: aJVM
	"answer a new JavaObject wrapping a byte[] which has been created by copying the bytes
	in aByteArray"

	^ super from: aByteArray jvm: aJVM.!

fromString: aString jvm: aJVM
	"answer a new JavaObject wrapping a java.lang.String which has been created by copying the characters
	in aString"

	^ super from: aString jvm: aJVM.!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[B'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewByteArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaByteArray class categoriesFor: #fromByteArray:jvm:!instance creation!public! !
!JavaByteArray class categoriesFor: #fromString:jvm:!instance creation!public! !
!JavaByteArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaByteArray class categoriesFor: #new:jvm:!Java constructors!public! !

JavaCharArray guid: (GUID fromString: '{0342D2AA-F767-4DE6-93CC-B10E0C71191D}')!
JavaCharArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java char arrays.

Chars can be specified as Smalltalk Characters or as integers.  Char values of elements are converted to Characters if they fit in 8bits, or are returned as Integers otherwise.'!
!JavaCharArray categoriesForClass!Unclassified! !
!JavaCharArray methodsFor!

asJavaCharArray: aJVM
	"Answer a JavaCharArray object which is owned by aJVM."

	^ aJVM == self jvm
		ifTrue: [self]
		ifFalse: [self class from: self asCollection jvm: aJVM].
!

asString
	"answer a Smalltalk String with the same bytes (but interpreted as characters)
	as the Java byte[] we stand for"

	| string |

	"HACK: see the note in JavaByteArray #asString"
	self size = 0 ifTrue: [^ ''].

	string := String new: self size.
	self withElementsDo: [:words | words keysAndValuesDo: [:i :each | string basicAt: i put: each]].

	^ string.!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based.
	Answers a Character or an Integer depending on whether the answer will fit in an 8-bit
	Smalltalk Character"

	| word ch |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	word := WORD new. 
	self jniEnv
			GetCharArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: word
			onException: [:jex | self jvm throwJavaException: jex].
	ch := word value.

	^ ch < 256 ifTrue: [Character value: ch] ifFalse: [ch].!

at: anIndex put: aCharacterOrInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| word |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	word := WORD new.
	word value: aCharacterOrInteger asInteger.

	self jniEnv
			SetCharArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: word
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer a WORDArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := WORDArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i || val |
		val := (aCollection at: i) asInteger.
		copy at: (copyIndex := copyIndex + 1) put: val].

	^ copy.!

from: aStartIndex to: aStopIndex keysAndValuesDo: a2Block
	"one of the root methods for <SequenceableCollection>, note that the indexes are Smalltalk
	indexes and, as such, 1-based"

	| stop |

	aStartIndex > aStopIndex ifTrue: [^ self].
	aStartIndex < 1 ifTrue: [self errorSubscriptBounds: aStartIndex].
	aStartIndex > self size ifTrue: [self errorSubscriptBounds: aStartIndex].

	stop := self size min: aStopIndex.
	self withElementsDo:
		[:words | words from: aStartIndex to: stop keysAndValuesDo:
			[:key :value || ch |
			ch := value.
			ch < 256 ifTrue: [ch := Character value: ch].
			a2Block value: key value: ch]].
	stop < aStopIndex ifTrue: [self errorSubscriptBounds: stop + 1].
!

setElements: aWORDArray offset: anOffset length: anInteger
	"copy anInteger elements from aWORDArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetCharArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aWORDArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary WORDArray
	of chars (unsigned 16bit words) in Java's native format (UTF16) as its parameter.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetCharArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| chars |
	chars := WORDArray fromAddress: ptr length: length.
	^ a1Block value: chars]
		ensure: [self jniEnv
				ReleaseCharArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaCharArray categoriesFor: #asJavaCharArray:!converting!public! !
!JavaCharArray categoriesFor: #asString!converting!public! !
!JavaCharArray categoriesFor: #at:!accessing!public! !
!JavaCharArray categoriesFor: #at:put:!accessing!public! !
!JavaCharArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaCharArray categoriesFor: #from:to:keysAndValuesDo:!enumerating!public! !
!JavaCharArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaCharArray categoriesFor: #withElementsDo:!operations!public! !

!JavaCharArray class methodsFor!

fromByteArray: aByteArray jvm: aJVM
	"answer a new JavaObject wrapping a byte[] which has been created by copying the bytes
	in aByteArray"

	^ super from: aByteArray jvm: aJVM.!

fromString: aString jvm: aJVM
	"answer a new JavaObject wrapping a java.lang.String which has been created by copying the characters
	in aString"

	^ super from: aString jvm: aJVM.!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[C'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewCharArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaCharArray class categoriesFor: #fromByteArray:jvm:!instance creation!public! !
!JavaCharArray class categoriesFor: #fromString:jvm:!instance creation!public! !
!JavaCharArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaCharArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaDoubleArray guid: (GUID fromString: '{4D16E516-AE35-4267-B82D-0B294874C7F8}')!
JavaDoubleArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java double arrays.'!
!JavaDoubleArray categoriesForClass!Unclassified! !
!JavaDoubleArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| double |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	double := DOUBLE new. 
	self jniEnv
			GetDoubleArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: double
			onException: [:jex | self jvm throwJavaException: jex].
	^ double value.!

at: anIndex put: aInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| double |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	double := DOUBLE new.
	double value: aInteger.

	self jniEnv
			SetDoubleArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: double
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer a DOUBLEArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := DOUBLEArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i)].

	^ copy.!

setElements: aDOUBLEArray offset: anOffset length: anInteger
	"copy anInteger elements from DOUBLEArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetDoubleArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aDOUBLEArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary DOUBLEArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetDoubleArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| doubles |
	doubles := DOUBLEArray fromAddress: ptr length: length.
	^ a1Block value: doubles]
		ensure: [self jniEnv
				ReleaseDoubleArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaDoubleArray categoriesFor: #at:!accessing!public! !
!JavaDoubleArray categoriesFor: #at:put:!accessing!public! !
!JavaDoubleArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaDoubleArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaDoubleArray categoriesFor: #withElementsDo:!accessing!public! !

!JavaDoubleArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[D'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewDoubleArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaDoubleArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaDoubleArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaFloatArray guid: (GUID fromString: '{444958FB-9D3F-4BDD-A988-E2B64BE50564}')!
JavaFloatArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java float arrays.'!
!JavaFloatArray categoriesForClass!Unclassified! !
!JavaFloatArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| float |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	float := FLOAT new. 
	self jniEnv
			GetFloatArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: float
			onException: [:jex | self jvm throwJavaException: jex].
	^ float value.!

at: anIndex put: aInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| float |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	float := FLOAT new.
	float value: aInteger.

	self jniEnv
			SetFloatArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: float
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer a FLOATArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := FLOATArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i)].

	^ copy.!

setElements: aFLOATArray offset: anOffset length: anInteger
	"copy anInteger elements from FLOATArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetFloatArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aFLOATArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary FLOATArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetFloatArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| floats |
	floats := FLOATArray fromAddress: ptr length: length.
	^ a1Block value: floats]
		ensure: [self jniEnv
				ReleaseFloatArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaFloatArray categoriesFor: #at:!accessing!public! !
!JavaFloatArray categoriesFor: #at:put:!accessing!public! !
!JavaFloatArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaFloatArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaFloatArray categoriesFor: #withElementsDo:!accessing!public! !

!JavaFloatArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[F'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewFloatArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaFloatArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaFloatArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaIntArray guid: (GUID fromString: '{E6140918-8C7F-4AF2-8604-0A6C58CC0308}')!
JavaIntArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java int arrays.'!
!JavaIntArray categoriesForClass!Unclassified! !
!JavaIntArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| sdword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sdword := SDWORD new. 
	self jniEnv
			GetIntArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sdword
			onException: [:jex | self jvm throwJavaException: jex].
	^ sdword value.!

at: anIndex put: aInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| sdword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sdword := SDWORD new.
	sdword value: aInteger.

	self jniEnv
			SetIntArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sdword
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer an SDWORDArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := SDWORDArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i)].

	^ copy.!

setElements: aSDWORDArray offset: anOffset length: anInteger
	"copy anInteger elements from SDWORDArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetIntArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aSDWORDArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary SDWORDArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetIntArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| sdwords |
	sdwords := SDWORDArray fromAddress: ptr length: length.
	^ a1Block value: sdwords]
		ensure: [self jniEnv
				ReleaseIntArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaIntArray categoriesFor: #at:!accessing!public! !
!JavaIntArray categoriesFor: #at:put:!accessing!public! !
!JavaIntArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaIntArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaIntArray categoriesFor: #withElementsDo:!accessing!public! !

!JavaIntArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[I'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewIntArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaIntArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaIntArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaLongArray guid: (GUID fromString: '{2B2C667D-C313-429D-B9F2-E9332ABCCEF5}')!
JavaLongArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java arrays of long.'!
!JavaLongArray categoriesForClass!Unclassified! !
!JavaLongArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| sqword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sqword := SQWORD new. 
	self jniEnv
			GetLongArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sqword
			onException: [:jex | self jvm throwJavaException: jex].
	^ sqword value.!

at: anIndex put: aInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| sqword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sqword := SQWORD new.
	sqword value: aInteger.

	self jniEnv
			SetLongArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sqword yourAddress
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer an SQWORDArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := SQWORDArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i)].

	^ copy.!

setElements: aSQWORDArray offset: anOffset length: anInteger
	"copy anInteger elements from SQWORDArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetLongArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aSQWORDArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary SQWORDArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetLongArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| sqwords |
	sqwords := SQWORDArray fromAddress: ptr length: length.
	^ a1Block value: sqwords]
		ensure: [self jniEnv
				ReleaseLongArrayElements_array: jniObject
				elems: ptr
				mode: 0
				onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaLongArray categoriesFor: #at:!accessing!public! !
!JavaLongArray categoriesFor: #at:put:!accessing!public! !
!JavaLongArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaLongArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaLongArray categoriesFor: #withElementsDo:!accessing!public! !

!JavaLongArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[J'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewLongArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.
! !
!JavaLongArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaLongArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaShortArray guid: (GUID fromString: '{F0B161A4-FE45-4388-9789-11A170E7635A}')!
JavaShortArray comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to Java arrays of short.'!
!JavaShortArray categoriesForClass!Unclassified! !
!JavaShortArray methodsFor!

at: anIndex
	"one of the root methods for <SequenceableCollection>, note that the index is for Smalltalk
	and, as such, 1-based"

	| sword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sword := SWORD new. 
	self jniEnv
			GetShortArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sword
			onException: [:jex | self jvm throwJavaException: jex].
	^ sword value.!

at: anIndex put: aInteger
	"one of the root methods for <SequenceableCollection>,
	note that the index is 1-based"

	| sword |

	anIndex < 1 ifTrue: [self errorSubscriptBounds: anIndex].
	anIndex > self size ifTrue: [self errorSubscriptBounds: anIndex].

	sword := SWORD new.
	sword value: aInteger.

	self jniEnv
			SetShortArrayRegion_array: jniObject
			start: anIndex-1
			len: 1
			buf: sword
			onException: [:jex | self jvm throwJavaException: jex].
!

copyForReplace: aCollection from: aStartIndex to: aStopIndex
	"private -- answer an SWORDArray with the 'same' data as the closed sub-interval of aCollection"

	| copy copyIndex |

	copy := SWORDArray new: aStopIndex - aStartIndex + 1.

	copyIndex := 0.
	aStartIndex to: aStopIndex do:
		[:i | copy at: (copyIndex := copyIndex + 1) put: (aCollection at: i)].

	^ copy.!

setElements: aSWORDArray offset: anOffset length: anInteger
	"copy anInteger elements from SWORDArray into our Java array,
	starting at the zero-based anOffset in the target"

	self jniEnv
			SetShortArrayRegion_array: jniObject
			start: anOffset
			len: anInteger
			buf: aSWORDArray
			onException: [:jex | self jvm throwJavaException: jex].
!

withElementsDo: a1Block
	"answer the result of evaluating a1Block, passing it a temporary SWORDArray.
	Changes made to the array will be copied back into Java space.
	DO NOT KEEP A REFERENCE TO THE ARRAY"

	| length ptr |

	length := self size.
	ptr := self jniEnv
			GetShortArrayElements_array: jniObject
			isCopy: nil
			onException: [:jex | self jvm throwJavaException: jex].

	[| swords |
	swords := SWORDArray fromAddress: ptr length: length.
	^ a1Block value: swords]
		ensure: [self jniEnv
			ReleaseShortArrayElements_array: jniObject
			elems: ptr
			mode: 0
			onException: [:jex | self jvm throwJavaException: jex]].
! !
!JavaShortArray categoriesFor: #at:!accessing!public! !
!JavaShortArray categoriesFor: #at:put:!accessing!public! !
!JavaShortArray categoriesFor: #copyForReplace:from:to:!helpers!private! !
!JavaShortArray categoriesFor: #setElements:offset:length:!operations!public! !
!JavaShortArray categoriesFor: #withElementsDo:!accessing!public! !

!JavaShortArray class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	"have to use the JNI name"
	^ #'[S'.!

new: length jvm: aJVM
	"answer a new 'instance' of the array class we stand for, which is of the specified length"

	| answer |

	answer := aJVM jniEnv
			NewShortArray_len: length
			onException: [:jex | aJVM throwJavaException: jex].

	^ answer asJavaObject: aJVM.! !
!JavaShortArray class categoriesFor: #javaClassName!accessing!constants!public! !
!JavaShortArray class categoriesFor: #new:jvm:!instance creation!Java constructors!public! !

JavaNetURLClassLoader guid: (GUID fromString: '{ECB1C7CE-28F6-42FA-BB81-A7ED78A96439}')!
JavaNetURLClassLoader comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.ClassLoader.

There''s no real need for a wrapper for this class, but since I''ve added one for the class-side, I though I might as well do one for the instance-side too.'!
!JavaNetURLClassLoader categoriesForClass!Unclassified! !
!JavaNetURLClassLoader methodsFor!

findResource_String: aString1
	"answer the result of calling the receiver's public findResource(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'findResource' signature: '(Ljava/lang/String;)Ljava/net/URL;' withArguments: args.
!

findResources_String: aString1
	"answer the result of calling the receiver's public findResources(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'findResources' signature: '(Ljava/lang/String;)Ljava/util/Enumeration;' withArguments: args.
!

getURLs_null
	"answer the result of calling the receiver's public getURLs() Java method"

	^ self callObjectMethod: 'getURLs' signature: '()[Ljava/net/URL;'.
! !
!JavaNetURLClassLoader categoriesFor: #findResource_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaNetURLClassLoader categoriesFor: #findResources_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaNetURLClassLoader categoriesFor: #getURLs_null!**auto generated**!Java-methods!Java-public!public! !

!JavaNetURLClassLoader class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#findResource_String:
		#findResources_String:
		#getURLs_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.net.URLClassLoader'.
! !
!JavaNetURLClassLoader class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaNetURLClassLoader class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaNetURLClassLoader class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaNetURLClassLoader class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaNetURLClassLoader class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaLangClass guid: (GUID fromString: '{D8A7CE14-A000-4D9D-8BB9-F63DDB350B10}')!
JavaLangClass comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for java.lang.Class.  Intstances are *always* paired with a JavaStatic.  See the comment on that class for details.  The class static can be reached by sending #classStatic to instances.  (Don''t send #static, that will return the class static for the Java class java.lang.Class, of which we are an instance, rather than the Java class we represent.)

BTW. java.lang.Class isn''t actually a subclass of java,lang.reflect.AccessibleObject, but it is convenient to treat it as if it were since we can then just inherit rather than duplicate the methods from that class.  In fact java.lang.Class is *supposed* to implement an interface which is shared with the other reflection objects, but -- as of J2SDK1.4 beta3 -- it doesn''t.'!
!JavaLangClass categoriesForClass!Unclassified! !
!JavaLangClass methodsFor!

asJavaLangClass
	"answer the receiver converted to a class object"

	^ self.!

asStatic
	"answer the receiver converted to a class static"

	^ classStatic.!

classfileBytes
	"make an attempt to supply our classfile's source byte array.
	Answers nil if it can't supply"

	^ (self getResourceAsStream_String: ('/' , self name asJNIClassName , '.class'))
		ifNotNil: [:stream | [stream upToEnd] ensure: [stream close]].!

classfileURL
	"make an attempt to supply the java.net.URL source of our classfile.
	Answers nil if it can't supply"

	^ self getResource_String: ('/' , self name asJNIClassName , '.class')!

classloader
	"answer this class's java.lang.ClassLoader"

	^ self getClassLoader_null.!

classLoader

	"deprecated in favour of a more consistant naming convention"
	Notification deprecated.
	^ self classloader.!

classStatic
	"answer the JavaStatic instance corresponding to this Java class. Note that this
	is *not* the same as our 'static'"

	^ classStatic.!

classStatic: aJavaStatic
	"private -- set the class static which is twinned to this JavaLangClass"

	classStatic := aJavaStatic.
!

declaredClasses
	"answer an Array of JavaLangClasses corresponding to the result of our Java getDeclaredClasses() method"

	^ self getDeclaredClasses_null collect: [:each | each classStatic].!

declaredConstructors
	"answer an Array of JavaLangReflectConstructors corresponding to the result of our Java getDeclaredConstructors() method"

	^ self getDeclaredConstructors_null asCollection.!

declaredFields
	"answer an Array of JavaLangReflectFields corresponding to the result of our Java getDeclaredFields() method"

	^ self getDeclaredFields_null asCollection.
!

declaredInterfaces
	"answer an Array of JavaLangReflectInterfaces corresponding to the result of our Java getInterfaces() method"

	^ self getInterfaces_null asCollection.!

declaredMethods
	"answer an Array of JavaLangReflectMethods corresponding to the result of our Java getDeclaredMethods() method"

	^ self getDeclaredMethods_null asCollection.!

desiredAssertionStatus_null
	"answer the result of calling the receiver's public desiredAssertionStatus() Java method"

	^ self callBooleanMethod: 'desiredAssertionStatus'.
!

getClasses_null
	"answer the result of calling the receiver's public getClasses() Java method"

	^ self callObjectMethod: 'getClasses' signature: '()[Ljava/lang/Class;'.
!

getClassLoader_null
	"answer the result of calling the receiver's public getClassLoader() Java method"

	^ self callObjectMethod: 'getClassLoader' signature: '()Ljava/lang/ClassLoader;'.
!

getComponentType_null
	"answer the result of calling the receiver's public native getComponentType() Java method"

	^ self callObjectMethod: 'getComponentType' signature: '()Ljava/lang/Class;'.
!

getConstructor_ClassArray: aClasses1
	"answer the result of calling the receiver's public getConstructor(java.lang.Class[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getConstructor' signature: '([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;' withArguments: args.
!

getConstructors_null
	"answer the result of calling the receiver's public getConstructors() Java method"

	^ self callObjectMethod: 'getConstructors' signature: '()[Ljava/lang/reflect/Constructor;'.
!

getDeclaredClasses_null
	"answer the result of calling the receiver's public getDeclaredClasses() Java method"

	^ self callObjectMethod: 'getDeclaredClasses' signature: '()[Ljava/lang/Class;'.
!

getDeclaredConstructor_ClassArray: aClasses1
	"answer the result of calling the receiver's public getDeclaredConstructor(java.lang.Class[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getDeclaredConstructor' signature: '([Ljava/lang/Class;)Ljava/lang/reflect/Constructor;' withArguments: args.
!

getDeclaredConstructors_null
	"answer the result of calling the receiver's public getDeclaredConstructors() Java method"

	^ self callObjectMethod: 'getDeclaredConstructors' signature: '()[Ljava/lang/reflect/Constructor;'.
!

getDeclaredField_String: aString1
	"answer the result of calling the receiver's public getDeclaredField(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getDeclaredField' signature: '(Ljava/lang/String;)Ljava/lang/reflect/Field;' withArguments: args.
!

getDeclaredFields_null
	"answer the result of calling the receiver's public getDeclaredFields() Java method"

	^ self callObjectMethod: 'getDeclaredFields' signature: '()[Ljava/lang/reflect/Field;'.
!

getDeclaredMethod_String: aString1 ClassArray: aClasses1
	"answer the result of calling the receiver's public getDeclaredMethod(java.lang.String, java.lang.Class[]) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getDeclaredMethod' signature: '(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;' withArguments: args.
!

getDeclaredMethods_null
	"answer the result of calling the receiver's public getDeclaredMethods() Java method"

	^ self callObjectMethod: 'getDeclaredMethods' signature: '()[Ljava/lang/reflect/Method;'.
!

getField_String: aString1
	"answer the result of calling the receiver's public getField(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getField' signature: '(Ljava/lang/String;)Ljava/lang/reflect/Field;' withArguments: args.
!

getFields_null
	"answer the result of calling the receiver's public getFields() Java method"

	^ self callObjectMethod: 'getFields' signature: '()[Ljava/lang/reflect/Field;'.
!

getInterfaces_null
	"answer the result of calling the receiver's public native getInterfaces() Java method"

	^ self callObjectMethod: 'getInterfaces' signature: '()[Ljava/lang/Class;'.
!

getMethod_String: aString1 ClassArray: aClasses1
	"answer the result of calling the receiver's public getMethod(java.lang.String, java.lang.Class[]) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getMethod' signature: '(Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;' withArguments: args.
!

getMethods_null
	"answer the result of calling the receiver's public getMethods() Java method"

	^ self callObjectMethod: 'getMethods' signature: '()[Ljava/lang/reflect/Method;'.
!

getPackage_null
	"answer the result of calling the receiver's public getPackage() Java method"

	^ self callObjectMethod: 'getPackage' signature: '()Ljava/lang/Package;'.
!

getProtectionDomain_null
	"answer the result of calling the receiver's public getProtectionDomain() Java method"

	^ self callObjectMethod: 'getProtectionDomain' signature: '()Ljava/security/ProtectionDomain;'.
!

getResource_String: aString1
	"answer the result of calling the receiver's public getResource(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getResource' signature: '(Ljava/lang/String;)Ljava/net/URL;' withArguments: args.
!

getResourceAsStream_String: aString1
	"answer the result of calling the receiver's public getResourceAsStream(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getResourceAsStream' signature: '(Ljava/lang/String;)Ljava/io/InputStream;' withArguments: args.
!

getSigners_null
	"answer the result of calling the receiver's public native getSigners() Java method"

	^ self callObjectMethod: 'getSigners' signature: '()[Ljava/lang/Object;'.
!

getSuperclass_null
	"answer the result of calling the receiver's public native getSuperclass() Java method"

	^ self callObjectMethod: 'getSuperclass' signature: '()Ljava/lang/Class;'.
!

isArray
	"answer whether we stand for an array class"

	"ask our class static to work it out since it can do it quickly"
	^ classStatic isArray.!

isArray_null
	"answer the result of calling the receiver's public native isArray() Java method"

	^ self callBooleanMethod: 'isArray'.
!

isAssignableFrom: aJavaLangClass
	"answer whether aJavaLangClass is derived from the Java class or interface we stand for"

	| answer |

	"we use the JNIEnv function directly, it's much faster"
	answer := self jniEnv
			IsAssignableFrom_sub: aJavaLangClass asParameter
			sup: jniObject
			onException: [:jex | self jvm throwJavaException: jex].

	^ answer == 1.
!

isAssignableFrom_Class: aClass1
	"answer the result of calling the receiver's public native isAssignableFrom(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callBooleanMethod: 'isAssignableFrom' signature: '(Ljava/lang/Class;)Z' withArguments: args.
!

isDerivedFrom: aJavaLangClass
	"answer whether this class or interface is derived from (is a subclass of, or implements
	the interface defined by) aJavaLangClass.
	This is just the reverse of #isAssignableFrom: and is defined because I cannot ever work
	out what #isAssignableFrom: really means"

	| answer |

	"we use the JNIEnv function directly, it's much faster"
	answer := self jniEnv
			IsAssignableFrom_sub: jniObject
			sup: aJavaLangClass asParameter
			onException: [:jex | self jvm throwJavaException: jex].

	^ answer == 1.
!

isInstance: aJavaInstance
	"answer whether aJavaInstance is of a class derived from the Java class or interface we stand for"

	| answer |

	"we use the JNIEnv function directly, it's much faster -- 3.7 usecs vs. 27.3 usecs on my machine"

	answer := self jniEnv
			IsAssignableFrom_sub: aJavaInstance javaClassObject asParameter
			sup: jniObject
			onException: [:jex | self jvm throwJavaException: jex].

	^ answer == 1.
!

isInstance_Object: anObject1
	"answer the result of calling the receiver's public native isInstance(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'isInstance' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

isInterface
	"answer whether we stand for an Java interface 'class'"

	"ask our class static to work it out since it can do it quickly"
	^ classStatic isInterface.
!

isInterface_null
	"answer the result of calling the receiver's public native isInterface() Java method"

	^ self callBooleanMethod: 'isInterface'.
!

isPrimitive
	"answer whether we stand for an Java primitive class (int, float, etc)"

	"ask our class static to work it out since it can do it quickly"
	^ classStatic isPrimitive.
!

isPrimitive_null
	"answer the result of calling the receiver's public native isPrimitive() Java method"

	^ self callBooleanMethod: 'isPrimitive'.
!

isSubclassOf: aJavaLangClass
	"answer whether this Java class includes the Java class defined by aJavaLangClass in
	its inheritance chain.
	Note: this does *not* include interfaces"

	"ask our class static to work it out since it can do it quickly"
	^ classStatic isSubclassOf: aJavaLangClass classStatic.
!

javaSuperclass
	"answer the JavaLangClass corresponding to our Java superclass, or nil if we haven't got one"

	"ask our class static to work it out since it can do it quickly"
	^ classStatic javaSuperclassDo: [:it | it classObject].!

javaSuperclassIfNone: a0Block
	"answer the JavaLangClass corresponding to our Java superclass, or the result of evaluating
	the <niladicValuable>, a0Block, if there isn't one"

	"ask our class static to work it out since it can do it quickly"
	classStatic javaSuperclassDo: [:it | ^ it classObject].

	^  a0Block value.!

jniSignature
	"answer a JNI-style signature for this class.
	(note that this is a fairly expensive operation)"

	^ classStatic jniSignature.!

newInstance_null
	"answer the result of calling the receiver's public newInstance() Java method"

	^ self callObjectMethod: 'newInstance' signature: '()Ljava/lang/Object;'.
!

protectionDomain
	"answer this class's java.security.ProtectionDomain"

	^ self getProtectionDomain_null.! !
!JavaLangClass categoriesFor: #asJavaLangClass!converting!public! !
!JavaLangClass categoriesFor: #asStatic!converting!public! !
!JavaLangClass categoriesFor: #classfileBytes!accessing!public! !
!JavaLangClass categoriesFor: #classfileURL!accessing!public! !
!JavaLangClass categoriesFor: #classloader!accessing!public! !
!JavaLangClass categoriesFor: #classLoader!accessing!public! !
!JavaLangClass categoriesFor: #classStatic!accessing!public! !
!JavaLangClass categoriesFor: #classStatic:!initializing!private! !
!JavaLangClass categoriesFor: #declaredClasses!public!reflection! !
!JavaLangClass categoriesFor: #declaredConstructors!public!reflection! !
!JavaLangClass categoriesFor: #declaredFields!public!reflection! !
!JavaLangClass categoriesFor: #declaredInterfaces!public!reflection! !
!JavaLangClass categoriesFor: #declaredMethods!public!reflection! !
!JavaLangClass categoriesFor: #desiredAssertionStatus_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getClasses_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getClassLoader_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getComponentType_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #getConstructor_ClassArray:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getConstructors_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredClasses_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredConstructor_ClassArray:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredConstructors_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredField_String:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredFields_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredMethod_String:ClassArray:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getDeclaredMethods_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getField_String:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getFields_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getInterfaces_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #getMethod_String:ClassArray:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getMethods_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getPackage_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getProtectionDomain_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getResource_String:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getResourceAsStream_String:!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #getSigners_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #getSuperclass_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isArray!public!testing! !
!JavaLangClass categoriesFor: #isArray_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isAssignableFrom:!Java class hierarchy!public!testing! !
!JavaLangClass categoriesFor: #isAssignableFrom_Class:!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isDerivedFrom:!Java class hierarchy!public!testing! !
!JavaLangClass categoriesFor: #isInstance:!public!testing! !
!JavaLangClass categoriesFor: #isInstance_Object:!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isInterface!public!testing! !
!JavaLangClass categoriesFor: #isInterface_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isPrimitive!public!testing! !
!JavaLangClass categoriesFor: #isPrimitive_null!Java-methods!Java-native!Java-public!public! !
!JavaLangClass categoriesFor: #isSubclassOf:!Java class hierarchy!public!testing! !
!JavaLangClass categoriesFor: #javaSuperclass!Java class hierarchy!public! !
!JavaLangClass categoriesFor: #javaSuperclassIfNone:!Java class hierarchy!public! !
!JavaLangClass categoriesFor: #jniSignature!accessing!public! !
!JavaLangClass categoriesFor: #newInstance_null!Java-methods!Java-public!public! !
!JavaLangClass categoriesFor: #protectionDomain!accessing!public! !

!JavaLangClass class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaObjectArray.!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#desiredAssertionStatus_null
		#getClasses_null
		#getClassLoader_null
		#getComponentType_null
		#getConstructor_ClassArray:
		#getConstructors_null
		#getDeclaredClasses_null
		#getDeclaredConstructor_ClassArray:
		#getDeclaredConstructors_null
		#getDeclaredField_String:
		#getDeclaredFields_null
		#getDeclaredMethod_String:ClassArray:
		#getDeclaredMethods_null
		#getField_String:
		#getFields_null
		#getInterfaces_null
		#getMethod_String:ClassArray:
		#getMethods_null
		#getPackage_null
		#getProtectionDomain_null
		#getResource_String:
		#getResourceAsStream_String:
		#getSigners_null
		#getSuperclass_null
		#isArray_null
		#isAssignableFrom_Class:
		#isInstance_Object:
		#isInterface_null
		#isPrimitive_null
		#newInstance_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.Class'.
! !
!JavaLangClass class categoriesFor: #arrayClass!constants!public! !
!JavaLangClass class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangClass class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangClass class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangClass class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangClass class categoriesFor: #javaClassName!accessing!constants!public! !

JavaLangReflectConstructor guid: (GUID fromString: '{D826BD59-64EF-49C2-A973-400C883B7051}')!
JavaLangReflectConstructor comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.reflect.Constructor'!
!JavaLangReflectConstructor categoriesForClass!Unclassified! !
!JavaLangReflectConstructor methodsFor!

argumentCount
	"answer how many arguments this method takes"

	^ self parameterTypes size.!

exceptionTypes
	"answer an Array of class statics corresponding to the resut of the getParameterTypes() Java method"

	"this is not often called so we don't cache the result"
	^ self getExceptionTypes_null collect: [:each | each classStatic].!

getExceptionTypes_null
	"answer the result of calling the receiver's public getExceptionTypes() Java method"

	^ self callObjectMethod: 'getExceptionTypes' signature: '()[Ljava/lang/Class;'.
!

getParameterTypes_null
	"answer the result of calling the receiver's public getParameterTypes() Java method"

	^ self callObjectMethod: 'getParameterTypes' signature: '()[Ljava/lang/Class;'.
!

jniSignature
	"answer a JNI-style signature for this member"

	| str |

	str := String writeStream.
	str nextPutAll: '('.
	self parameterTypes do: [:each | str nextPutAll: each jniSignature].
	str nextPutAll: ')V'.

	^ str contents.!

methodID
	"answer a JNIMethodID for this field"

	^self jniEnv
		FromReflectedMethod_method: jniObject
		onException: [:jex | self jvm throwJavaException: jex].

!

newInstance_ObjectArray: anObjects1
	"answer the result of calling the receiver's public newInstance(java.lang.Object[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObjects1;
			yourself.

	^ self callObjectMethod: 'newInstance' signature: '([Ljava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

parameterTypes
	"answer an Array of class statics corresponding to the resut of the getParameterTypes() Java method"

	parameterTypesCache isNil ifTrue:
		[parameterTypesCache := self getParameterTypes_null collect: [:each | each classStatic]].

	^ parameterTypesCache.!

type
	"answer the type of this constructor, ie the class static of the constructed instances.
	This is actually a bit iffy since there is a real sense that Java constructors have return
	type void (as seen by the JVM).  But it helps..."

	^ self declaredIn.! !
!JavaLangReflectConstructor categoriesFor: #argumentCount!accessing!public! !
!JavaLangReflectConstructor categoriesFor: #exceptionTypes!public!reflection! !
!JavaLangReflectConstructor categoriesFor: #getExceptionTypes_null!Java-methods!Java-public!public! !
!JavaLangReflectConstructor categoriesFor: #getParameterTypes_null!Java-methods!Java-public!public! !
!JavaLangReflectConstructor categoriesFor: #jniSignature!accessing!public! !
!JavaLangReflectConstructor categoriesFor: #methodID!accessing!public! !
!JavaLangReflectConstructor categoriesFor: #newInstance_ObjectArray:!Java-methods!Java-public!public! !
!JavaLangReflectConstructor categoriesFor: #parameterTypes!public!reflection! !
!JavaLangReflectConstructor categoriesFor: #type!public!reflection! !

!JavaLangReflectConstructor class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#getExceptionTypes_null
		#getParameterTypes_null
		#newInstance_ObjectArray:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.reflect.Constructor'.
! !
!JavaLangReflectConstructor class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectConstructor class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectConstructor class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectConstructor class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectConstructor class categoriesFor: #javaClassName!accessing!constants!public! !

JavaLangReflectField guid: (GUID fromString: '{F6A7FC37-7954-4130-85A6-D0EDAC2A7E7B}')!
JavaLangReflectField comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.reflect.Field'!
!JavaLangReflectField categoriesForClass!Unclassified! !
!JavaLangReflectField methodsFor!

fieldID
	"answer a JNIFieldID for this field"

	^self jniEnv
		FromReflectedField_field: jniObject
		onException: [:jex | self jvm throwJavaException: jex].
!

get_Object: anObject1
	"answer the result of calling the receiver's public get(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'get' signature: '(Ljava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

getBoolean_Object: anObject1
	"answer the result of calling the receiver's public getBoolean(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'getBoolean' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

getByte_Object: anObject1
	"answer the result of calling the receiver's public getByte(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callByteMethod: 'getByte' signature: '(Ljava/lang/Object;)B' withArguments: args.
!

getChar_Object: anObject1
	"answer the result of calling the receiver's public getChar(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callCharMethod: 'getChar' signature: '(Ljava/lang/Object;)C' withArguments: args.
!

getDouble_Object: anObject1
	"answer the result of calling the receiver's public getDouble(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callDoubleMethod: 'getDouble' signature: '(Ljava/lang/Object;)D' withArguments: args.
!

getFloat_Object: anObject1
	"answer the result of calling the receiver's public getFloat(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callFloatMethod: 'getFloat' signature: '(Ljava/lang/Object;)F' withArguments: args.
!

getInt_Object: anObject1
	"answer the result of calling the receiver's public getInt(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callIntMethod: 'getInt' signature: '(Ljava/lang/Object;)I' withArguments: args.
!

getLong_Object: anObject1
	"answer the result of calling the receiver's public getLong(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callLongMethod: 'getLong' signature: '(Ljava/lang/Object;)J' withArguments: args.
!

getShort_Object: anObject1
	"answer the result of calling the receiver's public getShort(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callShortMethod: 'getShort' signature: '(Ljava/lang/Object;)S' withArguments: args.
!

getType_null
	"answer the result of calling the receiver's public getType() Java method"

	^ self callObjectMethod: 'getType' signature: '()Ljava/lang/Class;'.
!

getValueFrom: aJavaObject
	"answer the result of geting the value of the field we stand for from the
	given Java object.
	NB: must be in the same JVM"

	self assert: [self jvm == aJavaObject jvm].

	^ self type
		getValueOfFID: self fieldID
		from: aJavaObject.!

hides: aField
	"answer whether the field we stand for hides aField; note that we do
	NOT consider instance and class-side fields to hide each other, and
	also that we consider the type of the field to be disambiguation too"

	self isStatic = aField isStatic ifFalse: [^ false].
	(self declaredIn isDerivedFrom: aField declaredIn) ifFalse: [^ false].
	(self name = aField name) ifFalse: [^ false].
	(self type = aField type) ifFalse: [^ false].

	^ true.
!

jniSignature
	"answer a JNI-style signature for this member"

	^ self type jniSignature.!

set_Object: anObject1 Object: anObject2
	"invoke the receiver's public set(java.lang.Object, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObject2;
			yourself.

	self callVoidMethod: 'set' signature: '(Ljava/lang/Object;Ljava/lang/Object;)V' withArguments: args.
!

setBoolean_Object: anObject1 boolean: boolean1
	"invoke the receiver's public setBoolean(java.lang.Object, boolean) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			booleanAt: 2 put: boolean1;
			yourself.

	self callVoidMethod: 'setBoolean' signature: '(Ljava/lang/Object;Z)V' withArguments: args.
!

setByte_Object: anObject1 byte: byte1
	"invoke the receiver's public setByte(java.lang.Object, byte) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			byteAt: 2 put: byte1;
			yourself.

	self callVoidMethod: 'setByte' signature: '(Ljava/lang/Object;B)V' withArguments: args.
!

setChar_Object: anObject1 char: char1
	"invoke the receiver's public setChar(java.lang.Object, char) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			charAt: 2 put: char1;
			yourself.

	self callVoidMethod: 'setChar' signature: '(Ljava/lang/Object;C)V' withArguments: args.
!

setDouble_Object: anObject1 double: double1
	"invoke the receiver's public setDouble(java.lang.Object, double) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			doubleAt: 2 put: double1;
			yourself.

	self callVoidMethod: 'setDouble' signature: '(Ljava/lang/Object;D)V' withArguments: args.
!

setFloat_Object: anObject1 float: float1
	"invoke the receiver's public setFloat(java.lang.Object, float) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			floatAt: 2 put: float1;
			yourself.

	self callVoidMethod: 'setFloat' signature: '(Ljava/lang/Object;F)V' withArguments: args.
!

setInt_Object: anObject1 int: int1
	"invoke the receiver's public setInt(java.lang.Object, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			intAt: 2 put: int1;
			yourself.

	self callVoidMethod: 'setInt' signature: '(Ljava/lang/Object;I)V' withArguments: args.
!

setLong_Object: anObject1 long: long1
	"invoke the receiver's public setLong(java.lang.Object, long) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			longAt: 2 put: long1;
			yourself.

	self callVoidMethod: 'setLong' signature: '(Ljava/lang/Object;J)V' withArguments: args.
!

setShort_Object: anObject1 short: short1
	"invoke the receiver's public setShort(java.lang.Object, short) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			shortAt: 2 put: short1;
			yourself.

	self callVoidMethod: 'setShort' signature: '(Ljava/lang/Object;S)V' withArguments: args.
!

setValueIn: aJavaObject to: anObject
	"set the value of the field we stand for in the given Java object
	to anObject.
	NB: must be in the same JVM"

	self assert: [self jvm == aJavaObject jvm].

	^ self type
		setValueOfFID: self fieldID
		in: aJavaObject
		to: anObject.!

type
	"answer the class static corresponding to our getType() Java method"

	typeCache isNil ifTrue: [typeCache := self getType_null classStatic].

	^ typeCache.! !
!JavaLangReflectField categoriesFor: #fieldID!accessing!public! !
!JavaLangReflectField categoriesFor: #get_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getBoolean_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getByte_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getChar_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getDouble_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getFloat_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getInt_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getLong_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getShort_Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getType_null!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #getValueFrom:!public!reflection! !
!JavaLangReflectField categoriesFor: #hides:!public!testing! !
!JavaLangReflectField categoriesFor: #jniSignature!accessing!public! !
!JavaLangReflectField categoriesFor: #set_Object:Object:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setBoolean_Object:boolean:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setByte_Object:byte:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setChar_Object:char:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setDouble_Object:double:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setFloat_Object:float:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setInt_Object:int:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setLong_Object:long:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setShort_Object:short:!Java-methods!Java-public!public! !
!JavaLangReflectField categoriesFor: #setValueIn:to:!public!reflection! !
!JavaLangReflectField categoriesFor: #type!public!reflection! !

!JavaLangReflectField class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_Object:
		#getBoolean_Object:
		#getByte_Object:
		#getChar_Object:
		#getDouble_Object:
		#getFloat_Object:
		#getInt_Object:
		#getLong_Object:
		#getShort_Object:
		#getType_null
		#set_Object:Object:
		#setBoolean_Object:boolean:
		#setByte_Object:byte:
		#setChar_Object:char:
		#setDouble_Object:double:
		#setFloat_Object:float:
		#setInt_Object:int:
		#setLong_Object:long:
		#setShort_Object:short:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.reflect.Field'.
! !
!JavaLangReflectField class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectField class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectField class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectField class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectField class categoriesFor: #javaClassName!accessing!constants!public! !

JavaLangReflectMethod guid: (GUID fromString: '{88A2D16B-BDA8-4CF7-80CF-9A9459943B7B}')!
JavaLangReflectMethod comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.reflect.Method.

I''ve included a few wrappers (#isBridge_null, #isVarargs_null, #isSynthetic_null) that are only defined in JDK1.5.  The ''raw'' wrappers will fail with a no-such-method error if we are running on an earlier JVM.  The ''cooked'' wrappers (#isBridge, #isVarargs, #isSynthetic) trap the error and answer false.'!
!JavaLangReflectMethod categoriesForClass!Unclassified! !
!JavaLangReflectMethod methodsFor!

argumentCount
	"answer how many arguments this method takes"

	^ self parameterTypes size.!

exceptionTypes
	"answer an Array of class statics corresponding to the resut of the getParameterTypes() Java method"

	"this is not often called so we don't cache the result"
	^ self getExceptionTypes_null collect: [:each | each classStatic].!

getExceptionTypes_null
	"answer the result of calling the receiver's public getExceptionTypes() Java method"

	^ self callObjectMethod: 'getExceptionTypes' signature: '()[Ljava/lang/Class;'.
!

getParameterTypes_null
	"answer the result of calling the receiver's public getParameterTypes() Java method"

	^ self callObjectMethod: 'getParameterTypes' signature: '()[Ljava/lang/Class;'.
!

getReturnType_null
	"answer the result of calling the receiver's public getReturnType() Java method"

	^ self callObjectMethod: 'getReturnType' signature: '()Ljava/lang/Class;'.
!

hasSameSignatureAs: aMethod
	"answer whether the method we stand takes the same list of parameters, and return
	the same value, as aMethod"

	| ourParams itsParams |

	"we can use == to compare types, thus saving trips into the JVM"

	self type == aMethod type ifFalse: [^ false].

	ourParams := self parameterTypes.
	itsParams := aMethod parameterTypes.
	(ourParams size = itsParams size) ifFalse: [^ false].
	ourParams
		with: itsParams
		do: [:ours :its | (ours == its) ifFalse: [^ false]].

	^ true.
!

invoke_Object: anObject1 ObjectArray: anObjects1
	"answer the result of calling the receiver's public invoke(java.lang.Object, java.lang.Object[]) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObjects1;
			yourself.

	^ self callObjectMethod: 'invoke' signature: '(Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

isBridge
	"answer whether we are one of the new Java5 'bridge' methods (which I loath)"

	"the method doesn't exist pre-Java 5"
	^ self jvm hasJava5Extensions and: [self isBridge_null].
!

isBridge_null
	"answer the result of calling the receiver's public isBridge() Java method"

	^ self callBooleanMethod: 'isBridge'.
!

isStaticOrAbstract
	"answer whether the receiver represents a static or abstract member"

	^ self modifiers anyMask: ##(MODIFIER_STATIC bitOr: MODIFIER_ABSTRACT).!

isVarArgs
	"answer whether we are one of the new Java5 'varargs' methods.
	(which /actually/ just take an array as our last parameter, but we won't harp
	on the hacky way that Sun do things..."

	"the method doesn't exist pre-Java 5"
	^ self jvm hasJava5Extensions and: [self isVarArgs_null].
!

isVarArgs_null
	"answer the result of calling the receiver's public isVarArgs() Java method"

	^ self callBooleanMethod: 'isVarArgs'.
!

isVoidReturn
	"answer whether the method we stand for returns void"

	^ self type isVoid.!

jniSignature
	"answer a JNI-style signature for this member"

	| str |

	str := String writeStream.
	str nextPutAll: '('.
	self parameterTypes do: [:each | str nextPutAll: each jniSignature].
	str nextPutAll: ')'.
	str nextPutAll: self type jniSignature.

	^ str contents.!

methodID
	"answer a JNIMethodID for this field"

	^self jniEnv
		FromReflectedMethod_method: jniObject
		onException: [:jex | self jvm throwJavaException: jex].

!

overrides: aMethod
	"answer whether the method we stand for overrides aMethod"

	self isStaticOrPrivate ifTrue: [^ false].
	aMethod isStaticOrPrivate ifTrue: [^ false].
	(self declaredIn isDerivedFrom: aMethod declaredIn) ifFalse: [^ false].
	(self name = aMethod name) ifFalse: [^ false].
	(self hasSameSignatureAs: aMethod) ifFalse: [^ false].

	^ true.
!

overridesOrDuplicates: aMethod
	"answer whether the method we stand for overrides aMethod.  This is different from
	#overrides: in that two identical methods defined in unrelated interfaces are considered
	to duplicate each other"

	| ourClass theirClass |

	self isStaticOrPrivate ifTrue: [^ false].
	aMethod isStaticOrPrivate ifTrue: [^ false].

	ourClass := self declaredIn.
	theirClass := aMethod declaredIn.
	((ourClass isInterface and: [theirClass isInterface]) or: [ourClass isDerivedFrom: theirClass]) ifFalse: [^ false].

	(self name = aMethod name) ifFalse: [^ false].
	(self hasSameSignatureAs: aMethod) ifFalse: [^ false].

	^ true.
!

parameterTypes
	"answer an Array of class statics corresponding to the resut of the getParameterTypes() Java method"

	parameterTypesCache isNil ifTrue:
		[parameterTypesCache := self getParameterTypes_null collect: [:each | each classStatic]].

	^ parameterTypesCache.!

type
	"answer the class static corresponding to our getReturnType() Java method"

	typeCache isNil ifTrue: [typeCache := self getReturnType_null classStatic].

	^ typeCache.! !
!JavaLangReflectMethod categoriesFor: #argumentCount!accessing!public! !
!JavaLangReflectMethod categoriesFor: #exceptionTypes!public!reflection! !
!JavaLangReflectMethod categoriesFor: #getExceptionTypes_null!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #getParameterTypes_null!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #getReturnType_null!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #hasSameSignatureAs:!public!testing! !
!JavaLangReflectMethod categoriesFor: #invoke_Object:ObjectArray:!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #isBridge!public!testing! !
!JavaLangReflectMethod categoriesFor: #isBridge_null!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #isStaticOrAbstract!public!testing! !
!JavaLangReflectMethod categoriesFor: #isVarArgs!public!testing! !
!JavaLangReflectMethod categoriesFor: #isVarArgs_null!Java-methods!Java-public!public! !
!JavaLangReflectMethod categoriesFor: #isVoidReturn!public!testing! !
!JavaLangReflectMethod categoriesFor: #jniSignature!accessing!public! !
!JavaLangReflectMethod categoriesFor: #methodID!accessing!public! !
!JavaLangReflectMethod categoriesFor: #overrides:!public!testing! !
!JavaLangReflectMethod categoriesFor: #overridesOrDuplicates:!public!testing! !
!JavaLangReflectMethod categoriesFor: #parameterTypes!public!reflection! !
!JavaLangReflectMethod categoriesFor: #type!public!reflection! !

!JavaLangReflectMethod class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#getExceptionTypes_null
		#getParameterTypes_null
		#getReturnType_null
		#invoke_Object:ObjectArray:

		#isBridge_null
		#isVarargs_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.reflect.Method'.
! !
!JavaLangReflectMethod class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectMethod class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectMethod class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectMethod class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaLangReflectMethod class categoriesFor: #javaClassName!accessing!constants!public! !

JavaClassStatic guid: (GUID fromString: '{4FFA9A0A-170F-4C23-86C7-00F67AE09669}')!
JavaClassStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Specialised form of class static that is used to wrap classes that have instances.  This adds the machinery needed to allow class statics to fullfill their responsibility of wrapping Java object references in instances of the correct proxy classes (our instanceClass).

Since these stand for instantiable Java classes, they add the ability to act as factories for Java objects of the corresponding type.  E.g. see #new.

Note also that instances can be used to to filter exceptions in the same way as the JavaException or JavaLangThrowable classes (see either of those classes'' comments).'!
!JavaClassStatic categoriesForClass!Unclassified! !
!JavaClassStatic methodsFor!

, anExceptionFilter
	"answer a new ExceptionSet containing the receiver and anExceptionFilter (typically a Class
	under Exception"

	^ ExceptionSet with: self with: anExceptionFilter.!

basicWrapJNIObject: aJNIObject
	"private -- answer a new instance of our instance class which wraps aJNIObject and which is not
	initially canonical (though it may be made so later).  Note we assume that aJNIObject is not Null"

	^ instanceClass
		jniObject: aJNIObject
		static: self.!

callConstructor
	"answer a JavaObject wrapping a newly created instance of the Java class we represent"

	^ self callConstructorMID: (self findConstructor: '()V').!

callConstructorMID: aMethodID
	"answer a JavaObject wrapping a newly created instance of the Java class we represent"

	^ self callConstructorMID: aMethodID withArguments: nil.!

callConstructorMID: aMethodID withArguments: aJNIValueArray
	"answer a JavaObject wrapping a newly created instance of the Java class we represent"

	| answer |

	answer := self jniEnv
			NewObjectA_class: self jniObject
			methodID: aMethodID
			args: aJNIValueArray
			onException: [:jex | jvm throwJavaException: jex].

	"since we know the precise type of the generated object, we can wrap it directly here,
	which is faster"
	^ self wrapJNIObject: answer.
!

callConstructorSignature: aJNISignatureString withArguments: aJNIValueArray
	"answer a JavaObject wrapping a newly created instance of the Java class we represent"

	^ self callConstructorMID: (self findConstructor: aJNISignatureString) withArguments: aJNIValueArray.
!

canonicalInstances
	"answer a list of all our canonical instances.
	NB: this isn't really threadsafe"

	^ registry 
		ifNil: [#()]
		ifNotNil: [:it | it contents].!

couldBeSubstituted
	"answer true if we represent a class for which the JVM rules do not allow any other
	to be substituted at runtime (e.g. final classes)"

	^ classObject isFinal not.!

ensureRegistryInstalled
	"private -- ensure we have an Object registry for holding canonical instances"

	self sharedMutex critical:
		[registry isNil ifTrue: [registry := self makeRegistry]].
!

findConstructor: aString
	"answer the JavaMethodID corresponding to our Java object's constructor with
	signature defined by anotherString"

	^ self jniEnv
		GetMethodID_class: self jniObject
		name: '<init>'
		sig: aString
		onException: [:ex | jvm throwJavaException: ex].

!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the field of this type defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field.  We are overriden in
	the primitive classes)"

	"overridden because we may be able to use the optimised version"
	^ aJavaObject
		getObjectFID: aJNIFieldID
		wrapperFactory: (self couldBeSubstituted ifTrue: [self jvm] ifFalse: [self]).!

handles: anException
	"answer whether the receiver matches anException.
	This is provided so that instances of JavaException can be caught
	with exception on: blocks using *INSTANCES* of javaLangClass"

	^ (anException isKindOf: JavaException)
		and: [(anException tag isKindOf: JavaLangThrowable)
			and: [classObject isInstance: anException tag]].!

hasAnyCanonicalInstances
	"answer whether any instances are currently canonical.
	(Also see #hasCanonicalInstances)"

	^ registry
		ifNil: [false]
		ifNotNil: [:it | it notEmpty].
!

hasCanonicalInstances
	"answer whether *all* instances are automatically made canonical.
	(Also see #hasAnyCanonicalInstances)"

	^ allInstancesAreCanonical.

!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later).
	Override in subclasses, to force this"

	"default is just to ask our instance class what it thinks"
	^ instanceClass hasCanonicalInstancesByDefault.!

haveCanonicalInstances
	"set that all subsequently created instances will be made canonical.
	There is intentionally no way to reverse the effect of this request"

	allInstancesAreCanonical ifFalse:
		[self ensureRegistryInstalled.
		allInstancesAreCanonical := true].!

initializeFrom: aJavaStatic
	"private -- copy as much data as possible from aJavaStatic.
	This is called during bootstrapping and conversion of clases to ghosts, so aJavaStatic
	will typically be of a *superclass* of the receiver.
	Should be overriden by subclasses that may need to copy extra data"

	super initializeFrom: aJavaStatic.

	"this is strictly unecessary since we should not reach here before we get
	#notifyRegistered (which is where we initialise this data), but just to be
	clear and clean..."
	registry := aJavaStatic objectRegistry.
	allInstancesAreCanonical := aJavaStatic hasCanonicalInstances.!

isCanonical: anInstance
	"answer whether the given instance is the canonical instance"

	^ registry
		ifNil: [false]
		ifNotNil: [:it | it includes: anInstance].!

isSubclassOf: aJavaStatic
	"answer whether this Java class includes the Java class defined by aJavaStatic in
	its inheritance chain.
	Note: this does *not* include interfaces"

	^ javaSuperclass isNil
		ifTrue: [false]
		ifFalse: [javaSuperclass == aJavaStatic or: [javaSuperclass isSubclassOf: aJavaStatic]].
!

makeCanonical: anInstance
	"make the given instance be the canonical instance (evicting
	any previous encumbent)"

	"install a registry if we don't already have one but don't set the
	allInstancesAreCanonical flag, since we don't want to make *all*
	instances canonical"
	self ensureRegistryInstalled.

	"if the registry already contained a different JavaInstance that
	refers to the same Java object as anInstance, then it'll be
	replaced"
	registry add: anInstance.!

makeNotCanonical: anInstance
	"make the given instance not be the canonical instance.
	NB: should normally only be used for instances that are about
	to be #free-d explicitly (very much the exepctional case)"

	registry ifNotNil: [:it | it purge: anInstance].!

makeRegistry
	"private -- answer  a new JavaObjectRegistry"

	^ self class registryClass
		new: self class registryCapacity
		withJVM: self jvm.!

new
	"answer the result of trying to call this classes default contructor"

	^ self callConstructor.!

notifyRegistered
	"this is called by the class registry once we have been fully initialised.
	Normally that means that the instance class and static class are both
	correct and stable (will not change unless you, the programmer, manually
	create and register a new wrapper class that is more appropriate for the
	Java class we represent).  If ghost classes are in use then we are fully
	populated with ghost methods by the time this is called too"

	super notifyRegistered.

	"we postpone this to here so that we can be sure that the instance
	class is correct"
	allInstancesAreCanonical := false.
	self hasCanonicalInstancesByDefault
		ifTrue: [self haveCanonicalInstances].
!

objectRegistry
	"private -- answer the registry of canoncial instances that we hold.
	Will be null if (as is typical) we do not have canoncial instances"

	^ registry.!

wrapJNIObject: aJNIObject
	"answer a new instance of our instance class which wraps aJNIObject.  If there
	was a previously existing instance that represents the same Java object, and which has
	been made canonical, then free the JNI object and answer the canonical instance"

	^ aJNIObject isNull
		ifTrue: [nil]
		ifFalse: [registry isNil
			ifTrue: [self basicWrapJNIObject: aJNIObject]
			ifFalse: [allInstancesAreCanonical
				ifTrue: [registry findOrRelease: aJNIObject ifAbsentPut: [self basicWrapJNIObject: aJNIObject]]
				ifFalse: [registry findOrRelease: aJNIObject ifAbsent: [self basicWrapJNIObject: aJNIObject]]]].
!

writeDescriptionOn: aStream
	"write a short description of our methods on aStream"

	super writeDescriptionOn: aStream.

	registry isNil ifTrue: [^ self].

	aStream
		cr;
		nextPutAll: (self hasCanonicalInstances ifTrue: ['Canonical ('] ifFalse: ['Partly canonical (']);
		display: registry tableSize;
		nextPutAll: ' registered)'.!

writeStatusOn: aStream
	"write a formatted description of our status on aStream"

	registry isNil ifTrue: [^ self].

	aStream
		nextPutAll: (self hasCanonicalInstances ifTrue: ['All'] ifFalse: ['Some']);
		nextPutAll: ' instances are canonical (';
		display: registry tableSize;
		nextPutAll: ' currently registered)';
		cr; cr.
! !
!JavaClassStatic categoriesFor: #,!exception filtering!public! !
!JavaClassStatic categoriesFor: #basicWrapJNIObject:!instance creation!managed objects!private! !
!JavaClassStatic categoriesFor: #callConstructor!instance creation!Java constructors!public! !
!JavaClassStatic categoriesFor: #callConstructorMID:!instance creation!Java constructors!public! !
!JavaClassStatic categoriesFor: #callConstructorMID:withArguments:!instance creation!Java constructors!public! !
!JavaClassStatic categoriesFor: #callConstructorSignature:withArguments:!instance creation!Java constructors!public! !
!JavaClassStatic categoriesFor: #canonicalInstances!canonical instances!modes!public! !
!JavaClassStatic categoriesFor: #couldBeSubstituted!public!testing! !
!JavaClassStatic categoriesFor: #ensureRegistryInstalled!canonical instances!private! !
!JavaClassStatic categoriesFor: #findConstructor:!instance creation!Java constructors!public! !
!JavaClassStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaClassStatic categoriesFor: #handles:!exception filtering!public! !
!JavaClassStatic categoriesFor: #hasAnyCanonicalInstances!canonical instances!public!testing! !
!JavaClassStatic categoriesFor: #hasCanonicalInstances!canonical instances!public!testing! !
!JavaClassStatic categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!public!testing! !
!JavaClassStatic categoriesFor: #haveCanonicalInstances!canonical instances!modes!public! !
!JavaClassStatic categoriesFor: #initializeFrom:!initializing!private! !
!JavaClassStatic categoriesFor: #isCanonical:!canonical instances!public! !
!JavaClassStatic categoriesFor: #isSubclassOf:!public!testing! !
!JavaClassStatic categoriesFor: #makeCanonical:!canonical instances!public! !
!JavaClassStatic categoriesFor: #makeNotCanonical:!canonical instances!public! !
!JavaClassStatic categoriesFor: #makeRegistry!canonical instances!private! !
!JavaClassStatic categoriesFor: #new!instance creation!public! !
!JavaClassStatic categoriesFor: #notifyRegistered!canonical instances!initializing!public! !
!JavaClassStatic categoriesFor: #objectRegistry!helpers!private! !
!JavaClassStatic categoriesFor: #wrapJNIObject:!canonical instances!instance creation!managed objects!public! !
!JavaClassStatic categoriesFor: #writeDescriptionOn:!displaying!public! !
!JavaClassStatic categoriesFor: #writeStatusOn:!displaying!public! !

JavaClassStatic methodProtocol: #javaWrapperFactory attributes: #() selectors: #(#jvm #wrapJNIObject:)!

!JavaClassStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaObjectArray.!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	^ JavaLangObject.! !
!JavaClassStatic class categoriesFor: #arrayClass!constants!public! !
!JavaClassStatic class categoriesFor: #defaultInstanceClass!constants!public! !

JavaInterfaceStatic guid: (GUID fromString: '{9005E8C6-0FCD-4992-9000-84B61275B591}')!
JavaInterfaceStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Specialised form of class static that is used to wrap interface classes.  This is split out from JavaClassStatic mainly just to allow us to substitute polymorphism for runtime tests (e.g. #isInterface).
'!
!JavaInterfaceStatic categoriesForClass!Unclassified! !
!JavaInterfaceStatic methodsFor!

couldBeSubstituted
	"answer true if we represent a class for which the JVM rules do not allow any other
	to be substituted at runtime (e.g. final classes)"

	^ true.!

isInterface
	"answer whether we stand for an Java interface 'class'"

	^ true.! !
!JavaInterfaceStatic categoriesFor: #couldBeSubstituted!public!testing! !
!JavaInterfaceStatic categoriesFor: #isInterface!Java methods!public!testing! !

!JavaInterfaceStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaObjectArray.!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	^ JavaInterfaceInstance.
! !
!JavaInterfaceStatic class categoriesFor: #arrayClass!constants!public! !
!JavaInterfaceStatic class categoriesFor: #defaultInstanceClass!constants!public! !

JavaPrimitiveStatic guid: (GUID fromString: '{1FDAA83D-E816-403C-B70D-CE48AF422C32}')!
JavaPrimitiveStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Java''s primitive types are not classes, but there are instances of java.lang.Class corresponding to each of them.  So we have corresponding specialised class statics too.  They don''t actually add a lot of value except that by using these things we can use polymorphism for some common tests, instead of having to go back to the Java runtime for information.'!
!JavaPrimitiveStatic categoriesForClass!Unclassified! !
!JavaPrimitiveStatic methodsFor!

couldBeSubstituted
	"answer true if we represent a class for which the JVM rules do not allow any other
	to be substituted at runtime (e.g. final classes)"

	^ false.!

isPrimitive
	"answer whether we stand for an Java primitive class (int, float, etc)"

	^ true.!

jniSignature
	"answer a JNI-style signature for this class"

	^ self class jniSignature.!

name
	"answer the name of the member as a String"

	^ self class javaTypeName.
!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	self class wrapperJavaClassName.! !
!JavaPrimitiveStatic categoriesFor: #couldBeSubstituted!public!testing! !
!JavaPrimitiveStatic categoriesFor: #isPrimitive!Java methods!public!reflection!testing! !
!JavaPrimitiveStatic categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveStatic categoriesFor: #name!accessing!public! !
!JavaPrimitiveStatic categoriesFor: #wrapperJavaClassName!constants!public! !

!JavaPrimitiveStatic class methodsFor!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	^ JavaPrimitiveInstance.
!

isWrapperClass
	"answer whether this class is intended to correspond to some particular Java class"

	^ self class includesSelector: #javaTypeName.!

registerWith: aClassFinder
	"this is called as JVM is initialised, or with a SupplementaryClassloader, and gives
	us the chance (should we wish to accept it) to register ourself with it as a wrapper
	class"

	"we cannot treat JVMs and local classpath entries eqivalently, so we don't try..."
	self shouldNotImplement.!

registerWithJVM: aJVM
	"this is called as JVM is initialised and gives us the chance (should we wish to accept it)
	to register ourself with it as a wrapper class.
	NB: this is only called if #shouldRegisterWithJVM answers true"

	|  wrapperClass classObject classStatic |

	"this is all just a little too wierd.
	The twisted logic is ultimately because JNI findClass() won't work with any form of the names of
	the primitive types, so we have to go via the Java primitive wrapper classes (java.lang.Integer,
	java.lang.Boolean, etc) .  We can /almost/ do it via the Array class names, '[I', '[F', and so on
	(which do work), but that won't do for void since void[] is not a legal class.
	Grrr..."

	"start from the wrapper class ('wrapper' in this case meaning the java.lang.Integer or similar
	Java class that 'wraps' the primiive type)"
	wrapperClass := aJVM findClass: self wrapperJavaClassName.

	"the primitive type is the value of the wrapper class's static TYPE field; it'll be registered
	as a side effect of being returned by #getObjectField:signature:"
	classObject := wrapperClass getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
	self assert: [classObject notNil].
	classStatic := classObject classStatic.

	"the class registry doesn't automatically ensure that JavaLangClass objects corresponding to
	primitive types are created with class statics that are JavaPrimitiveStatics (since there's no point
	in doing a relatively slow test for every class loaded when there is a fixed, small number of primitive
	types which can be handled specially during startup), so we have to make the correction here"
	classStatic
		changeInstanceClassTo: JavaPrimitiveInstance;
		changeStaticClassTo: self.

	"and finally we can add it to the class index"
	aJVM classIndex addClass: classStatic.
!

registerWithSupplementaryClassloader: aSupplementaryClassloader
	"this is called with a SupplementaryClassloader, and gives us the chance
	(should we wish to accept it) to register ourself with it as a wrapper class.
	NB: this is only called if #shouldRegisterWithSupplementaryClassloader
	answers true"

	| classStatic |

	"we /must/ be known to the main JVM already, so find our class static there"
	classStatic := aSupplementaryClassloader jvm findClass: self wrapperJavaClassName.

	"and add it to the classloader's class index"
	aSupplementaryClassloader classIndex addClass: classStatic.

!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	self subclassResponsibility.! !
!JavaPrimitiveStatic class categoriesFor: #defaultInstanceClass!constants!public! !
!JavaPrimitiveStatic class categoriesFor: #isWrapperClass!public!testing! !
!JavaPrimitiveStatic class categoriesFor: #registerWith:!public!registering wrapper classes! !
!JavaPrimitiveStatic class categoriesFor: #registerWithJVM:!public!registering wrapper classes! !
!JavaPrimitiveStatic class categoriesFor: #registerWithSupplementaryClassloader:!public!registering wrapper classes! !
!JavaPrimitiveStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

StaticJavaLangObject guid: (GUID fromString: '{4C42C4A2-78D7-4726-AA57-CF61077A2715}')!
StaticJavaLangObject comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Wrapper for the class-side java.lang.Object, and other Java classes that are not given more specialised wrappers.

It is not necessary to derive wrappers for subclasses of java.lang.Object from this class, but it is normally tidier to try to keep the Java class hierarchy and the wrapper hierarchy recognisably similar.'!
!StaticJavaLangObject categoriesForClass!Unclassified! !
!StaticJavaLangObject methodsFor!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
! !
!StaticJavaLangObject categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !

!StaticJavaLangObject class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Object'.
! !
!StaticJavaLangObject class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangObject class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangObject class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangObject class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangObject class categoriesFor: #javaClassName!accessing!constants!public! !

JavaArrayClassStatic guid: (GUID fromString: '{63245631-0BF2-488A-BC97-915387CF277D}')!
JavaArrayClassStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Specialised class static used as a root class for all Java array classes.  Specialises the inherited behaviour a bit, but the main added value is that it knows how to act as a Smalltalk-style array factory.'!
!JavaArrayClassStatic categoriesForClass!Unclassified! !
!JavaArrayClassStatic methodsFor!

couldBeSubstituted
	"answer true if we represent a class for which the JVM rules do not allow any other
	to be substituted at runtime (e.g. final classes)"

	"this is the way that the JVM treats arrays -- formally odd, but..."
	^ self elementClass couldBeSubstituted.!

elementClass
	"answer the class static for the type of our elements"

	elementClassCache isNil ifTrue: [elementClassCache := classObject getComponentType_null classStatic].

	^ elementClassCache.!

instanceElementWrapper
	"answer the JVM or class static to be used by instances to wrap their elements as they are
	extracted"

	instanceElementWrapperCache isNil ifTrue:
		[instanceElementWrapperCache := self elementClass couldBeSubstituted
								ifTrue: [self jvm]
								ifFalse: [self elementClass]].

	^ instanceElementWrapperCache.!

isArray
	"answer whether we stand for an array class"

	^ true.!

javaSuperclass: aJavaStatic
	"private -- set the record of our Java superclass.
	Overridden not to save the supeclasses instanceClass"

	javaSuperclass := aJavaStatic.
!

jniSignature
	"answer a JNI-style signature for this class.
	(note that this is a fairly expensive operation)"

	^ '[' , self elementClass jniSignature.!

name
	"answer a name to know ourselves by"

	^ self elementClass name , '[]'.!

new: size
	"answer a JavaArrayObject wrapping a newly created instance of the Array class we represent"

	^ self elementClass newArray: size.!

with: anObject
	"answer a JavaArrayObject of size 1, wrapping a newly created instance of the Array class we represent
	and holding anObject"

	^ self elementClass newArrayWith: anObject.!

with: anObject1 with: anObject2
	"answer a JavaArrayObject of size 2, wrapping a newly created instance of the Array class we represent
	and holding the given objects"

	^ self elementClass newArrayWith: anObject1 with: anObject2.!

with: anObject1 with: anObject2 with: anObject3
	"answer a JavaArrayObject of size 3, wrapping a newly created instance of the Array class we represent
	and holding the given objects"

	^ self elementClass newArrayWith: anObject1 with: anObject2 with: anObject3.!

withAll: aList
	"answer a JavaArrayObject holding the given objects"

	^ self elementClass newArrayWithAll: aList.! !
!JavaArrayClassStatic categoriesFor: #couldBeSubstituted!public!testing! !
!JavaArrayClassStatic categoriesFor: #elementClass!accessing!public! !
!JavaArrayClassStatic categoriesFor: #instanceElementWrapper!accessing!public! !
!JavaArrayClassStatic categoriesFor: #isArray!public!testing! !
!JavaArrayClassStatic categoriesFor: #javaSuperclass:!initializing!private! !
!JavaArrayClassStatic categoriesFor: #jniSignature!accessing!public! !
!JavaArrayClassStatic categoriesFor: #name!accessing!public! !
!JavaArrayClassStatic categoriesFor: #new:!instance creation!public! !
!JavaArrayClassStatic categoriesFor: #with:!instance creation!public! !
!JavaArrayClassStatic categoriesFor: #with:with:!instance creation!public! !
!JavaArrayClassStatic categoriesFor: #with:with:with:!instance creation!public! !
!JavaArrayClassStatic categoriesFor: #withAll:!instance creation!public! !

!JavaArrayClassStatic class methodsFor!

defaultInstanceClass
	"answer the Smalltalk class to use by default for objects representing instances of our Java class"

	^ JavaObjectArray.
!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #().
! !
!JavaArrayClassStatic class categoriesFor: #defaultInstanceClass!constants!public! !
!JavaArrayClassStatic class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaArrayClassStatic class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaArrayClassStatic class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaArrayClassStatic class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !

StaticJavaLangClass guid: (GUID fromString: '{26B652D6-A925-4712-8DFD-F24B8400CA34}')!
StaticJavaLangClass comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Class and provide access to its class-side methods.'!
!StaticJavaLangClass categoriesForClass!Unclassified! !
!StaticJavaLangClass methodsFor!

basicWrapJNIObject: aJNIClass
	"private -- answer a new instance of our instance class which wraps aJNIClass and which is not
	initially canonical (though it will be made so later).  Note we assume that aJNIClass is not Null.
	Overriden superclass def to create/assign a new class static too"

	| newClassObject newStatic |

	"create a new class static and class object.
	NB: the order here is subtle -- we cannot use the aJNIClass ref at any time
	after it has been wrapped since that might have invalidated the reference as
	a side-effect (of converting it to a global)"
	#CUtodo.  "remove this fragility"
	newStatic := self classStaticFor: aJNIClass.
	newClassObject := super basicWrapJNIObject: aJNIClass.

	"tie them together"
	newStatic classObject: newClassObject.
	newClassObject classStatic: newStatic.

	^ newClassObject.!

classStaticFor: aJNIClass
	"private -- answer a new class static which is appropriate to aJNIClass.  We assume
	that no class static has yet been registered for aJNIClass"

	| superclassObject superclassStatic classStaticClass |

	"find out what sort of class static to use for the new class.  We use different class statics for
	array clases, normal classes, and interface classes.  For normal classes the class static's
	class is normally the same as was used for its superclass (JavaLangObjectStatic for
	java.lang.Object). however this may be overridden if a more specialised class static is registered.
	(Note that we do not end up in this code is a specialisation *is* registered).
	Logically we should also test for primitive types, but they are all dealt with during bootstrapping
	so there is no actual need for a test
	We use the raw JNI interfaces for speed, also assume no exception checking is necessary here"

	"default assumption"
	classStaticClass := StaticJavaLangClass.

	superclassObject := (self jniEnv GetSuperclass_sub: aJNIClass) asJavaObject: jvm.
	superclassObject isNil
		ifFalse: [superclassStatic := superclassObject classStatic.  classStaticClass := superclassStatic class].

	"interfaces have no superclass. NB: java.lang.Object is registered during bootstrap"
	(superclassObject isNil and: [jvm isInterfaceClass: aJNIClass]) ifTrue:
		[classStaticClass := JavaInterfaceStatic].

	"array classes are subclasses of java.lang.Object"
	(superclassStatic notNil and: [jvm isArrayClass: aJNIClass]) ifTrue:
		[classStaticClass := JavaArrayClassStatic].

	^ (classStaticClass newWithJVM: self jvm)
		javaSuperclass: superclassStatic;
		yourself.
!

forName: aJavaString
	"answer the result of calling java.lang.Class.forName()"

	^ self forName_String: aJavaString.
!

forName: aJavaString initialize: aBool classloader: aJavaObject
	"answer the result of calling java.lang.Class.forName(String, boolean, ClassLoader)"

	^ self forName_String: aJavaString boolean: aBool ClassLoader: aJavaObject.!

forName: aJavaString initialize: aBool classLoader: aJavaObject

	"deprecated in favour of a more consistant naming convention"
	Notification deprecated.
	^ self forName: aJavaString initialize: aBool classloader: aJavaObject.!

forName_String: aString1
	"answer the result of calling the receiver's public static forName(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'forName' signature: '(Ljava/lang/String;)Ljava/lang/Class;' withArguments: args.
!

forName_String: aString1 boolean: boolean1 ClassLoader: aClassLoader1
	"answer the result of calling the receiver's public static forName(java.lang.String, boolean, java.lang.ClassLoader) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aString1Ref;
			booleanAt: 2 put: boolean1;
			objectAt: 3 put: aClassLoader1;
			yourself.

	^ self callObjectMethod: 'forName' signature: '(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;' withArguments: args.
!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	Overriden to force this"

	"actually we have a JavaClassRegistry rather than the normal JavaObjectRegistry"
	^ true.!

jvm: aJVM
	"private -- set the JVM that we, and all our 'instances' will use.
	Overriden to pick up the class registry"

	super jvm: aJVM.
	registry := aJVM classRegistry.!

makeNotCanonical: anInstance
	"overriden to avoid accidents"

	self shouldNotImplement.!

makeRegistry
	"private -- answer  a new JavaObjectRegistry.
	Overridden because the JVM creates the system-wide JavaClassRegistry before we are
	created ourself"

	"if we get here then something's gone wrong"
	^ self shouldNotImplement.!

replaceInstanceClass: anOldClass by: aNewClass
	"private -- if we are using anOld class as our instance class, then change to use aNewClass.
	This will be called during bootstrapping if it turns out that we have to change the Smalltalk
	class used to represent java.lang.Class instances.  Unless ghost classes are used (or something
	similar) there will be no need for this (since the class registry bootstrap ensures we start off
	correct).
	The main difference from the superclass implementation is that we must ensure that our instances
	(since we know we have some) are changed too"

	"purposly don't supersend"

	instanceClass == anOldClass ifFalse: [^ self].

	"remember our new instance class"
	instanceClass := aNewClass.

	"convert our existing instances.
	This use of #becomeA: is *probably* not fragile since we only get here if we are
	being converted to use ghost classes, in which case the old and new classes
	must have the same shape"
	(anOldClass allInstances
		select: [:each | each static == self])
			do: [:each | each becomeA: aNewClass].
!

wrapJNIObject: aJNIClass
	"answer a (possibly, but not usually, new) class object which wraps aJNIClass.
	Overriden superclass def to avoid some tests (this is called *A LOT*)"

	^ aJNIClass isNull
		ifTrue: [nil]
		ifFalse: [registry findOrRelease: aJNIClass ifAbsentPut: [self basicWrapJNIObject: aJNIClass]].! !
!StaticJavaLangClass categoriesFor: #basicWrapJNIObject:!instance creation!managed objects!private! !
!StaticJavaLangClass categoriesFor: #classStaticFor:!instance creation!managed objects!private! !
!StaticJavaLangClass categoriesFor: #forName:!instance creation!public! !
!StaticJavaLangClass categoriesFor: #forName:initialize:classloader:!Java-methods!public! !
!StaticJavaLangClass categoriesFor: #forName:initialize:classLoader:!Java-methods!public! !
!StaticJavaLangClass categoriesFor: #forName_String:!**auto generated**!Java-public!Java-static!public! !
!StaticJavaLangClass categoriesFor: #forName_String:boolean:ClassLoader:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangClass categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!StaticJavaLangClass categoriesFor: #jvm:!initializing!private! !
!StaticJavaLangClass categoriesFor: #makeNotCanonical:!canonical instances!public! !
!StaticJavaLangClass categoriesFor: #makeRegistry!managed objects!private! !
!StaticJavaLangClass categoriesFor: #replaceInstanceClass:by:!ghost classes!private! !
!StaticJavaLangClass categoriesFor: #wrapJNIObject:!instance creation!managed objects!public! !

!StaticJavaLangClass class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#forName_String:
		#forName_String:boolean:ClassLoader:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.Class'.
!

registryCapacity
	"answer the initial capacity to give registries used by instances.
	This is not, in fact, ever called, since the JVM creates the class
	registry for JavaLangClass during bootstrap"

	^ 200.!

registryClass
	"answer the kind of Object registry used by instances.
	This is not, in fact, ever called, since the JVM creates the class
	registry for JavaLangClass during bootstrap"


	^ JavaClassRegistry.! !
!StaticJavaLangClass class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClass class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClass class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClass class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClass class categoriesFor: #javaClassName!accessing!constants!public! !
!StaticJavaLangClass class categoriesFor: #registryCapacity!constants!public! !
!StaticJavaLangClass class categoriesFor: #registryClass!constants!public! !

StaticJavaLangClassLoader guid: (GUID fromString: '{B05A008A-23FD-46B9-A568-D9C9326E17B0}')!
StaticJavaLangClassLoader comment: 'Copyright © Chris Uppal, 2001 - 2005.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.ClassLoader and provide access to its class-side methods.'!
!StaticJavaLangClassLoader categoriesForClass!Unclassified! !
!StaticJavaLangClassLoader methodsFor!

getSystemClassLoader_null
	"answer the result of calling the receiver's public static getSystemClassLoader() Java method"

	^ self callObjectMethod: 'getSystemClassLoader' signature: '()Ljava/lang/ClassLoader;'.
!

getSystemResource_String: aString1
	"answer the result of calling the receiver's public static getSystemResource(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getSystemResource' signature: '(Ljava/lang/String;)Ljava/net/URL;' withArguments: args.
!

getSystemResourceAsStream_String: aString1
	"answer the result of calling the receiver's public static getSystemResourceAsStream(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getSystemResourceAsStream' signature: '(Ljava/lang/String;)Ljava/io/InputStream;' withArguments: args.
!

getSystemResources_String: aString1
	"answer the result of calling the receiver's public static getSystemResources(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getSystemResources' signature: '(Ljava/lang/String;)Ljava/util/Enumeration;' withArguments: args.
!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later)"

	"since our instances can hold some local data it best to make them canonical"
	^ true.! !
!StaticJavaLangClassLoader categoriesFor: #getSystemClassLoader_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangClassLoader categoriesFor: #getSystemResource_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangClassLoader categoriesFor: #getSystemResourceAsStream_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangClassLoader categoriesFor: #getSystemResources_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangClassLoader categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public!testing! !

!StaticJavaLangClassLoader class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#getSystemClassLoader_null
		#getSystemResource_String:
		#getSystemResourceAsStream_String:
		#getSystemResources_String:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.ClassLoader'.
! !
!StaticJavaLangClassLoader class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClassLoader class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClassLoader class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClassLoader class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangClassLoader class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangReflectModifier guid: (GUID fromString: '{714CF5C3-7202-4AE7-BF17-639FC901AD21}')!
StaticJavaLangReflectModifier comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.reflect.Modifier and provide access to its class-side methods (actually, most static fields).'!
!StaticJavaLangReflectModifier categoriesForClass!Unclassified! !
!StaticJavaLangReflectModifier methodsFor!

get_ABSTRACT
	"answer the value of the receiver's public static final ABSTRACT Java field"

	^ self getIntField: 'ABSTRACT'.
!

get_FINAL
	"answer the value of the receiver's public static final FINAL Java field"

	^ self getIntField: 'FINAL'.
!

get_INTERFACE
	"answer the value of the receiver's public static final INTERFACE Java field"

	^ self getIntField: 'INTERFACE'.
!

get_NATIVE
	"answer the value of the receiver's public static final NATIVE Java field"

	^ self getIntField: 'NATIVE'.
!

get_PRIVATE
	"answer the value of the receiver's public static final PRIVATE Java field"

	^ self getIntField: 'PRIVATE'.
!

get_PROTECTED
	"answer the value of the receiver's public static final PROTECTED Java field"

	^ self getIntField: 'PROTECTED'.
!

get_PUBLIC
	"answer the value of the receiver's public static final PUBLIC Java field"

	^ self getIntField: 'PUBLIC'.
!

get_STATIC
	"answer the value of the receiver's public static final STATIC Java field"

	^ self getIntField: 'STATIC'.
!

get_STRICT
	"answer the value of the receiver's public static final STRICT Java field"

	^ self getIntField: 'STRICT'.
!

get_SYNCHRONIZED
	"answer the value of the receiver's public static final SYNCHRONIZED Java field"

	^ self getIntField: 'SYNCHRONIZED'.
!

get_TRANSIENT
	"answer the value of the receiver's public static final TRANSIENT Java field"

	^ self getIntField: 'TRANSIENT'.
!

get_VOLATILE
	"answer the value of the receiver's public static final VOLATILE Java field"

	^ self getIntField: 'VOLATILE'.
!

isAbstract_int: int1
	"answer the result of calling the receiver's public static isAbstract(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isAbstract' signature: '(I)Z' withArguments: args.
!

isFinal_int: int1
	"answer the result of calling the receiver's public static isFinal(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isFinal' signature: '(I)Z' withArguments: args.
!

isInterface_int: int1
	"answer the result of calling the receiver's public static isInterface(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isInterface' signature: '(I)Z' withArguments: args.
!

isNative_int: int1
	"answer the result of calling the receiver's public static isNative(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isNative' signature: '(I)Z' withArguments: args.
!

isPrivate_int: int1
	"answer the result of calling the receiver's public static isPrivate(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isPrivate' signature: '(I)Z' withArguments: args.
!

isProtected_int: int1
	"answer the result of calling the receiver's public static isProtected(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isProtected' signature: '(I)Z' withArguments: args.
!

isPublic_int: int1
	"answer the result of calling the receiver's public static isPublic(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isPublic' signature: '(I)Z' withArguments: args.
!

isStatic_int: int1
	"answer the result of calling the receiver's public static isStatic(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isStatic' signature: '(I)Z' withArguments: args.
!

isStrict_int: int1
	"answer the result of calling the receiver's public static isStrict(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isStrict' signature: '(I)Z' withArguments: args.
!

isSynchronized_int: int1
	"answer the result of calling the receiver's public static isSynchronized(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isSynchronized' signature: '(I)Z' withArguments: args.
!

isTransient_int: int1
	"answer the result of calling the receiver's public static isTransient(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isTransient' signature: '(I)Z' withArguments: args.
!

isVolatile_int: int1
	"answer the result of calling the receiver's public static isVolatile(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callBooleanMethod: 'isVolatile' signature: '(I)Z' withArguments: args.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

toString_int: int1
	"answer the result of calling the receiver's public static toString(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(I)Ljava/lang/String;' withArguments: args.
! !
!StaticJavaLangReflectModifier categoriesFor: #get_ABSTRACT!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_FINAL!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_INTERFACE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_NATIVE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_PRIVATE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_PROTECTED!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_PUBLIC!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_STATIC!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_STRICT!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_SYNCHRONIZED!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_TRANSIENT!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #get_VOLATILE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isAbstract_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isFinal_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isInterface_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isNative_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isPrivate_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isProtected_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isPublic_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isStatic_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isStrict_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isSynchronized_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isTransient_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #isVolatile_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectModifier categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangReflectModifier categoriesFor: #toString_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangReflectModifier class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_ABSTRACT
		#get_FINAL
		#get_INTERFACE
		#get_NATIVE
		#get_PRIVATE
		#get_PROTECTED
		#get_PUBLIC
		#get_STATIC
		#get_STRICT
		#get_SYNCHRONIZED
		#get_TRANSIENT
		#get_VOLATILE
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#isAbstract_int:
		#isFinal_int:
		#isInterface_int:
		#isNative_int:
		#isPrivate_int:
		#isProtected_int:
		#isPublic_int:
		#isStatic_int:
		#isStrict_int:
		#isSynchronized_int:
		#isTransient_int:
		#isVolatile_int:
		#toString_int:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.reflect.Modifier'.
! !
!StaticJavaLangReflectModifier class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectModifier class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectModifier class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectModifier class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectModifier class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangReflectProxy guid: (GUID fromString: '{0F2579A8-B183-4B47-9D06-68CD7A03EC37}')!
StaticJavaLangReflectProxy comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.reflect.Proxy and provide access to its class-side methods.'!
!StaticJavaLangReflectProxy categoriesForClass!Unclassified! !
!StaticJavaLangReflectProxy methodsFor!

getInvocationHandler_Object: anObject1
	"answer the result of calling the receiver's public static getInvocationHandler(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'getInvocationHandler' signature: '(Ljava/lang/Object;)Ljava/lang/reflect/InvocationHandler;' withArguments: args.
!

getProxyClass_ClassLoader: aClassLoader1 ClassArray: aClasses1
	"answer the result of calling the receiver's public static getProxyClass(java.lang.ClassLoader, java.lang.Class[]) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: aClassLoader1;
			objectAt: 2 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getProxyClass' signature: '(Ljava/lang/ClassLoader;[Ljava/lang/Class;)Ljava/lang/Class;' withArguments: args.
!

isProxyClass_Class: aClass1
	"answer the result of calling the receiver's public static isProxyClass(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callBooleanMethod: 'isProxyClass' signature: '(Ljava/lang/Class;)Z' withArguments: args.
!

newProxyInstance_ClassLoader: aClassLoader1 ClassArray: aClasses1 InvocationHandler: anInvocationHandler1
	"answer the result of calling the receiver's public static newProxyInstance(java.lang.ClassLoader, java.lang.Class[], java.lang.reflect.InvocationHandler) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: aClassLoader1;
			objectAt: 2 put: aClasses1;
			objectAt: 3 put: anInvocationHandler1;
			yourself.

	^ self callObjectMethod: 'newProxyInstance' signature: '(Ljava/lang/ClassLoader;[Ljava/lang/Class;Ljava/lang/reflect/InvocationHandler;)Ljava/lang/Object;' withArguments: args.
! !
!StaticJavaLangReflectProxy categoriesFor: #getInvocationHandler_Object:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectProxy categoriesFor: #getProxyClass_ClassLoader:ClassArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectProxy categoriesFor: #isProxyClass_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangReflectProxy categoriesFor: #newProxyInstance_ClassLoader:ClassArray:InvocationHandler:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangReflectProxy class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#getInvocationHandler_Object:
		#getProxyClass_ClassLoader:ClassArray:
		#isProxyClass_Class:
		#newProxyInstance_ClassLoader:ClassArray:InvocationHandler:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.reflect.Proxy'.
! !
!StaticJavaLangReflectProxy class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectProxy class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectProxy class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectProxy class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangReflectProxy class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangSystem guid: (GUID fromString: '{E4E2FA2F-E8E2-4FD4-9921-F475D5023EE1}')!
StaticJavaLangSystem comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

This class exists to provide easy access to static methods and fields in the java.lang.System class.  An instance is available directly from the JVM by sending it #javaLangSystem.'!
!StaticJavaLangSystem categoriesForClass!Unclassified! !
!StaticJavaLangSystem methodsFor!

arraycopy_Object: anObject1 int: int1 Object: anObject2 int: int2 int: int3
	"invoke the receiver's public static native arraycopy(java.lang.Object, int, java.lang.Object, int, int) Java method"

	| args |

	args := (JNIValueArray new: 5)
			objectAt: 1 put: anObject1;
			intAt: 2 put: int1;
			objectAt: 3 put: anObject2;
			intAt: 4 put: int2;
			intAt: 5 put: int3;
			yourself.

	self callVoidMethod: 'arraycopy' signature: '(Ljava/lang/Object;ILjava/lang/Object;II)V' withArguments: args.
!

currentTimeMillis
	"answer the result of calling java.lang.System.currentTimeMillis()"

	^ self currentTimeMillis_null.
!

currentTimeMillis_null
	"answer the result of calling the receiver's public static native currentTimeMillis() Java method"

	^ self callLongMethod: 'currentTimeMillis'.
!

err
	"answer the java.lang.System.err object"

	^ self get_err.!

err: aJavaObject
	"call java.lang.System.setErr()"

	self setErr_PrintStream: aJavaObject.!

exit: anInteger
	"call java.lang.System.exit().

	*********  NOTE ****************
	Calling this method, or any of its bretheren (e.g. java.lang.Runtim.exit()) will deadlock
	Dolphin irrecoverably.  I'm not yet sure why this happens, but there is no reason to be sure that
	it'll ever be possible to fix it.
	NB: this applies to Java programs that call java.lang.System.exit() too.
	NB2: as of j2sdk 1.4.1 this crashes Dolphin instead...
	*********  NOTE ****************"

	self exit_int: anInteger.!

exit_int: int1
	"invoke the receiver's public static exit(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'exit' signature: '(I)V' withArguments: args.
!

gc
	"call java.lang.System.gc()"

	self gc_null.!

gc_null
	"invoke the receiver's public static gc() Java method"

	self callVoidMethod: 'gc'.
!

get_err
	"answer the value of the receiver's public static final err Java field"

	^ self getObjectField: 'err' signature: 'Ljava/io/PrintStream;'.
!

get_in
	"answer the value of the receiver's public static final in Java field"

	^ self getObjectField: 'in' signature: 'Ljava/io/InputStream;'.
!

get_out
	"answer the value of the receiver's public static final out Java field"

	^ self getObjectField: 'out' signature: 'Ljava/io/PrintStream;'.
!

getenv_String: aString1
	"answer the result of calling the receiver's public static getenv(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getenv' signature: '(Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

getProperties_null
	"answer the result of calling the receiver's public static getProperties() Java method"

	^ self callObjectMethod: 'getProperties' signature: '()Ljava/util/Properties;'.
!

getProperty_String: aString1
	"answer the result of calling the receiver's public static getProperty(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getProperty' signature: '(Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

getProperty_String: aString1 String: aString2
	"answer the result of calling the receiver's public static getProperty(java.lang.String, java.lang.String) Java method"

	| args aString1Ref aString2Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	aString2Ref := aString2 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aString2Ref;
			yourself.

	^ self callObjectMethod: 'getProperty' signature: '(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

getSecurityManager_null
	"answer the result of calling the receiver's public static getSecurityManager() Java method"

	^ self callObjectMethod: 'getSecurityManager' signature: '()Ljava/lang/SecurityManager;'.
!

identityHashCode: aJavaObject
	"answer the result of calling java.lang.System.identityHashCode()"

	^ self identityHashCode_Object: aJavaObject!

identityHashCode_Object: anObject1
	"answer the result of calling the receiver's public static native identityHashCode(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callIntMethod: 'identityHashCode' signature: '(Ljava/lang/Object;)I' withArguments: args.
!

in
	"answer the java.lang.System.in object"

	^ self get_in.!

in: aJavaObject
	"call java.lang.System.setIn()"

	self setIn_InputStream: aJavaObject.!

load: aString
	"call java.lang.System.load()"

	self load_String: aString.!

load_String: aString1
	"invoke the receiver's public static load(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'load' signature: '(Ljava/lang/String;)V' withArguments: args.
!

loadLibrary: aString
	"call java.lang.System.loadLibrary()"

	self loadLibrary_String: aString.!

loadLibrary_String: aString1
	"invoke the receiver's public static loadLibrary(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'loadLibrary' signature: '(Ljava/lang/String;)V' withArguments: args.
!

mapLibraryName: aString
	"answer the result of calling java.lang.System.mapLibraryName()"

	^ self mapLibraryName_String: aString.!

mapLibraryName_String: aString1
	"answer the result of calling the receiver's public static native mapLibraryName(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'mapLibraryName' signature: '(Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

out
	"answer the java.lang.System.out object"

	^ self get_out.!

out: aJavaObject
	"call java.lang.System.setOut()"

	self setOut_PrintStream: aJavaObject!

properties
	"answer the result of calling java.lang.System.getProperties()"

	^ self getProperties_null.!

properties: aJavaObject
	"call java.lang.System.setProperties()"

	self setProperties_Properties: aJavaObject.!

propertyAt: aJavaString
	"answer the result of calling java.lang.System.getProperty(String)"

	^ self getProperty_String: aJavaString.!

propertyAt: aJavaString put: anotherJavaString
	"answer the result of calliing java.lang.System.setProperty(String, String)"

	^ self setProperty_String: aJavaString String: anotherJavaString!

runFinalization
	"call java.lang.System.runFinalization()"

	self runFinalization_null.
!

runFinalization_null
	"invoke the receiver's public static runFinalization() Java method"

	self callVoidMethod: 'runFinalization'.
!

runFinalizersOnExit_boolean: boolean1
	"invoke the receiver's public static runFinalizersOnExit(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'runFinalizersOnExit' signature: '(Z)V' withArguments: args.
!

securityManager
	"answer the result of calling java.lang.System.securityManager()"

	^ self getSecurityManager_null.!

securityManager: aJavaObject
	"call java.lang.System.setSecurityManager()"

	self setSecurityManager_SecurityManager: aJavaObject.!

setErr_PrintStream: aPrintStream1
	"invoke the receiver's public static setErr(java.io.PrintStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aPrintStream1;
			yourself.

	self callVoidMethod: 'setErr' signature: '(Ljava/io/PrintStream;)V' withArguments: args.
!

setIn_InputStream: anInputStream1
	"invoke the receiver's public static setIn(java.io.InputStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anInputStream1;
			yourself.

	self callVoidMethod: 'setIn' signature: '(Ljava/io/InputStream;)V' withArguments: args.
!

setOut_PrintStream: aPrintStream1
	"invoke the receiver's public static setOut(java.io.PrintStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aPrintStream1;
			yourself.

	self callVoidMethod: 'setOut' signature: '(Ljava/io/PrintStream;)V' withArguments: args.
!

setProperties_Properties: aProperties1
	"invoke the receiver's public static setProperties(java.util.Properties) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aProperties1;
			yourself.

	self callVoidMethod: 'setProperties' signature: '(Ljava/util/Properties;)V' withArguments: args.
!

setProperty_String: aString1 String: aString2
	"answer the result of calling the receiver's public static setProperty(java.lang.String, java.lang.String) Java method"

	| args aString1Ref aString2Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	aString2Ref := aString2 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aString2Ref;
			yourself.

	^ self callObjectMethod: 'setProperty' signature: '(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;' withArguments: args.
!

setSecurityManager_SecurityManager: aSecurityManager1
	"invoke the receiver's public static setSecurityManager(java.lang.SecurityManager) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aSecurityManager1;
			yourself.

	self callVoidMethod: 'setSecurityManager' signature: '(Ljava/lang/SecurityManager;)V' withArguments: args.
! !
!StaticJavaLangSystem categoriesFor: #arraycopy_Object:int:Object:int:int:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #currentTimeMillis!accessing!public! !
!StaticJavaLangSystem categoriesFor: #currentTimeMillis_null!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #err!accessing!public! !
!StaticJavaLangSystem categoriesFor: #err:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #exit:!operations!public! !
!StaticJavaLangSystem categoriesFor: #exit_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #gc!operations!public! !
!StaticJavaLangSystem categoriesFor: #gc_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #get_err!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #get_in!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #get_out!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #getenv_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #getProperties_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #getProperty_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #getProperty_String:String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #getSecurityManager_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #identityHashCode:!helpers!public! !
!StaticJavaLangSystem categoriesFor: #identityHashCode_Object:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #in!accessing!public! !
!StaticJavaLangSystem categoriesFor: #in:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #load:!operations!public! !
!StaticJavaLangSystem categoriesFor: #load_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #loadLibrary:!operations!public! !
!StaticJavaLangSystem categoriesFor: #loadLibrary_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #mapLibraryName:!helpers!public! !
!StaticJavaLangSystem categoriesFor: #mapLibraryName_String:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #out!accessing!public! !
!StaticJavaLangSystem categoriesFor: #out:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #properties!accessing!public! !
!StaticJavaLangSystem categoriesFor: #properties:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #propertyAt:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #propertyAt:put:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #runFinalization!operations!public! !
!StaticJavaLangSystem categoriesFor: #runFinalization_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #runFinalizersOnExit_boolean:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #securityManager!accessing!public! !
!StaticJavaLangSystem categoriesFor: #securityManager:!accessing!public! !
!StaticJavaLangSystem categoriesFor: #setErr_PrintStream:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #setIn_InputStream:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #setOut_PrintStream:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #setProperties_Properties:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #setProperty_String:String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangSystem categoriesFor: #setSecurityManager_SecurityManager:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangSystem class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_err
		#get_in
		#get_out
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#arraycopy_Object:int:Object:int:int:
		#currentTimeMillis_null
		#exit_int:
		#gc_null
		#getenv_String:
		#getProperties_null
		#getProperty_String:
		#getProperty_String:String:
		#getSecurityManager_null
		#identityHashCode_Object:
		#load_String:
		#loadLibrary_String:
		#mapLibraryName_String:
		#runFinalization_null
		#runFinalizersOnExit_boolean:
		#setErr_PrintStream:
		#setIn_InputStream:
		#setOut_PrintStream:
		#setProperties_Properties:
		#setProperty_String:String:
		#setSecurityManager_SecurityManager:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.System'.
! !
!StaticJavaLangSystem class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangSystem class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangSystem class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangSystem class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangSystem class categoriesFor: #javaClassName!accessing!constants!public! !

StaticJavaLangThread guid: (GUID fromString: '{E8BC9D9A-8134-4BDF-8538-513306561CFA}')!
StaticJavaLangThread comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Thread and provide access to its class-side methods and fields.'!
!StaticJavaLangThread categoriesForClass!Unclassified! !
!StaticJavaLangThread methodsFor!

activeCount_null
	"answer the result of calling the receiver's public static activeCount() Java method"

	^ self callIntMethod: 'activeCount'.
!

currentThread_null
	"answer the result of calling the receiver's public static native currentThread() Java method"

	^ self callObjectMethod: 'currentThread' signature: '()Ljava/lang/Thread;'.
!

dumpStack_null
	"invoke the receiver's public static dumpStack() Java method"

	self callVoidMethod: 'dumpStack'.
!

enumerate_ThreadArray: aThreads1
	"answer the result of calling the receiver's public static enumerate(java.lang.Thread[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThreads1;
			yourself.

	^ self callIntMethod: 'enumerate' signature: '([Ljava/lang/Thread;)I' withArguments: args.
!

get_MAX_PRIORITY
	"answer the value of the receiver's public static final MAX_PRIORITY Java field"

	^ self getIntField: 'MAX_PRIORITY'.
!

get_MIN_PRIORITY
	"answer the value of the receiver's public static final MIN_PRIORITY Java field"

	^ self getIntField: 'MIN_PRIORITY'.
!

get_NORM_PRIORITY
	"answer the value of the receiver's public static final NORM_PRIORITY Java field"

	^ self getIntField: 'NORM_PRIORITY'.
!

holdsLock_Object: anObject1
	"answer the result of calling the receiver's public static native holdsLock(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'holdsLock' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

interrupted_null
	"answer the result of calling the receiver's public static interrupted() Java method"

	^ self callBooleanMethod: 'interrupted'.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

new_Runnable: aRunnable1
	"answer the result of calling the receiver's public new(java.lang.Runnable) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aRunnable1;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/Runnable;)V' withArguments: args.
!

new_Runnable: aRunnable1 String: aString1
	"answer the result of calling the receiver's public new(java.lang.Runnable, java.lang.String) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aRunnable1;
			objectAt: 2 put: aString1Ref;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/Runnable;Ljava/lang/String;)V' withArguments: args.
!

new_String: aString1
	"answer the result of calling the receiver's public new(java.lang.String) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/String;)V' withArguments: args.
!

new_ThreadGroup: aThreadGroup1 Runnable: aRunnable1
	"answer the result of calling the receiver's public new(java.lang.ThreadGroup, java.lang.Runnable) Java constructor"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: aThreadGroup1;
			objectAt: 2 put: aRunnable1;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;)V' withArguments: args.
!

new_ThreadGroup: aThreadGroup1 Runnable: aRunnable1 String: aString1
	"answer the result of calling the receiver's public new(java.lang.ThreadGroup, java.lang.Runnable, java.lang.String) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aThreadGroup1;
			objectAt: 2 put: aRunnable1;
			objectAt: 3 put: aString1Ref;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;)V' withArguments: args.
!

new_ThreadGroup: aThreadGroup1 Runnable: aRunnable1 String: aString1 long: long1
	"answer the result of calling the receiver's public new(java.lang.ThreadGroup, java.lang.Runnable, java.lang.String, long) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 4)
			objectAt: 1 put: aThreadGroup1;
			objectAt: 2 put: aRunnable1;
			objectAt: 3 put: aString1Ref;
			longAt: 4 put: long1;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;J)V' withArguments: args.
!

new_ThreadGroup: aThreadGroup1 String: aString1
	"answer the result of calling the receiver's public new(java.lang.ThreadGroup, java.lang.String) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aThreadGroup1;
			objectAt: 2 put: aString1Ref;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/ThreadGroup;Ljava/lang/String;)V' withArguments: args.
!

sleep_long: long1
	"invoke the receiver's public static native sleep(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	self callVoidMethod: 'sleep' signature: '(J)V' withArguments: args.
!

sleep_long: long1 int: int1
	"invoke the receiver's public static sleep(long, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			longAt: 1 put: long1;
			intAt: 2 put: int1;
			yourself.

	self callVoidMethod: 'sleep' signature: '(JI)V' withArguments: args.
!

yield_null
	"invoke the receiver's public static native yield() Java method"

	self callVoidMethod: 'yield'.
! !
!StaticJavaLangThread categoriesFor: #activeCount_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #currentThread_null!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #dumpStack_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #enumerate_ThreadArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #get_MAX_PRIORITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #get_MIN_PRIORITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #get_NORM_PRIORITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #holdsLock_Object:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #interrupted_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_Runnable:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_Runnable:String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_ThreadGroup:Runnable:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_ThreadGroup:Runnable:String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_ThreadGroup:Runnable:String:long:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #new_ThreadGroup:String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThread categoriesFor: #sleep_long:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #sleep_long:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangThread categoriesFor: #yield_null!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !

!StaticJavaLangThread class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
		#new_Runnable:
		#new_Runnable:String:
		#new_String:
		#new_ThreadGroup:Runnable:
		#new_ThreadGroup:Runnable:String:
		#new_ThreadGroup:Runnable:String:long:
		#new_ThreadGroup:String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_PRIORITY
		#get_MIN_PRIORITY
		#get_NORM_PRIORITY
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#activeCount_null
		#currentThread_null
		#dumpStack_null
		#enumerate_ThreadArray:
		#holdsLock_Object:
		#interrupted_null
		#sleep_long:
		#sleep_long:int:
		#yield_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Thread'.
! !
!StaticJavaLangThread class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThread class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThread class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThread class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThread class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangThrowable guid: (GUID fromString: '{468FDA70-B741-4EF2-BEB5-5D200E71FF04}')!
StaticJavaLangThrowable comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Throwable and provide access to its class-side methods.'!
!StaticJavaLangThrowable categoriesForClass!Unclassified! !
!StaticJavaLangThrowable methodsFor!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

new_String: aString1
	"answer the result of calling the receiver's public new(java.lang.String) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/String;)V' withArguments: args.
!

new_String: aString1 Throwable: aThrowable1
	"answer the result of calling the receiver's public new(java.lang.String, java.lang.Throwable) Java constructor"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aThrowable1;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/String;Ljava/lang/Throwable;)V' withArguments: args.
!

new_Throwable: aThrowable1
	"answer the result of calling the receiver's public new(java.lang.Throwable) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThrowable1;
			yourself.

	^ self callConstructorSignature: '(Ljava/lang/Throwable;)V' withArguments: args.
!

signal
	"throw a new instance of this class.  Will generate a Smalltalk exception wrapping
	the new Java object"

	self new_null signal.!

signal: aString
	"throw a new instance of this class with the given message.  The message is held in the Java object.
	Will generate a Smalltalk exception wrapping the new Java object"

	(self new_String: aString) signal.! !
!StaticJavaLangThrowable categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThrowable categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThrowable categoriesFor: #new_String:Throwable:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThrowable categoriesFor: #new_Throwable:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangThrowable categoriesFor: #signal!public!raising! !
!StaticJavaLangThrowable categoriesFor: #signal:!public!raising! !

!StaticJavaLangThrowable class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_null
		#new_String:
		#new_String:Throwable:
		#new_Throwable:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Throwable'.
! !
!StaticJavaLangThrowable class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThrowable class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThrowable class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThrowable class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangThrowable class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaNetURLClassLoader guid: (GUID fromString: '{F2F89E00-56F9-47B4-BF8A-F59ACA4C645C}')!
StaticJavaNetURLClassLoader comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

This wrapper exists mainly because the interface to creating instances is so odd. 

By providing an easier interface which takes an optional parent and optional array of Strings, we can compensate for the awkwardness, and also allow the "local class path" stuff to be more generic.'!
!StaticJavaNetURLClassLoader categoriesForClass!Unclassified! !
!StaticJavaNetURLClassLoader methodsFor!

asURL: aString
	"private -- answer a String which represents the given string filename, directory name, or URL converted
	into an URL.
	Note that this only works with Smalltalk strings"

	| stream answer |

	#CUtodo. "this is seriously hacky..."

	"does it look like an URL already ?"
	(aString indexOf: $:) > 2 ifTrue: [^ aString].

	stream := (String writeStream)
			nextPutAll: 'file:///';
			yourself.

	aString do:
		[:ch |
		(ch isAlphaNumeric or: ['.-/?' includes: ch])
			ifTrue: [stream nextPut: ch]
			ifFalse: [ch = $\
				ifTrue: [stream nextPut: $/]
				ifFalse: [stream nextPut: $%; nextPutAll: ('%02x' sprintfWith: ch)]]].
	answer := stream contents.

	((answer endsWith: '/') or: [answer asLowercase endsWith: '.jar']) ifFalse:
		[answer := answer , '/'].

	^ answer.!

new
	"answer a new instance of java.net.URLClassLoader that has no explicit parent
	classloader and no URLs to search (usefull for holding dynamically generated
	classes)"

	^ self newURLs: #() parent: nil.!

new_URLArray: anURLs1
	"answer the result of calling the receiver's public new(java.net.URL[]) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anURLs1;
			yourself.

	^ self callConstructorSignature: '([Ljava/net/URL;)V' withArguments: args.
!

new_URLArray: anURLs1 ClassLoader: aClassLoader1
	"answer the result of calling the receiver's public new(java.net.URL[], java.lang.ClassLoader) Java constructor"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anURLs1;
			objectAt: 2 put: aClassLoader1;
			yourself.

	^ self callConstructorSignature: '([Ljava/net/URL;Ljava/lang/ClassLoader;)V' withArguments: args.
!

new_URLArray: anURLs1 ClassLoader: aClassLoader1 URLStreamHandlerFactory: anURLStreamHandlerFactory1
	"answer the result of calling the receiver's public new(java.net.URL[], java.lang.ClassLoader, java.net.URLStreamHandlerFactory) Java constructor"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: anURLs1;
			objectAt: 2 put: aClassLoader1;
			objectAt: 3 put: anURLStreamHandlerFactory1;
			yourself.

	^ self callConstructorSignature: '([Ljava/net/URL;Ljava/lang/ClassLoader;Ljava/net/URLStreamHandlerFactory;)V' withArguments: args.
!

newInstance_URLArray: anURLs1
	"answer the result of calling the receiver's public static newInstance(java.net.URL[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anURLs1;
			yourself.

	^ self callObjectMethod: 'newInstance' signature: '([Ljava/net/URL;)Ljava/net/URLClassLoader;' withArguments: args.
!

newInstance_URLArray: anURLs1 ClassLoader: aClassLoader1
	"answer the result of calling the receiver's public static newInstance(java.net.URL[], java.lang.ClassLoader) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anURLs1;
			objectAt: 2 put: aClassLoader1;
			yourself.

	^ self callObjectMethod: 'newInstance' signature: '([Ljava/net/URL;Ljava/lang/ClassLoader;)Ljava/net/URLClassLoader;' withArguments: args.
!

newParent: aJavaLangClassLoader
	"answer a new instance of java.net.URLClassLoader that has the given
	aJavaLangClassLoader parent but no collection of URLs to search"

	^ self newURLs: #() parent: aJavaLangClassLoader.!

newPath: aString parent: aJavaLangClassLoader
	"answer a new instance of java.net.URLClassLoader that has the given
	aJavaLangClassLoader parent and a search path derived by parsing the
	given ;-separated list of URLs, filenames or directory names.
	NB: we have to be a bit, um, heuristic about how we recognise and convert file/dir names"

	^ self
		newURLs: (self splitPath: aString)
		parent: aJavaLangClassLoader.!

newURLs: aCollectionOfStrings
	"answer a new instance of java.net.URLClassLoader that has no explicit parent
	classloader and the given collection of URLs to search"

	^ self newURLs: aCollectionOfStrings parent: nil.!

newURLs: aCollectionOfStrings parent: aJavaLangClassLoader
	"answer a new instance of java.net.URLClassLoader that has the given
	aJavaLangClassLoader parent and the given collection of URLs to search"

	| urlClass urls |

	#CUtodo.  "should we be trying to use our own classloader ?"
	urlClass := self jvm findClass: #'java.net.URL'.
	urls := aCollectionOfStrings
			ifNil: [#()]
			ifNotNil: [:them | them collect: [:each | urlClass new_String: each]].
	urls := urlClass newArrayWithAll: urls.

	"needless to say, the more general form of the method /isn't/ more general..."
	^ aJavaLangClassLoader
		ifNil: [self newInstance_URLArray: urls]
		ifNotNil: [:it | self newInstance_URLArray: urls ClassLoader: it].
!

splitPath: aString
	"private -- answer a search path derived by parsing the ;-separated list of URLs, filenames, or
	directory names"

	aString isNil ifTrue: [^ #()].

	^ (aString subStrings: ';')
		select: [:each | each notEmpty]
		thenCollect: [:each | self asURL: each].
! !
!StaticJavaNetURLClassLoader categoriesFor: #asURL:!helpers!private! !
!StaticJavaNetURLClassLoader categoriesFor: #new!instance creation!public! !
!StaticJavaNetURLClassLoader categoriesFor: #new_URLArray:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaNetURLClassLoader categoriesFor: #new_URLArray:ClassLoader:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaNetURLClassLoader categoriesFor: #new_URLArray:ClassLoader:URLStreamHandlerFactory:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newInstance_URLArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newInstance_URLArray:ClassLoader:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newParent:!instance creation!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newPath:parent:!instance creation!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newURLs:!instance creation!public! !
!StaticJavaNetURLClassLoader categoriesFor: #newURLs:parent:!instance creation!public! !
!StaticJavaNetURLClassLoader categoriesFor: #splitPath:!helpers!private! !

!StaticJavaNetURLClassLoader class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_URLArray:
		#new_URLArray:ClassLoader:
		#new_URLArray:ClassLoader:URLStreamHandlerFactory:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedSetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
	).
!

generatedWrapperSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#newInstance_URLArray:
		#newInstance_URLArray:ClassLoader:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.net.URLClassLoader'.
! !
!StaticJavaNetURLClassLoader class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaNetURLClassLoader class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaNetURLClassLoader class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaNetURLClassLoader class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaNetURLClassLoader class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaPrimitiveBooleanStatic guid: (GUID fromString: '{D219EA83-1408-4C22-BDF7-99921DAA165A}')!
JavaPrimitiveBooleanStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveBooleanStatic categoriesForClass!Unclassified! !
!JavaPrimitiveBooleanStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the boolean field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getBooleanFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: aBool
	"private -- set the boolean field defined by JNIFieldID it aJavaObject to aBool.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setBooleanFID: aJNIFieldID to: aBool.! !
!JavaPrimitiveBooleanStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveBooleanStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveBooleanStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaBooleanArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #boolean.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'Z'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Boolean'.
! !
!JavaPrimitiveBooleanStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveBooleanStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveBooleanStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveBooleanStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveByteStatic guid: (GUID fromString: '{77208B3B-7D43-4DDB-8E9D-B2F9619D2617}')!
JavaPrimitiveByteStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveByteStatic categoriesForClass!Unclassified! !
!JavaPrimitiveByteStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the byte field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getByteFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: anInteger
	"private -- set the byte field defined by JNIFieldID it aJavaObject to anInteger.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setByteFID: aJNIFieldID to: anInteger.! !
!JavaPrimitiveByteStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveByteStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveByteStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaByteArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #byte.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'B'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Byte'.
! !
!JavaPrimitiveByteStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveByteStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveByteStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveByteStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveCharStatic guid: (GUID fromString: '{C7CD7BD5-24C8-4258-A480-90FD39C1C7F0}')!
JavaPrimitiveCharStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveCharStatic categoriesForClass!Unclassified! !
!JavaPrimitiveCharStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the char field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getCharFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: aCharacterOrInteger
	"private -- set the char field defined by JNIFieldID it aJavaObject to aCharacterOrInteger.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setCharFID: aJNIFieldID to: aCharacterOrInteger.! !
!JavaPrimitiveCharStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveCharStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveCharStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaCharArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #char.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'C'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Character'.
! !
!JavaPrimitiveCharStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveCharStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveCharStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveCharStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveDoubleStatic guid: (GUID fromString: '{B4ED6BB1-AAD0-45EC-809C-7958B9422B7E}')!
JavaPrimitiveDoubleStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveDoubleStatic categoriesForClass!Unclassified! !
!JavaPrimitiveDoubleStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the double field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getDoubleFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: aFloat
	"private -- set the double field defined by JNIFieldID it aJavaObject to aFloat.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setDoubleFID: aJNIFieldID to: aFloat.! !
!JavaPrimitiveDoubleStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveDoubleStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveDoubleStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaDoubleArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #double.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'D'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Double'.
! !
!JavaPrimitiveDoubleStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveDoubleStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveDoubleStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveDoubleStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveFloatStatic guid: (GUID fromString: '{5A5F3136-9CC6-4AFD-AF7E-CDCBD0FD63A0}')!
JavaPrimitiveFloatStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveFloatStatic categoriesForClass!Unclassified! !
!JavaPrimitiveFloatStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the float field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getFloatFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: aFloat
	"private -- set the float field defined by JNIFieldID it aJavaObject to aFloat.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setFloatFID: aJNIFieldID to: aFloat.! !
!JavaPrimitiveFloatStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveFloatStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveFloatStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaFloatArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #float.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'F'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Float'.
! !
!JavaPrimitiveFloatStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveFloatStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveFloatStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveFloatStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveIntStatic guid: (GUID fromString: '{97B19E44-80C9-4444-800A-498A668442C9}')!
JavaPrimitiveIntStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveIntStatic categoriesForClass!Unclassified! !
!JavaPrimitiveIntStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the int field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getIntFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: anInteger
	"private -- set the int field defined by JNIFieldID it aJavaObject to anInteger.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setIntFID: aJNIFieldID to: anInteger.! !
!JavaPrimitiveIntStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveIntStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveIntStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaIntArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #int.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'I'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Integer'.
! !
!JavaPrimitiveIntStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveIntStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveIntStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveIntStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveLongStatic guid: (GUID fromString: '{9A43A04F-99D3-4EDE-9419-8792E290DF2F}')!
JavaPrimitiveLongStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveLongStatic categoriesForClass!Unclassified! !
!JavaPrimitiveLongStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the long field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getLongFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: anInteger
	"private -- set the long field defined by JNIFieldID it aJavaObject to anInteger.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setLongFID: aJNIFieldID to: anInteger.! !
!JavaPrimitiveLongStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveLongStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveLongStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaLongArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #long.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'J'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Long'.
! !
!JavaPrimitiveLongStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveLongStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveLongStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveLongStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitivesShortStatic guid: (GUID fromString: '{D8AB08D5-D075-488A-BE89-B11622C0C995}')!
JavaPrimitivesShortStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitivesShortStatic categoriesForClass!Unclassified! !
!JavaPrimitivesShortStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the short field defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	^ aJavaObject getShortFID: aJNIFieldID.!

setValueOfFID: aJNIFieldID in: aJavaObject to: anInteger
	"private -- set the short field defined by JNIFieldID it aJavaObject to anInteger.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	aJavaObject setShortFID: aJNIFieldID to: anInteger.! !
!JavaPrimitivesShortStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitivesShortStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitivesShortStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	^ JavaShortArray.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #short.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'S'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Short'.! !
!JavaPrimitivesShortStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitivesShortStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitivesShortStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitivesShortStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaPrimitiveVoidStatic guid: (GUID fromString: '{34C15135-E879-47BD-BC70-04948B285FF3}')!
JavaPrimitiveVoidStatic comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org
'!
!JavaPrimitiveVoidStatic categoriesForClass!Unclassified! !
!JavaPrimitiveVoidStatic methodsFor!

getValueOfFID: aJNIFieldID from: aJavaObject
	"private -- answer the result of getting the field of this type defined by JNIFieldID from aJavaObject.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	self shouldNotImplement.!

isVoid
	"answer whether we stand for the primitive Java type void"

	^ true.!

setValueOfFID: aJNIFieldID in: aJavaObject to: aFloat
	"private -- set the field of this type defined by JNIFieldID it aJavaObject to aFloat.
	(This is part of a double-dispatch-like pattern where the access bounces off
	ourself in order to determine the correct category of field"

	self shouldNotImplement.
! !
!JavaPrimitiveVoidStatic categoriesFor: #getValueOfFID:from:!private!reflection! !
!JavaPrimitiveVoidStatic categoriesFor: #isVoid!public!testing! !
!JavaPrimitiveVoidStatic categoriesFor: #setValueOfFID:in:to:!private!reflection! !

!JavaPrimitiveVoidStatic class methodsFor!

arrayClass
	"answer the Smalltalk wrapper class that is used for arrays of objects of the type we represent"

	"can't have arrays of void!!"
	self shouldNotImplement.!

javaTypeName
	"answer the Java name of primitive type to which we correspond.
	It is helpful, but not obligatory, for this to be a Symbol rather than String"

	^ #void.
!

jniSignature
	"answer a JNI-style signature for this class"

	^ 'V'.!

wrapperJavaClassName
	"answer the name of the java.lang.* class which wraps the kinds of primitives we correspond to"

	^ #'java.lang.Void'.
! !
!JavaPrimitiveVoidStatic class categoriesFor: #arrayClass!constants!public! !
!JavaPrimitiveVoidStatic class categoriesFor: #javaTypeName!converting!public! !
!JavaPrimitiveVoidStatic class categoriesFor: #jniSignature!accessing!constants!public! !
!JavaPrimitiveVoidStatic class categoriesFor: #wrapperJavaClassName!constants!public! !

JavaClassRegistry guid: (GUID fromString: '{F6526FC0-DE08-11D5-8727-00107A150673}')!
JavaClassRegistry comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of these holds a collection of JavaLangClass objects.  It ensures that no further class objects are added which correspond to a Java class which is already in the registry.

Ownership is shared between the JVM and the class static for java.lang.Class (which uses the class registry as its ObjectRegistry).

It is important to understand that any objects entered into the registry are made *unfinalizable*.  That is because they are now "owned" by the registry and are no longer responsible for cleaning up after themselves.  Also note that the owning JVM is finalizable, and that it holds a ref to the registry; if the registry or any of its contents were finalizable too, then we''d have a finalization loop, where the Dolphin VM will choose one object *arbitrarily* to finalize first.'!
!JavaClassRegistry categoriesForClass!Unclassified! !
!JavaClassRegistry methodsFor!

add: aJavaObject
	"add a java object to the registry, replacing the existing incumbent, if any"

	self shouldNotImplement.!

addToRegistry: aJavaLangClass
	"private -- add aJavaObject to the registry.
	Assumes that it is called under the protection of the sharedMutex, and that aJavaLangClass
	is not already present in the registry"

	| new |

	"NB: we are trying to allow read access to the old registry while we are constructing the new
	one.  This is more expensive in the relatively rare case where an object is added to the registry
	but avoids the cost of a lock on every read access -- of which there are *LOTS*"

	"reduce the load on the local pool"
	aJavaLangClass beGlobalRef.

	"registered classes should not be finalisable; see the class comment for more info.
	Note we can't make it unfinalisable until now when we are *sure* we are going to add it to the
	registry"
	aJavaLangClass beUnfinalizable.

	"make a copy of the registry with the new class added"
	new := (self makeRegistry: 2 * registry size + 1)		"force low occupancy for speed"
			addAll: registry;
			add: aJavaLangClass;
			yourself.

	"swap over.  Hopefully this is atomic from the POV of other theads which may be
	reading (but not modifying) the registry at the same time, and which are *NOT*
	under the shared mutex"
	registry := new.
!

allArrayClasses
	"answer an OrderedCollection of all the registered classes that stand for Java interfaces"

	^ self allClasses select: [:each | each isArray].!

allClasses
	"answer an OrderedCollection of all the registered classes"

	^ self contents collect: [:each | each classStatic].

	!

allClassObjects
	"answer an OrderedCollection of all the registered classes' class objects"

	^ self contents.!

allInterfaces
	"answer an OrderedCollection of all the registered classes that stand for Java interfaces"

	^ self allClasses select: [:each | each isInterface].!

allPrimitiveTypes
	"answer an OrderedCollection of all the registered classes that stand for Java primitive types"

	^ self allClasses select: [:each | each isPrimitive].!

allRootClasses
	"answer an OrderedCollection of all the registered classes that are normal classes without parents
	(i.e, in fact, just java.lang.Object)"

	^ self allClasses reject: [:each | each isArray or: [each isPrimitive or: [each isInterface or: [each javaSuperclass notNil]]]].!

findOrRelease: aJNIClass ifAbsentPut: a0Block
	"look for an entry in the registry that refers to the same Java object as
	aJNIObject, if one exists then release aJNIObject and answer the incumbent.
	Otherwise add and answer the result of evaluating a0Block.  This is overridden
	so as not to hold onto locks for too long"

	| previous |

	"this is called *A LOT*, so the test has to be quick.
	NB: we are deliberately not yet using the shared mutex, see #addToRegistry for more detail"
	previous := registry find: aJNIClass ifAbsent: [nil].
	previous isNil
		ifTrue: [previous := self registerClassObject: a0Block value]
		ifFalse: [aJNIClass releaseRef: jvm jniEnv].

	^ previous.
!

javaLangClass
	"answer the JavaStatic corresponding to java.lang.Class"

	"the JVM needs fast access to this, so we cache it in an instvar"
	^ javaLangClass.!

javaLangObject
	"answer the JavaStatic corresponding to java.lang.Object"

	"the JVM needs fast access to this, so we cache it in an instvar"
	^ javaLangObject.!

javaLangString
	"answer the JavaStatic corresponding to java.lang.String"

	"the JVM needs fast access to this, so we cache it in an instvar"
	^ javaLangString.!

knownDirectSubclassesOf: aJavaStatic
	"answer an OrderedCollection of all the *known* direct subclasses of aJavaStatic.
	NB: by subclass we *mean* subclass, interfaces don't count"

	^ self allClasses select: [:each | each javaSuperclass == aJavaStatic].
!

knownSubclassesOf: aJavaStatic
	"answer an OrderedCollection of all the *known* subclasses of aJavaStatic.
	NB: by subclass we *mean* subclass, interfaces don't count"

	^ self allClasses select: [:each | each isSubclassOf: aJavaStatic].!

notifyRegistered: aClassStatic
	"private -- send out, or add to the pending queue, notification to the effect that aClassStatic has been
	registered"

	#CUtodo. "use the mutex ?"

	notificationQueue ifNotNil: [:it | it addLast: aClassStatic. ^ self].

	"tell the various interested parties; the JVM must be told first since it may have
	interested Watchers (e.g. waiting to convert the class into a ghost)"
	jvm notifyClassRegistered: aClassStatic.
	aClassStatic notifyRegistered.
	self trigger: #classRegistered: with: aClassStatic.

!

purge: aJavaStatic
	"private -- do *NOT* call this!!"

	self trigger: #classPurged: with: aJavaStatic.
	jvm notifyClassPurged: aJavaStatic.

	sharedMutex critical:
		[| new |
		new := self makeRegistry: 2 * registry size.		"force low occupancy for speed"
		registry do: [:each | each classStatic == aJavaStatic ifFalse: [new add: each]].
		registry := new].

	aJavaStatic classObject beFinalizable.
!

registerClassObject: aJavaLangClass
	"private -- add a new class object to the registry.  It is assumed that aJNIClass is not normally
	already present in the registry, however we do need to double-check.
	Answers the added object or the previous resident if there turned out to be one"

	sharedMutex critical:
		[| it |
		it := registry find: aJavaLangClass ifAbsent: [nil].
		it isNil ifFalse: [^ it].
		self addToRegistry: aJavaLangClass].

	self notifyRegistered: aJavaLangClass classStatic.

	^ aJavaLangClass.!

registerSystemClasses
	"private -- called during initialisation.  Create wrappers for the raw JNIClass objects, and add them
	to the class registry.  Note that this *must* happen before any other classes are registered or else an
	infinite loop will occurr"

	| rawJavaLangObject rawJavaLangClass rawJavaLangString |

	rawJavaLangObject := jvm jniEnv FindClass_name: 'java/lang/Object'.
	self assert: [rawJavaLangObject notNull].

	rawJavaLangClass := jvm jniEnv FindClass_name: 'java/lang/Class'.
	self assert: [rawJavaLangClass notNull].

	rawJavaLangString := jvm jniEnv FindClass_name: 'java/lang/String'.
	self assert: [rawJavaLangString notNull].

	"note that the Java class of java.lang.Class is itself, that it is a subclass of java.lang.Object, and
	that the class of java.lang.Object is an instance of java.lang.Class.  Hence we have to be careful
	about the order we do things here..."

	"start by creating (normally illegal) blank class static object for the classes"
	javaLangObject := StaticJavaLangObject newWithJVM: jvm.
	javaLangClass := StaticJavaLangClass newWithJVM: jvm.
	javaLangString := StaticJavaLangObject newWithJVM: jvm.

	"can now finish creating java.lang.Object"
	javaLangObject
		instanceClass: JavaLangObject;	"not necessary ?"
		classObject: ((JavaLangClass jniObject: rawJavaLangObject static: javaLangClass)
					classStatic: javaLangObject;
					yourself).

	"and java.lang.Class"
	javaLangClass
		javaSuperclass: javaLangObject;
		instanceClass: JavaLangClass;
		classObject: ((JavaLangClass jniObject: rawJavaLangClass static: javaLangClass)
					classStatic: javaLangClass;
					yourself).

	"and now do java.lang.String"
	javaLangString
		javaSuperclass: javaLangObject;
		instanceClass: JavaLangString;
		classObject: ((JavaLangClass jniObject: rawJavaLangString static: javaLangClass)
					classStatic: javaLangString;
					yourself).

	"add them to the registry"
	self addToRegistry: javaLangObject classObject.
	self addToRegistry: javaLangClass classObject.
	self addToRegistry: javaLangString classObject.

	"NB: deliberately don't send the notifications that #registerClassObject: does, since
	we're not bootstrapped yet"
!

releaseRegistrationNotifications
	"private -- stop enqueing registration notifications, and send any pending notifications
	that have been enqueued"

	notificationQueue ifNotNil:
		[:them |
		notificationQueue := nil.
		them do: [:each | self notifyRegistered: each]].!

shutdown
	"private -- called by our owning JVM when it shuts down"

	"intentionally do not super-send"

	javaLangClass := javaLangObject := javaLangString := nil.
	notificationQueue := nil.
	sharedMutex critical: [registry := self makeRegistry: 0].!

suspendRegistrationNotifications
	"private -- arrange to enqueue all registration notifications until such time
	as #releaseRegistrationNotifications is called"

	notificationQueue isNil ifTrue: [notificationQueue := OrderedCollection new].
!

suspendRegistrationNotificationsWhile: a0Block
	"evaluate a0Block in an environment where registration notifications
	are suspended until the block has completed"

	self suspendRegistrationNotifications.
	^ a0Block ensure: [self releaseRegistrationNotifications].! !
!JavaClassRegistry categoriesFor: #add:!adding!public! !
!JavaClassRegistry categoriesFor: #addToRegistry:!adding!private! !
!JavaClassRegistry categoriesFor: #allArrayClasses!enumerating!public! !
!JavaClassRegistry categoriesFor: #allClasses!enumerating!public! !
!JavaClassRegistry categoriesFor: #allClassObjects!enumerating!public! !
!JavaClassRegistry categoriesFor: #allInterfaces!enumerating!public! !
!JavaClassRegistry categoriesFor: #allPrimitiveTypes!enumerating!public! !
!JavaClassRegistry categoriesFor: #allRootClasses!enumerating!public! !
!JavaClassRegistry categoriesFor: #findOrRelease:ifAbsentPut:!accessing!public! !
!JavaClassRegistry categoriesFor: #javaLangClass!accessing!private! !
!JavaClassRegistry categoriesFor: #javaLangObject!accessing!private! !
!JavaClassRegistry categoriesFor: #javaLangString!accessing!private! !
!JavaClassRegistry categoriesFor: #knownDirectSubclassesOf:!enumerating!public! !
!JavaClassRegistry categoriesFor: #knownSubclassesOf:!enumerating!public! !
!JavaClassRegistry categoriesFor: #notifyRegistered:!notifying!private! !
!JavaClassRegistry categoriesFor: #purge:!private!removing! !
!JavaClassRegistry categoriesFor: #registerClassObject:!adding!private! !
!JavaClassRegistry categoriesFor: #registerSystemClasses!initializing!managed objects!private! !
!JavaClassRegistry categoriesFor: #releaseRegistrationNotifications!notifying!private! !
!JavaClassRegistry categoriesFor: #shutdown!initializing!private! !
!JavaClassRegistry categoriesFor: #suspendRegistrationNotifications!notifying!private! !
!JavaClassRegistry categoriesFor: #suspendRegistrationNotificationsWhile:!notifying!public! !

!JavaClassRegistry class methodsFor!

defaultCapacity
	"private -- answer the default capacity to use for instances"

	^ 200.!

publishedEventsOfInstances
	"answer a Set of Symbols that describe the published events triggered
	by JVM instances"	

	^ (super publishedEventsOfInstances)
		add: #classRegistered:;		"parameter is the new class static"
		add: #classPurged:;			"parameter is the newly purged class static"
		yourself.!

registryContainerClass
	"private -- answer the kind of container used to hold elements"

	"overriden becuase we hold strong refs to objects"
	^ PluggableSet.! !
!JavaClassRegistry class categoriesFor: #defaultCapacity!constants!private! !
!JavaClassRegistry class categoriesFor: #publishedEventsOfInstances!constants!development!events!public! !
!JavaClassRegistry class categoriesFor: #registryContainerClass!constants!private! !

JVMWithoutCallbacks guid: (GUID fromString: '{43892165-3188-45BA-BDB9-D7E85C86A4A8}')!
JVMWithoutCallbacks comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Special subclass of JVM that forbids any callbacks.  The principle differences are:
	- we can use JNI "local" references (which are supposed to be faster).
	- we (therefore) keep track of the local pool size, and adjust it when necessary.
	- we don''t set up a class registry.

Please note: despite the subclass relationship, and despite the fact that this class is in a separate (and separable) package from the other concrete subclass, this class and the other and our superclass are not independent in design.'!
!JVMWithoutCallbacks categoriesForClass!Unclassified! !
!JVMWithoutCallbacks methodsFor!

asCallbackDo: a0Block
	"answer the result of evaluating a0Block in an environment where we have
	replaced the current JNIEnv with the currently valid one"

	"we don't support callbacks, if we reach this point then we are probably buggered"
	self assert: [false].!

callbackDepth
	"answer how deeply nested we are in callbacks"

	"we don't do callbacks, so there can't be any outstanding..."
	^ 0.!

callbackRegistry
	"answer our registry of callbacks, or rather answer nil since we don't support
	callbacks"

	^ nil.!

callbacks
	"answer how many callbacks have been serviced"

	"we don't do callbacks, so there can't have been any..."
	^ 0.!

initializeCallbackRegistry
	"private -- called during initialisation.  We are now sufficiently initialized for a callback registry
	to work; if we want one then set one up now"

	"we don't"!

initialLocalCapacity
	"private -- answer how many slots of local references we will demand of the JVM"

	"must be at least big enough for us to initialise the system without calling #increaseLocalCapacity,
	actually we go for a much larger figure than that"
	^ 1000.	"the JNI default is only 16 !!"!

localCapacityIncrement
	"private -- answer how many more slots of local references we will ask for if we
	discover that we're runnig out"

	^ 500.!

shutdownCallbackRegistry
	"private -- shutdown any callback registry we have"

	"nothing to do"! !
!JVMWithoutCallbacks categoriesFor: #asCallbackDo:!Java callbacks!public! !
!JVMWithoutCallbacks categoriesFor: #callbackDepth!Java callbacks!monitoring!public! !
!JVMWithoutCallbacks categoriesFor: #callbackRegistry!accessing!Java callbacks!public! !
!JVMWithoutCallbacks categoriesFor: #callbacks!Java callbacks!monitoring!public! !
!JVMWithoutCallbacks categoriesFor: #initializeCallbackRegistry!initializing!Java callbacks!private! !
!JVMWithoutCallbacks categoriesFor: #initialLocalCapacity!constants!private! !
!JVMWithoutCallbacks categoriesFor: #localCapacityIncrement!constants!private! !
!JVMWithoutCallbacks categoriesFor: #shutdownCallbackRegistry!private!shutting down! !

JavaRuntimeSettings guid: (GUID fromString: '{3D1C746C-B527-4B54-8934-EFE4D72B7F38}')!
JavaRuntimeSettings comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of these holds settings that are passed to the JNI runtime.  The options are essentially the same as the arguments that can be passed to the "java" command (or rather, it''s the other way around, since the arguments to "java" are just packaged up into runtime options for the JVM).'!
!JavaRuntimeSettings categoriesForClass!Unclassified! !
!JavaRuntimeSettings methodsFor!

addOption: aString
	"append the option given by aString to our list.  Does not remove
	any previous version of the same option"

	options addLast: aString.
!

classpath
	"answer the receiver's classpath as ';'-separated String, or nil"

	^ self
		propertyAt: self classpathPropertyName
		ifAbsent: [nil].
!

classpath: aStringOrNil
	"set the receiver's classpath; if the argument is nil then the property is removed"

	self
		propertyAt: self classpathPropertyName
		put: aStringOrNil.!

classpathPropertyName
	"answer the name of the 'property' that controls the JVMs classpath"

	^  'java.class.path'.
!

importClasspathFromEnvironment
	"attempt to set the classpath property from the %CLASSPATH% environment variable"

	| classpath |

	classpath := SessionManager current getenv: 'CLASSPATH'.
	classpath isNil ifFalse: [self propertyAt: self classpathPropertyName put: classpath].!

initialize
	"private -- establish a default initial state.
	NB: this is only used for creating a new 'default' (template) instance.  Subsequent
	instances are created by cloning that"

	options := OrderedCollection new.
	self importClasspathFromEnvironment.

	super initialize.

!

javaVMInitArgs
	"answer a JavaVMInitArgs instance discovered from our settings"

	^ (JavaVMInitArgs new)
		addOptions: options;
		yourself.!

libpath
	"answer the receiver's libpath as ';'-separated String, or nil"

	^ self
		propertyAt: self libpathPropertyName
		ifAbsent: [nil].
!

libpath: aStringOrNil
	"set the receiver's libpath; if the argument is nil then the property is removed"

	self
		propertyAt: self libpathPropertyName
		put: aStringOrNil.!

libpathPropertyName
	"answer the name of the 'property' that controls the JVMs libpath"

	^  'java.library.path'.
!

loadOtherAspectsFromRegistryUnder: aRegKey
	"private -- load our non-standard state from the registry under the given key.
	Answer true iff we found an acceptable minumum of data"

	| count |

	count := aRegKey valueAt: 'option count' ifAbsent: [0].
	options := OrderedCollection new: count.
	1 to: count do:
		[:i  || subkey |
		subkey := 'option %03d' sprintfWith: i.
		options addLast: (aRegKey valueAt: subkey ifAbsent: [^ true])].

	^ true.
!

options
	"answer the receiver's options, an OrderedCollection of Strings"

	^ options.
!

options: anOrderedCollection
	"set the receiver's options, an OrderedCollection of Strings"

	options := OrderedCollection withAll: anOrderedCollection.
!

propertyAt: aString ifAbsent: a0Block
	"answer the property named by aString, or the result of
	evaluating a0Block if there isn't one"

	| index |

	index := self propertyIndex: aString ifAbsent: [^ a0Block value].
	^ self splitProperty: aString from: (options at: index).!

propertyAt: aString put: anotherString
	"set the property named by aString"

	| index option |

	index := self propertyIndex: aString ifAbsent: [nil].
	option := self propertyLabel: aString value: anotherString.
	index isNil
		ifTrue: [options addLast: option]
		ifFalse: [options at: index put: option].!

propertyIndex: aString ifAbsent: a0Block
	"private -- answer the index of the propery named by aString, or the result
	of evaluating a0Block if there isn't one"

	| label |

	label := self propertyLabel: aString.
	options keysAndValuesDo: [:i :each | (each beginsWith: label) ifTrue: [^ i]].
	^ a0Block value.!

propertyLabel: aString
	"private -- answer the label to use for the propery named by aString"

	^ '-D%s=' sprintfWith: aString.!

propertyLabel: aString value: anotherString
	"private -- answer the string to use for setting the propery named by aString"

	^ '-D%s=%s' sprintfWith: aString with: anotherString.!

removeOption: aString
	"remove the option given by aString from our list"

	options remove: aString ifAbsent: [].
!

removePropertyAt: aString
	"ensure that we do not have a property named by aString"

	options removeAtIndex: (self propertyIndex: aString ifAbsent: [^ self]).!

saveOtherAspectsToRegistryUnder: aRegKey
	"private -- save our state to the registry under the given key"

	#CUtodo.  "we should remove the old ones"
	aRegKey valueAt: 'option count' put: options size.
	options keysAndValuesDo:
		[:i :each || subkey |
		subkey := 'option %03d' sprintfWith: i.
		aRegKey valueAt: subkey put: each].!

splitProperty: aString from: anotherString
	"private -- answer the String value of the property named by aString in the option
	string, anotherString"

	"format is '-D<name>=<value>'"
	^ anotherString allButFirst: aString size + 3.! !
!JavaRuntimeSettings categoriesFor: #addOption:!options!public! !
!JavaRuntimeSettings categoriesFor: #classpath!accessing!public! !
!JavaRuntimeSettings categoriesFor: #classpath:!accessing!public! !
!JavaRuntimeSettings categoriesFor: #classpathPropertyName!constants!public! !
!JavaRuntimeSettings categoriesFor: #importClasspathFromEnvironment!initializing!properties!public! !
!JavaRuntimeSettings categoriesFor: #initialize!initializing!private! !
!JavaRuntimeSettings categoriesFor: #javaVMInitArgs!accessing!public! !
!JavaRuntimeSettings categoriesFor: #libpath!accessing!public! !
!JavaRuntimeSettings categoriesFor: #libpath:!accessing!public! !
!JavaRuntimeSettings categoriesFor: #libpathPropertyName!constants!public! !
!JavaRuntimeSettings categoriesFor: #loadOtherAspectsFromRegistryUnder:!private!registry! !
!JavaRuntimeSettings categoriesFor: #options!options!public! !
!JavaRuntimeSettings categoriesFor: #options:!options!public! !
!JavaRuntimeSettings categoriesFor: #propertyAt:ifAbsent:!properties!public! !
!JavaRuntimeSettings categoriesFor: #propertyAt:put:!properties!public! !
!JavaRuntimeSettings categoriesFor: #propertyIndex:ifAbsent:!private!properties! !
!JavaRuntimeSettings categoriesFor: #propertyLabel:!private!properties! !
!JavaRuntimeSettings categoriesFor: #propertyLabel:value:!private!properties! !
!JavaRuntimeSettings categoriesFor: #removeOption:!options!public! !
!JavaRuntimeSettings categoriesFor: #removePropertyAt:!properties!public! !
!JavaRuntimeSettings categoriesFor: #saveOtherAspectsToRegistryUnder:!private!registry! !
!JavaRuntimeSettings categoriesFor: #splitProperty:from:!private!properties! !

!JavaRuntimeSettings class methodsFor!

initialize
	"private -- class initialization.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #runtimeSettings.

!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

	| suggestions aspect |

	aspect := Smalltalk at: #Aspect.

	suggestions := #(
				'-Dxxx=yyy'
				'-verbose:jni'
				'-verbose:gc'
				'-verbose:class'
				'-Xcheck:jni'
				'-xxx' '-Xyyy'
				'-XXyyy'
			).
	(SessionManager current getenv: 'CLASSPATH') ifNotNil:
		[:it |
		suggestions := suggestions asOrderedCollection.
		suggestions addLast: ('-Djava.class.path=%s' sprintfWith: it)].

	"we add #classpath and #libpath here, rather than adding them to #stringAspectNames, to stop them
	being persisted"
	^ super publishedAspectsOfInstances
		add: (aspect sequenceableCollection: #options addFrom: suggestions);
		add: (aspect string: #classpath);
		add: (aspect string: #libpath);
		yourself.
!

suggestedJVMOptions
	"answer an array of suggested values of options to the JVM"

	| suggestions |

	suggestions :=
		#(
			"turning on verbosity"
			'-verbose:jni'
			'-verbose:gc'
			'-verbose:class'
	
			"turning on JNI checks"
			'-Xcheck:jni'
	
			"template for debug options (7777 is a port number)"
			'-Xdebug'
			'--Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=7777'
	
			"generic options"
			'-Dxxx=yyy'
			'-xxx' '-Xyyy'
			'-XXyyy'
		).

	(SessionManager current getenv: 'CLASSPATH') ifNotNil:
		[:it |
		suggestions := suggestions asOrderedCollection.
		suggestions addLast: ('-Djava.class.path=%s' sprintfWith: it)].

	^ suggestions asArray.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #runtimeSettings.
! !
!JavaRuntimeSettings class categoriesFor: #initialize!initializing!private! !
!JavaRuntimeSettings class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!JavaRuntimeSettings class categoriesFor: #suggestedJVMOptions!constants!public! !
!JavaRuntimeSettings class categoriesFor: #uninitialize!initializing!private! !

JNIPortSettings guid: (GUID fromString: '{2A4FBBCF-1D05-410E-98E9-F3CA6766515C}')!
JNIPortSettings comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of these holds the basic settings that cofigure JNIPort and the way it talks to the JNI library.'!
!JNIPortSettings categoriesForClass!Unclassified! !
!JNIPortSettings methodsFor!

initialize
	"private -- establish a default initial state.
	NB: this is only used for creating a new 'default' (template) instance.  Subsequent
	instances are created by cloning that"

	super initialize.

	jvmClass := JVMWithoutCallbacks.
	jniLibraryClass := JNILibraryForSunJRE.
	jniLibraryFilename := nil.
	jniLibraryVersion := jniLibraryJVMType := nil.
	watcherClasses := OrderedCollection new.
!

jniLibrary
	"answer a JNI library instance discovered from our settings, or nil if we have not been
	configured with that info"

	jniLibraryClass isNil ifTrue: [^ nil].

	jniLibraryFilename isNil ifFalse: [^ jniLibraryClass open: jniLibraryFilename].

	(jniLibraryVersion isNil and: [jniLibraryJVMType isNil]) ifTrue: [^ jniLibraryClass default].

	(jniLibraryJVMType isNil) ifTrue: [^ jniLibraryClass version: jniLibraryVersion].

	^ jniLibraryClass version: jniLibraryVersion jvmType: jniLibraryJVMType.
!

jniLibraryClass
	"answer the receiver's jniLibraryClass, i.e. the subclass of JNILibrary that we will connect to
	to run a Java runtime.  The choice of library class primarily determins the way that the actual
	jvm.dll is located (using data from the registry, etc)"

	^ jniLibraryClass.
!

jniLibraryClass: aSmalltalkClass
	"set the receiver's jniLibraryClass to aSmalltalkClass"

	jniLibraryClass := aSmalltalkClass.
!

jniLibraryFilename
	"answer the receiver's jniLibraryFilename.  If this is set then it is overrides
	the jni libraries default rules for finding the jvm.dll"

	^ jniLibraryFilename.
!

jniLibraryFilename: aStringOrNil
	"set the receiver's jniLibraryFilename to aStringOrNil"

	jniLibraryFilename := aStringOrNil.
	(aStringOrNil isNil or: [aStringOrNil isEmpty]) ifFalse:
		[jniLibraryVersion := jniLibraryJVMType := nil].
!

jniLibraryJVMType
	"answer the receiver's jniLibraryJVMType string.  If this is set then it overrides
	the default assumptions (as encoded in the registry or whereever) about
	which type of jvm to start.  Not all vendors support multiple JVM types,
	but Sun had 'client' 'classic' 'hotspot' and 'server' up though 1.3, since then
	they only support 'client' and 'server'"

	^ jniLibraryJVMType.
!

jniLibraryJVMType: anObject
	"set the receiver's jniLibraryJVMType to anObject"

	jniLibraryJVMType := anObject.
	anObject isNil ifFalse: [jniLibraryFilename := nil].
!

jniLibraryVersion
	"answer the receiver's jniLibraryVersion string.  If this is set then
	it overrides the jni libraries default assumption about what version
	of the jvm.dll to start.  Normally the default is to start the latest
	version.  If it is set then it will typically be something like '1.4' or
	1.4.1, but it does depend on the vendor"

	^ jniLibraryVersion.
!

jniLibraryVersion: anObject
	"set the receiver's jniLibraryVersion to anObject"

	jniLibraryVersion := anObject.
	anObject isNil ifFalse: [jniLibraryFilename := nil].
!

jvmClass
	"answer the receiver's jvmClass, i.e. the subclass of JVM that will actually be
	started to wrap/monitor the Java runtime.  At present the choice is between
	JVMWithCallbacks and JVMWithoutCallbacks, which differ in whether they
	support setting callbacks from Java into Smalltalk"

	^ jvmClass.
!

jvmClass: aSmalltalkClass
	"set the receiver's jvmClass, i.e. the subclass of JVM that will actually be
	started to wrap/monitor the Java runtime"

	jvmClass := aSmalltalkClass.
!

loadOtherAspectsFromRegistryUnder: aRegKey
	"private -- load our non-standard state from the registry under the given key.
	Answer true iff we found an acceptable minumum of data"

	| classkey value count |

	classkey := aRegKey at: 'jvmClass' ifAbsent: [^ false].
	jvmClass := self loadClassFromRegistryUnder: classkey ifNone: [^ false].

	classkey := aRegKey at: 'jniLibraryClass' ifAbsent: [^ false].
	jniLibraryClass := self loadClassFromRegistryUnder: classkey ifNone: [^ false].

	jniLibraryFilename := aRegKey valueAt: 'jniLibraryFilename' ifAbsent: [nil].

	count := aRegKey valueAt: 'watcherClasses count' ifAbsent: [0].
	watcherClasses := OrderedCollection new: count.
	1 to: count do:
		[:i || subkey |
		subkey := 'watcherClasses %d' sprintfWith: i.
		classkey := aRegKey at: subkey ifAbsent: [^ true].
		watcherClasses addLast: (self loadClassFromRegistryUnder: classkey ifNone: [^ true])].

	^ true.
!

saveOtherAspectsToRegistryUnder: aRegKey
	"private -- save our non-standard state to the registry under the given key"

	self saveClass: jvmClass toRegistryUnder: (aRegKey createKey: 'jvmClass').
	self saveClass: jniLibraryClass toRegistryUnder: (aRegKey createKey: 'jniLibraryClass').

	aRegKey valueAt: 'jniLibraryFilename' put: jniLibraryFilename.

	aRegKey valueAt: 'watcherClasses count' put: watcherClasses size.
	watcherClasses keysAndValuesDo:
		[:i :each || subkey |
		subkey := aRegKey createKey: ('watcherClasses %d' sprintfWith: i).
		self saveClass: each toRegistryUnder: subkey].
!

useAbortHook
	"answer whether we will attempt to use the JNI runtime 'abort' hook;
	that allows the JVM object to get notification from the Java runtime
	when the runtime aborts.  It is unwise to use this unless you have a
	'jni hleper' configured, since you may get deadlocks otherwise"

	^ self allFlagsSet: USE_ABORT_HOOK_MASK.
!

useAbortHook: aBool
	"set whether we will attempt to use the JNI runtime 'abort' hook.
	Note that this risks almost certainly deadlocking Dolphin unless
	the JNIHelper library is also used"

	self setFlags: USE_ABORT_HOOK_MASK to: aBool.
!

useExitHook
	"answer whether we will attempt to use the JNI runtime 'exit' hook"

	^ self allFlagsSet: USE_EXIT_HOOK_MASK.
!

useExitHook: aBool
	"set whether we will attempt to use the JNI runtime 'exit' hook.
	Note that setting this will cause Dolphin to hand (deadlock) when exiting any
	of the current range of Sun JVMs"

	self setFlags: USE_EXIT_HOOK_MASK to: aBool.
!

useJNIHelperLibrary
	"answer whether we will attempt to use the JNIHelper library"

	^ self allFlagsSet: USE_JNI_HELPER_LIBRARY_MASK.
!

useJNIHelperLibrary: aBool
	"set whether we will attempt to use the JNIHelper library (which allows us to
	hook abort() and vfprintf() without deadlocking Dolphin -- it should allow hooking
	exit() too, but that doesn't work)"

	self setFlags: USE_JNI_HELPER_LIBRARY_MASK to: aBool.
!

useVFPrintfHook
	"answer whether we will attempt to use the JNI runtime 'vfprintf' hook"

	^ self allFlagsSet: USE_VFPRINTF_HOOK_MASK.
!

useVFPrintfHook: aBool
	"set whether we will attempt to use the JNI runtime 'vfprintf' hook.
	Note that this risks almost certainly deadlocking Dolphin unless
	the JNIHelper library is also used"

	self setFlags: USE_VFPRINTF_HOOK_MASK to: aBool.
!

useVFPrintfRedirection
	"answer whether we will attempt to set the JNI runtime 'vfprintf' hook to
	redirect the JVM's debugging output to the Windows debug stream.  If this
	is used (and it requires useVFPrintfHook *and* useJNIHelper), then debuging
	output will go to the system output stream without touching Dolphin at all, which
	may help diagnose/avoid difficulties"

	^ self allFlagsSet: USE_VFPRINTF_REDIRECT_MASK.!

useVFPrintfRedirection: aBool
	"answer whether we will attempt to set the JNI runtime 'vfprintf' hook to
	redirect the JVM's debugging output to the Windows debug stream.  If this
	is used (and it requires useVFPrintfHook *and* useJNIHelper), then debuging
	output will go to the system output stream without touching Dolphin at all, which
	may help diagnose/avoid difficulties"

	self setFlags: USE_VFPRINTF_REDIRECT_MASK to: aBool.!

watcherClasses
	"answer the OrderedCollection of classes that JVMs will send notifications to as they start up.
	E.g. to use ghost classes you need to configure JVMGhostMaker as a watcher class"

	^ watcherClasses.!

watcherClasses: aCollection
	"set the OrderedCollection of classes that JVMs will send notifications to as they start up"

	watcherClasses := OrderedCollection withAll: aCollection.! !
!JNIPortSettings categoriesFor: #initialize!initializing!private! !
!JNIPortSettings categoriesFor: #jniLibrary!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryClass!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryClass:!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryFilename!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryFilename:!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryJVMType!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryJVMType:!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryVersion!accessing!public! !
!JNIPortSettings categoriesFor: #jniLibraryVersion:!accessing!public! !
!JNIPortSettings categoriesFor: #jvmClass!accessing!public! !
!JNIPortSettings categoriesFor: #jvmClass:!accessing!public! !
!JNIPortSettings categoriesFor: #loadOtherAspectsFromRegistryUnder:!private!registry! !
!JNIPortSettings categoriesFor: #saveOtherAspectsToRegistryUnder:!private!registry! !
!JNIPortSettings categoriesFor: #useAbortHook!accessing!public!testing! !
!JNIPortSettings categoriesFor: #useAbortHook:!accessing!public! !
!JNIPortSettings categoriesFor: #useExitHook!accessing!public!testing! !
!JNIPortSettings categoriesFor: #useExitHook:!accessing!public! !
!JNIPortSettings categoriesFor: #useJNIHelperLibrary!accessing!public!testing! !
!JNIPortSettings categoriesFor: #useJNIHelperLibrary:!accessing!public! !
!JNIPortSettings categoriesFor: #useVFPrintfHook!accessing!public!testing! !
!JNIPortSettings categoriesFor: #useVFPrintfHook:!accessing!public! !
!JNIPortSettings categoriesFor: #useVFPrintfRedirection!accessing!public! !
!JNIPortSettings categoriesFor: #useVFPrintfRedirection:!accessing!public! !
!JNIPortSettings categoriesFor: #watcherClasses!accessing!public! !
!JNIPortSettings categoriesFor: #watcherClasses:!accessing!public! !

!JNIPortSettings class methodsFor!

booleanAspectNames
	"private -- answer an Array of the names of boolean aspects of instances"

	^ super booleanAspectNames
		, #(
			#useJNIHelperLibrary
			#useExitHook
			#useAbortHook
			#useVFPrintfHook
			#useVFPrintfRedirection
		).
!

defaultFlags
	"answer the collection of flags that are set by default"

	^ (Smalltalk includesKey: #DolphinJNIHelperLibrary)
		ifTrue: [	USE_ABORT_HOOK_MASK
	"			| USE_EXIT_HOOK_MASK				-- doesn't work"
				| USE_VFPRINTF_HOOK_MASK
				| USE_JNI_HELPER_LIBRARY_MASK]
		ifFalse: [0].

!

initialize
	"private -- class initialization.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #jniPortSettings.


!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

	| aspect |

	aspect := Smalltalk at: #Aspect.

	^ super publishedAspectsOfInstances
		add: (aspect choice: #jvmClass from: JVM allSubclasses);
		add: (aspect choice: #jniLibraryClass from: (JNILibrary withAllSubclasses reject: [:each | each isAbstract]));
		add: (aspect fileSave: #jniLibraryFilename);
		add: (aspect sequenceableCollection: #watcherClasses addFrom: JVMWatcher allSubclasses);
		yourself.
!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"
	(Smalltalk at: #JavaBaseConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'USE_ABORT_HOOK_MASK'				put: 16r0001;
		at: 'USE_EXIT_HOOK_MASK'				put: 16r0002;
		at: 'USE_VFPRINTF_HOOK_MASK'			put: 16r0004;
		at: 'USE_VFPRINTF_REDIRECT_MASK'			put: 16r0008;
		at: 'USE_JNI_HELPER_LIBRARY_MASK'		put: 16r0010;

		shrink.
!

stringAspectNames
	"answer an Array of the names of string aspects of instances"

	^ super stringAspectNames
		, #(
			#jniLibraryFilename
			#jniLibraryVersion
			#jniLibraryJVMType
		).!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #jniPortSettings.
! !
!JNIPortSettings class categoriesFor: #booleanAspectNames!constants!development!must strip!private! !
!JNIPortSettings class categoriesFor: #defaultFlags!constants!public! !
!JNIPortSettings class categoriesFor: #initialize!initializing!private! !
!JNIPortSettings class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!JNIPortSettings class categoriesFor: #rebuildPoolConstants!initializing!private! !
!JNIPortSettings class categoriesFor: #stringAspectNames!constants!development!must strip!private! !
!JNIPortSettings class categoriesFor: #uninitialize!initializing!private! !

SupplementaryClassloadersSettings guid: (GUID fromString: '{63741DA9-B7D3-42B3-9DC2-36E3EAD9F070}')!
SupplementaryClassloadersSettings comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these holds settings that are used for so-called ''supplementary classloaders -- see the class comment of SupplementaryClassloaderTree for more details.'!
!SupplementaryClassloadersSettings categoriesForClass!Unclassified! !
!SupplementaryClassloadersSettings methodsFor!

entries
	"answer the list of SupplementaryClassloaders that JVMs may define as they start up
	(and which can be modified thereafter).
	Note, this answers an OrderedCollection of SupplementaryClassloaders, not a SupplementaryClassloaderTree"

	^ entries ifNil: [OrderedCollection new].!

entries: aCollection
	"set the list of SupplementaryClassloaders that JVMs may define as they start up
	(and which can be modified thereafter).
	Note, this takes an OrderedCollection of SupplementaryClassloaders, not a SupplementaryClassloaderTree"

	entries:= aCollection.!

initialize
	"private -- establish a default initial state.
	NB: this is only used for creating a new 'default' (template) instance.  Subsequent
	instances are created by cloning that"

	super initialize.

	entries := OrderedCollection new.
!

loadOtherAspectsFromRegistryUnder: aRegKey
	"private -- load our non-standard state from the registry under the given key.
	Answer true iff we found an acceptable minumum of data"

	| count |

	count := aRegKey valueAt: 'entry count' ifAbsent: [0].
	entries := OrderedCollection new: count.
	1 to: count do:
		[:i || key subkey entry |
		key := 'entry %02d' sprintfWith: i.
		subkey := aRegKey at: key ifAbsent: [^ true].
		entry := SupplementaryClassloader newFromRegistryUnder: subkey ifNone: [^ true].
		entries addLast: entry].

	^ true.
!

saveOtherAspectsToRegistryUnder: aRegKey
	"private -- save our non-standard state to the registry under the given key"

	#CUtodo.  "we should remove the old ones"
	aRegKey valueAt: 'entry count' put: self entries size.
	self entries keysAndValuesDo:
		[:i :each || subkey |
		subkey := aRegKey createKey: ('entry %02d' sprintfWith: i).
		each saveToRegistryUnder: subkey].
!

useAtStartup
	"answer whether the JVM will set up any supplementary classloaders that we
	call for during its intialisation (if not then they can be initialised later explicitly)"

	^ self allFlagsSet: USE_AT_STARTUP.
!

useAtStartup: aBool
	"answer whether the JVM will set up any supplementary classloaders that we
	call for during its intialisation (if not then they can be initialised later explicitly)"

	self setFlags: USE_AT_STARTUP to: aBool.
! !
!SupplementaryClassloadersSettings categoriesFor: #entries!accessing!public! !
!SupplementaryClassloadersSettings categoriesFor: #entries:!accessing!public! !
!SupplementaryClassloadersSettings categoriesFor: #initialize!initializing!private! !
!SupplementaryClassloadersSettings categoriesFor: #loadOtherAspectsFromRegistryUnder:!private!registry! !
!SupplementaryClassloadersSettings categoriesFor: #saveOtherAspectsToRegistryUnder:!private!registry! !
!SupplementaryClassloadersSettings categoriesFor: #useAtStartup!accessing!public!testing! !
!SupplementaryClassloadersSettings categoriesFor: #useAtStartup:!accessing!public! !

!SupplementaryClassloadersSettings class methodsFor!

booleanAspectNames
	"private -- answer an Array of the names of boolean aspects of instances"

	^ super booleanAspectNames
		, #(
			#useAtStartup
		).
!

initialize
	"private -- class initialization.

		self initialize.
	"

	JVMSettings addToTemplate: self new name: #supplementaryClassloaders.


!

publishedAspectsOfInstances
	"answer a Collection of Aspects of our instances"

	| aspect |

	aspect := Smalltalk at: #Aspect.

	^ super publishedAspectsOfInstances
		add: (aspect sequenceableCollection: #entries addEvaluationFrom: #('SupplementaryClassloader new' 'SupplementaryClassloader fromCLASSPATH'));
		yourself.
!

rebuildPoolConstants
	"private -- rebuild the pool constants dictionary.

		self rebuildPoolConstants.
	"
	(Smalltalk at: #SupplementaryClassloaderConstants ifAbsentPut: [PoolConstantsDictionary new])

		at: 'USE_AT_STARTUP'				put: 16r0001;

		shrink.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	JVMSettings removeFromTemplate: #supplementaryClassloaders.
! !
!SupplementaryClassloadersSettings class categoriesFor: #booleanAspectNames!constants!development!must strip!private! !
!SupplementaryClassloadersSettings class categoriesFor: #initialize!initializing!private! !
!SupplementaryClassloadersSettings class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!SupplementaryClassloadersSettings class categoriesFor: #rebuildPoolConstants!initializing!private! !
!SupplementaryClassloadersSettings class categoriesFor: #uninitialize!initializing!private! !

JavaEqualitySearchPolicy guid: (GUID fromString: '{16B0A3A5-80C6-45BF-98A6-4D623F270A69}')!
JavaEqualitySearchPolicy comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Form of search policy that knows how to compare JavaInstances for equivalence as defined by the underlying Java object''s #equals()  and hashCode() methods.

There is no need for a similar JavaIdentitySearchPolicy since the default implementations of #= and #hash correspond to the underlying Java identities.  I.e. a normal EqualitySearchPolicy will serve as an IdentitySearchPolicy for Java objects.'!
!JavaEqualitySearchPolicy categoriesForClass!Unclassified! !
!JavaEqualitySearchPolicy methodsFor!

compare: aJavaInstance with: anotherJavaInstance
	"answer whether the two Java objects consider themselves to be the same as determined by
	their equals() Java method"

	^ aJavaInstance equals: anotherJavaInstance!

hash: aJavaInstance
	"answer the Java object underlying aJavaInstance's equality hash"

	^ aJavaInstance hashCode.! !
!JavaEqualitySearchPolicy categoriesFor: #compare:with:!comparing!public! !
!JavaEqualitySearchPolicy categoriesFor: #hash:!comparing!public! !

"Binary Globals"!

"Resources"!

