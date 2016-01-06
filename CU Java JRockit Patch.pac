| package |
package := Package name: 'CU Java JRockit Patch'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

This package contains patches to allow jniport to work over the BEA JRockit JVM.  You will have to save each of the loose methods under the name with ''PATCH_'' removed, thus overwriting the existing methods in the base package.

To automate the process, evaluate:

	StaticOrgMetagnosticJniportJRockitPatch installPatchedMethods.

The problem is that most of the introspection methods in java.lang.Class are broken in JRockit, which means that very little of JNIPort, except the basic JNI level, and applications of the Java base level that don''t use introspection, will work without the patch.

BTW, BEA are aware of the bug, and I hope it be fixed soon.  At which time I''ll deprecate this package.

If you are using this package then the corresponding Java code (shipped in the "Extras\JNIPort.jar" file) must be on the Java classpath somewhere (one way to ensure this is to add the file to the classpath in the JVM settings).   The Java source is in the corresponding zip file.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.00'.


package classNames
	add: #StaticOrgMetagnosticJniportJRockitPatch;
	yourself.

package methodNames
	add: #JavaLangClass -> #PATCH_declaredClasses;
	add: #JavaLangClass -> #PATCH_declaredConstructors;
	add: #JavaLangClass -> #PATCH_declaredFields;
	add: #JavaLangClass -> #PATCH_declaredMethods;
	add: #JavaLangClass -> #patchClass;
	add: #StaticOrgMetagnosticJniportDolphinNotifierThread -> #PATCH_notifierMethod;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU Java Callbacks';
	add: 'CU JNI';
	yourself).

package!

"Class Definitions"!

StaticJavaLangObject subclass: #StaticOrgMetagnosticJniportJRockitPatch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaLangClass methodsFor!

PATCH_declaredClasses
	"answer an Array of JavaLangClasses corresponding to the result of our Java getDeclaredClasses() method"

	"workaround for bug in BEA JRockit7"
	^ (self patchClass getDeclaredClasses_Class: self) collect: [:each | each classStatic].!

PATCH_declaredConstructors
	"answer an Array of JavaLangReflectConstructors corresponding to the result of our Java getDeclaredConstructors() method"

	"workaround for bug in BEA JRockit7"
	^ (self patchClass getDeclaredConstructors_Class: self) asCollection.!

PATCH_declaredFields
	"answer an Array of JavaLangReflectFields corresponding to the result of our Java getDeclaredFields() method"

	"workaround for bug in BEA JRockit7"
	^ (self patchClass getDeclaredFields_Class: self) asCollection.!

PATCH_declaredMethods
	"answer an Array of JavaLangReflectMethods corresponding to the result of our Java getDeclaredMethods() method"

	"workaround for bug in BEA JRockit7"
	^ (self patchClass getDeclaredMethods_Class: self) asCollection.!

patchClass
	"private -- answer the class static of the class that is used to patcharound bugs in JRockit7"

	^ self jvm findClass: #'org.metagnostic.jniport.JRockitPatch'.! !
!JavaLangClass categoriesFor: #PATCH_declaredClasses!patches!public!reflection! !
!JavaLangClass categoriesFor: #PATCH_declaredConstructors!patches!public!reflection! !
!JavaLangClass categoriesFor: #PATCH_declaredFields!patches!public!reflection! !
!JavaLangClass categoriesFor: #PATCH_declaredMethods!patches!public!reflection! !
!JavaLangClass categoriesFor: #patchClass!patches!private! !

!StaticOrgMetagnosticJniportDolphinNotifierThread methodsFor!

PATCH_notifierMethod
	"private -- answer our native 'notifier' method"

	"workaround for bug in BEA JRockit7"
	^ (self jvm findClass: #'org.metagnostic.jniport.JRockitPatch')
		getDeclaredMethod_Class: self
		String: 'dolphinNotifierMethod'
		ClassArray: nil.! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #PATCH_notifierMethod!Java native methods!patches!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StaticOrgMetagnosticJniportJRockitPatch guid: (GUID fromString: '{BB27C2A2-36CE-490C-BA4E-4CAFF81CADF8}')!
StaticOrgMetagnosticJniportJRockitPatch comment: 'Copyright © Chris Uppal, 2002.
chris.uppal@metagnostic.org

Instances reify the Java class org.metagnostic.jniport.JRockitPatch and provide access to its class-side methods which simply forward to the corresponding methods of java.lang.Class.  Thus avoiding the problem where calling some methods of java.lang.Class directly from JNI in a JRockit JVM fails.'!
!StaticOrgMetagnosticJniportJRockitPatch categoriesForClass!Unclassified! !
!StaticOrgMetagnosticJniportJRockitPatch methodsFor!

getClasses_Class: aClass1
	"answer the result of calling the receiver's public static getClasses(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getClasses' signature: '(Ljava/lang/Class;)[Ljava/lang/Class;' withArguments: args.
!

getConstructor_Class: aClass1 ClassArray: aClasses1
	"answer the result of calling the receiver's public static getConstructor(java.lang.Class, java.lang.Class[]) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getConstructor' signature: '(Ljava/lang/Class;[Ljava/lang/Class;)Ljava/lang/reflect/Constructor;' withArguments: args.
!

getConstructors_Class: aClass1
	"answer the result of calling the receiver's public static getConstructors(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getConstructors' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Constructor;' withArguments: args.
!

getDeclaredClasses_Class: aClass1
	"answer the result of calling the receiver's public static getDeclaredClasses(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getDeclaredClasses' signature: '(Ljava/lang/Class;)[Ljava/lang/Class;' withArguments: args.
!

getDeclaredConstructor_Class: aClass1 ClassArray: aClasses1
	"answer the result of calling the receiver's public static getDeclaredConstructor(java.lang.Class, java.lang.Class[]) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getDeclaredConstructor' signature: '(Ljava/lang/Class;[Ljava/lang/Class;)Ljava/lang/reflect/Constructor;' withArguments: args.
!

getDeclaredConstructors_Class: aClass1
	"answer the result of calling the receiver's public static getDeclaredConstructors(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getDeclaredConstructors' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Constructor;' withArguments: args.
!

getDeclaredField_Class: aClass1 String: aString1
	"answer the result of calling the receiver's public static getDeclaredField(java.lang.Class, java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getDeclaredField' signature: '(Ljava/lang/Class;Ljava/lang/String;)Ljava/lang/reflect/Field;' withArguments: args.
!

getDeclaredFields_Class: aClass1
	"answer the result of calling the receiver's public static getDeclaredFields(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getDeclaredFields' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Field;' withArguments: args.
!

getDeclaredMethod_Class: aClass1 String: aString1 ClassArray: aClasses1
	"answer the result of calling the receiver's public static getDeclaredMethod(java.lang.Class, java.lang.String, java.lang.Class[]) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aString1Ref;
			objectAt: 3 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getDeclaredMethod' signature: '(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;' withArguments: args.
!

getDeclaredMethods_Class: aClass1
	"answer the result of calling the receiver's public static getDeclaredMethods(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getDeclaredMethods' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Method;' withArguments: args.
!

getField_Class: aClass1 String: aString1
	"answer the result of calling the receiver's public static getField(java.lang.Class, java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getField' signature: '(Ljava/lang/Class;Ljava/lang/String;)Ljava/lang/reflect/Field;' withArguments: args.
!

getFields_Class: aClass1
	"answer the result of calling the receiver's public static getFields(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getFields' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Field;' withArguments: args.
!

getMethod_Class: aClass1 String: aString1 ClassArray: aClasses1
	"answer the result of calling the receiver's public static getMethod(java.lang.Class, java.lang.String, java.lang.Class[]) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aClass1;
			objectAt: 2 put: aString1Ref;
			objectAt: 3 put: aClasses1;
			yourself.

	^ self callObjectMethod: 'getMethod' signature: '(Ljava/lang/Class;Ljava/lang/String;[Ljava/lang/Class;)Ljava/lang/reflect/Method;' withArguments: args.
!

getMethods_Class: aClass1
	"answer the result of calling the receiver's public static getMethods(java.lang.Class) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aClass1;
			yourself.

	^ self callObjectMethod: 'getMethods' signature: '(Ljava/lang/Class;)[Ljava/lang/reflect/Method;' withArguments: args.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getClasses_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getConstructor_Class:ClassArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getConstructors_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredClasses_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredConstructor_Class:ClassArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredConstructors_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredField_Class:String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredFields_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredMethod_Class:String:ClassArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getDeclaredMethods_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getField_Class:String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getFields_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getMethod_Class:String:ClassArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #getMethods_Class:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportJRockitPatch categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !

!StaticOrgMetagnosticJniportJRockitPatch class methodsFor!

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
		#getClasses_Class:
		#getConstructor_Class:ClassArray:
		#getConstructors_Class:
		#getDeclaredClasses_Class:
		#getDeclaredConstructor_Class:ClassArray:
		#getDeclaredConstructors_Class:
		#getDeclaredField_Class:String:
		#getDeclaredFields_Class:
		#getDeclaredMethod_Class:String:ClassArray:
		#getDeclaredMethods_Class:
		#getField_Class:String:
		#getFields_Class:
		#getMethod_Class:String:ClassArray:
		#getMethods_Class:
	).
!

installPatchedMethod: aMethod
	"private -- install the a patched method from our package"

	| selector class source original |

	selector := aMethod selector.
	class := aMethod methodClass.
	source := aMethod getSource.

	(selector beginsWith: 'PATCH_') ifFalse: [^ self].
	selector := (selector allButFirst: 6) asSymbol.
	source := source allButFirst: 6.

	original := class compiledMethodAt: selector ifAbsent: [^ self].

	class
		compile: ('ORIG_' , original getSource)
		categories: original categories
		package: original owningPackage.

	class
		compile: source
		categories: original categories
		package: original owningPackage.

!

installPatchedMethods
	"private -- install the patched methods from our package"

	self owningPackage methods do: [:each | self installPatchedMethod: each].!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.JRockitPatch'.
! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #installPatchedMethod:!patching!private! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #installPatchedMethods!patching!private! !
!StaticOrgMetagnosticJniportJRockitPatch class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

"Binary Globals"!

"Resources"!

