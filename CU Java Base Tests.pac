| package |
package := Package name: 'CU Java Base Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Some regression tests for the basic level of JNIPort

If you are using this package then the corresponding Java code (shipped in the "Extras\JNIPort-Tests.jar" file) must be on the Java classpath somewhere (one way to ensure this is to add the file to the classpath in the JVM settings). The Java source is in the corresponding zip file.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.03'.

package basicScriptAt: #postinstall put: '(Package manager packageNamed: ''CU Java Base Tests'')
	propertyAt: #ExternalResourceFileNames
	put: #(
		''Extras\JNIPort-Tests.jar''
		''Extras\JNIPort-Tests.zip''
	).
!!
'.

package classNames
	add: #GenericJNIPortRegressionTest;
	add: #JNIPortArrayOfBooleansTest;
	add: #JNIPortArrayOfBytesTest;
	add: #JNIPortArrayOfCharsTest;
	add: #JNIPortArrayOfDoublesTest;
	add: #JNIPortArrayOfFloatsTest;
	add: #JNIPortArrayOfIntegerTest;
	add: #JNIPortArrayOfIntsTest;
	add: #JNIPortArrayOfLongsTest;
	add: #JNIPortArrayOfObjectsTest;
	add: #JNIPortArrayOfPointsTest;
	add: #JNIPortArrayOfShortsTest;
	add: #JNIPortArrayOfStringsTest;
	add: #JNIPortArrayTest;
	add: #JNIPortBasicConstructorsTest;
	add: #JNIPortBasicFieldAccessTest;
	add: #JNIPortBasicFieldsTest;
	add: #JNIPortBasicHiddenFieldsTest;
	add: #JNIPortBasicHiddenMethodsTest;
	add: #JNIPortBasicInheritedFieldsTest;
	add: #JNIPortBasicInheritedMethodsTest;
	add: #JNIPortBasicInhertedInterfaceFieldsTest;
	add: #JNIPortBasicInstanceFieldsTest;
	add: #JNIPortBasicInstanceMethodArgumentsTest;
	add: #JNIPortBasicInstanceMethodReturnsTest;
	add: #JNIPortBasicInterfaceFieldsTest;
	add: #JNIPortBasicMethodAccessTest;
	add: #JNIPortBasicMethodArgumentsTest;
	add: #JNIPortBasicMethodReturnsTest;
	add: #JNIPortBasicStaticFieldsTest;
	add: #JNIPortBasicStaticMethodArgumentsTest;
	add: #JNIPortBasicStaticMethodReturnsTest;
	add: #JNIPortClassLookupTest;
	add: #JNIPortComparisonTest;
	add: #JNIPortExceptionTest;
	add: #JNIPortStringTest;
	add: #StaticOMJTRStaticMethods;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Additional Wrappers';
	add: 'CU Java Base';
	add: 'CU JNI';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

StaticJavaLangObject subclass: #StaticOMJTRStaticMethods
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #GenericJNIPortRegressionTest
	instanceVariableNames: 'jvm'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicConstructorsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicFieldAccessTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicFieldsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicHiddenFieldsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicHiddenMethodsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicInheritedFieldsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicInheritedMethodsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicInhertedInterfaceFieldsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicInterfaceFieldsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicMethodAccessTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicMethodArgumentsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortBasicMethodReturnsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortClassLookupTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortComparisonTest
	instanceVariableNames: 'string1 string2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortExceptionTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortStringTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfBooleansTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfCharsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfDoublesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfFloatsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfIntegerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfObjectsTest
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfPointsTest
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayTest subclass: #JNIPortArrayOfStringsTest
	instanceVariableNames: 'elements'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayOfIntegerTest subclass: #JNIPortArrayOfBytesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayOfIntegerTest subclass: #JNIPortArrayOfIntsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayOfIntegerTest subclass: #JNIPortArrayOfLongsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortArrayOfIntegerTest subclass: #JNIPortArrayOfShortsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicFieldsTest subclass: #JNIPortBasicInstanceFieldsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicFieldsTest subclass: #JNIPortBasicStaticFieldsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicMethodArgumentsTest subclass: #JNIPortBasicInstanceMethodArgumentsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicMethodArgumentsTest subclass: #JNIPortBasicStaticMethodArgumentsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicMethodReturnsTest subclass: #JNIPortBasicInstanceMethodReturnsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortBasicMethodReturnsTest subclass: #JNIPortBasicStaticMethodReturnsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StaticOMJTRStaticMethods guid: (GUID fromString: '{255EA03C-0931-4F98-811C-98642A1916E4}')!
StaticOMJTRStaticMethods comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!StaticOMJTRStaticMethods categoriesForClass!Unclassified! !
!StaticOMJTRStaticMethods methodsFor!

booleanMethod_null
	"answer the result of calling the receiver's public static booleanMethod() Java method"

	^ self callBooleanMethod: 'booleanMethod'.
!

byteMethod_null
	"answer the result of calling the receiver's public static byteMethod() Java method"

	^ self callByteMethod: 'byteMethod'.
!

charMethod_null
	"answer the result of calling the receiver's public static charMethod() Java method"

	^ self callCharMethod: 'charMethod'.
!

doubleMethod_null
	"answer the result of calling the receiver's public static doubleMethod() Java method"

	^ self callDoubleMethod: 'doubleMethod'.
!

floatMethod_null
	"answer the result of calling the receiver's public static floatMethod() Java method"

	^ self callFloatMethod: 'floatMethod'.
!

intMethod_null
	"answer the result of calling the receiver's public static intMethod() Java method"

	^ self callIntMethod: 'intMethod'.
!

longMethod_null
	"answer the result of calling the receiver's public static longMethod() Java method"

	^ self callLongMethod: 'longMethod'.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

nullMethod_null
	"answer the result of calling the receiver's public static nullMethod() Java method"

	^ self callObjectMethod: 'nullMethod' signature: '()Ljava/lang/Object;'.
!

pointMethod_null
	"answer the result of calling the receiver's public static pointMethod() Java method"

	^ self callObjectMethod: 'pointMethod' signature: '()Ljava/awt/Point;'.
!

shortMethod_null
	"answer the result of calling the receiver's public static shortMethod() Java method"

	^ self callShortMethod: 'shortMethod'.
!

stringMethod_null
	"answer the result of calling the receiver's public static stringMethod() Java method"

	^ self callObjectMethod: 'stringMethod' signature: '()Ljava/lang/String;'.
!

voidMethod_null
	"invoke the receiver's public static voidMethod() Java method"

	self callVoidMethod: 'voidMethod'.
! !
!StaticOMJTRStaticMethods categoriesFor: #booleanMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #byteMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #charMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #doubleMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #floatMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #intMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #longMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticOMJTRStaticMethods categoriesFor: #nullMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #pointMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #shortMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #stringMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRStaticMethods categoriesFor: #voidMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticOMJTRStaticMethods class methodsFor!

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
		#booleanMethod_null
		#byteMethod_null
		#charMethod_null
		#doubleMethod_null
		#floatMethod_null
		#intMethod_null
		#longMethod_null
		#nullMethod_null
		#pointMethod_null
		#shortMethod_null
		#stringMethod_null
		#voidMethod_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.StaticMethods'.
! !
!StaticOMJTRStaticMethods class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRStaticMethods class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRStaticMethods class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRStaticMethods class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRStaticMethods class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

GenericJNIPortRegressionTest guid: (GUID fromString: '{30EDEA01-0275-485B-BE9F-69968335B155}')!
GenericJNIPortRegressionTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!GenericJNIPortRegressionTest categoriesForClass!Unclassified! !
!GenericJNIPortRegressionTest methodsFor!

arrayStoreExceptionClass

	^ jvm findClass: #'java.lang.ArrayStoreException'.!

instantiationExceptionClass

	^ jvm findClass: #'java.lang.InstantiationException'.!

javaTestClass

	^ self javaTestClass: self javaTestClassName.!

javaTestClass: aString

	^ jvm findClass: (self javaTestClassName: aString).!

javaTestClassName

	self subclassResponsibility.!

javaTestClassName: aString

	^ self javaTestClassRoot , '.' , aString.!

javaTestClassRoot

	^ 'org.metagnostic.jniport.test.regression'.!

makeOverloadedByTypeClass
	"create and answer a class that overloads members by type.  This is impossible
	in Java so we generate the bytecodes directly"

	| bytes |

	bytes := self makeOverloadedByTypeClassBytes ifNil: [^ nil].
	^ jvm
		defineClass: self overloadedByTypeClassJNIName
		fromBytes: bytes
		classloader: jvm systemClassloader.!

makeOverloadedByTypeClassBytes
	"create and answer a ByteArray classfile that overloads members by type"

	| factory classfile |

	#CUtodo.  "write this out to a classfile so we can distribute this test case too"
	factory := Smalltalk at: #JVMClassfile ifAbsent: [^ nil].

	classfile := (factory name: self overloadedByTypeClassJNIName)
		bePublic;
		yourself.

	"set up a public static int-valued field"
	(classfile fieldName: 'field' type: 'I')
		bePublic;
		beStatic;
		intValue: 33.

	"and a public static boolean-valued field of the same name"
	(classfile fieldName: 'field' type: 'Z')
		bePublic;
		beStatic;
		booleanValue: false.

	"set up a public int-valued method"
	(classfile methodName: 'method' signature: '()I')
		bePublic;
		startBytecodes;
			returnInt: 22;
		endBytecodes.

	"add a public double-valued method of the same name"
	(classfile methodName: 'method' signature: '()D')
		bePublic;
		startBytecodes;
			returnDouble: 10.0;
		endBytecodes.

	"not needed for testing JNIPort, but just nice to check up on the JVM"
	(classfile methodName: 'checkAmbiguousIntField' signature: '()I')
		bePublic; beStatic;
		startBytecodes;
			getStaticNamed: 'ambiguousIntField' type: 'I' class: 'org/metagnostic/jniport/test/regression/HidingFields';
			returnInt;
		endBytecodes.

	classfile generateDefaultConstructor bePublic.

	classfile close.

	^ classfile getBytes.!

noSuchClassExceptionClass

	^ jvm findClass: #'java.lang.NoClassDefFoundError'.!

noSuchFieldExceptionClass

	^ jvm findClass: #'java.lang.NoSuchFieldError'.
!

noSuchMethodExceptionClass

	^ jvm findClass: #'java.lang.NoSuchMethodError'.
!

nullPointerExceptionClass

	^ jvm findClass: #'java.lang.NullPointerException'.!

overloadedByTypeClass

	^ [self javaTestClass: self overloadedByTypeClassName]
		on: self noSuchClassExceptionClass
		do: [:ex | ^ self makeOverloadedByTypeClass].!

overloadedByTypeClassJNIName

	^ (self javaTestClassName: self overloadedByTypeClassName) asJNIClassName.
!

overloadedByTypeClassName

	^ 'GeneratedTemporaryClass'.!

setUp

	super setUp.
	jvm := JVM current.! !
!GenericJNIPortRegressionTest categoriesFor: #arrayStoreExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #instantiationExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #javaTestClass!helpers!public! !
!GenericJNIPortRegressionTest categoriesFor: #javaTestClass:!helpers!public! !
!GenericJNIPortRegressionTest categoriesFor: #javaTestClassName!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #javaTestClassName:!helpers!public! !
!GenericJNIPortRegressionTest categoriesFor: #javaTestClassRoot!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #makeOverloadedByTypeClass!helpers!public! !
!GenericJNIPortRegressionTest categoriesFor: #makeOverloadedByTypeClassBytes!helpers!public! !
!GenericJNIPortRegressionTest categoriesFor: #noSuchClassExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #noSuchFieldExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #noSuchMethodExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #nullPointerExceptionClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #overloadedByTypeClass!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #overloadedByTypeClassJNIName!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #overloadedByTypeClassName!constants!public! !
!GenericJNIPortRegressionTest categoriesFor: #setUp!public!running! !

!GenericJNIPortRegressionTest class methodsFor!

isAbstract

	^self = ##(self).! !
!GenericJNIPortRegressionTest class categoriesFor: #isAbstract!public!testing! !

JNIPortArrayTest guid: (GUID fromString: '{2E17FCF2-576D-43FD-9DAA-CE882324622B}')!
JNIPortArrayTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayTest categoriesForClass!Unclassified! !
!JNIPortArrayTest methodsFor!

arrayClass

	^ jvm findClass: self arrayClassName.!

arrayClassName

	^ (self elementClassName , '[]') asSymbol.!

elementAt: anInteger

	^ self elements at: anInteger.!

elementClass

	^ jvm findClass: self elementClassName.!

elementClassName

	self subclassResponsibility.!

elementCount

	^ self elements size.!

elements

	self subclassResponsibility.!

testArrayBoundsCheck

	| array slice count |

	count := self elementCount.
	array := self elementClass newArray: count.

	"just in case"
	self should: [count >= 2].

	self should: [array at: -1] raise: BoundsError.
	self should: [array at: 0] raise: BoundsError.
	self shouldnt: [array at: 1] raise: BoundsError.
	self shouldnt: [array at: count] raise: BoundsError.
	self should: [array at: count+1] raise: BoundsError.

	slice := OrderedCollection new.
	self shouldnt: [array from: 2 to: count - 1 keysAndValuesDo: [:i :each | slice addLast: each]] raise: BoundsError.
	self should: [slice size + 2 = count].

	slice := OrderedCollection new.
	self should: [array from: 2 to: count + 2 keysAndValuesDo: [:i :each | slice addLast: each]] raise: BoundsError.
	self should: [slice size + 1 = count].	"should have looped over the valid indices before throwing"

	slice := OrderedCollection new.
	self should: [array from: -2 to: 2 keysAndValuesDo: [:i :each | slice addLast: each]] raise: BoundsError.
	self should: [slice size = 0].
!

testCreateArray

	| array |

	array := self elementClass newArray: self elementCount.

	self shouldnt: [array isNil].
	self should: [array static == self arrayClass].
	self should: [array size = self elementCount].
	self should: [array at: 0] raise: BoundsError.
	self should: [array at: self elementCount + 1] raise: BoundsError.
!

testFillArray

	| array |

	array := self elementClass newArray: self elementCount.

	1 to: self elementCount do: [:i | array at: i put: (self elementAt: i)].

	1 to: self elementCount do: [:i | self should: [(array at: i) = (self elementAt: i)]].
!

testIterateArray

	| array |

	array := self elementClass newArray: self elementCount.

	array
		replaceFrom: 1
		to: self elementCount
		with: self elements.

	array asCollection keysAndValuesDo: [:i :each | self should: [each = (self elementAt: i)]].
!

testZeroSizedArray1

	| array |

	array := self elementClass newArray: 0.

	self shouldnt: [array isNil].
	self should: [array static == self arrayClass].
	self should: [array size = 0].
!

testZeroSizedArray2

	| array |

	array := self arrayClass new: 0.

	self shouldnt: [array isNil].
	self should: [array static == self arrayClass].
	self should: [array size = 0].
! !
!JNIPortArrayTest categoriesFor: #arrayClass!accessing!public! !
!JNIPortArrayTest categoriesFor: #arrayClassName!accessing!public! !
!JNIPortArrayTest categoriesFor: #elementAt:!accessing!public! !
!JNIPortArrayTest categoriesFor: #elementClass!accessing!public! !
!JNIPortArrayTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayTest categoriesFor: #elementCount!accessing!public! !
!JNIPortArrayTest categoriesFor: #elements!accessing!public! !
!JNIPortArrayTest categoriesFor: #testArrayBoundsCheck!public!unit tests! !
!JNIPortArrayTest categoriesFor: #testCreateArray!public!unit tests! !
!JNIPortArrayTest categoriesFor: #testFillArray!public!unit tests! !
!JNIPortArrayTest categoriesFor: #testIterateArray!public!unit tests! !
!JNIPortArrayTest categoriesFor: #testZeroSizedArray1!public!unit tests! !
!JNIPortArrayTest categoriesFor: #testZeroSizedArray2!public!unit tests! !

!JNIPortArrayTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortArrayTest class categoriesFor: #isAbstract!public!testing! !

JNIPortBasicConstructorsTest guid: (GUID fromString: '{9415839D-6577-4074-9855-6AD9BAEF0A53}')!
JNIPortBasicConstructorsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicConstructorsTest categoriesForClass!Unclassified! !
!JNIPortBasicConstructorsTest methodsFor!

abstractClass

	^ self javaTestClass: 'AbstractClass'.!

derivedClass

	^ self javaTestClass: 'DerivedConstructors'.!

interface

	^ self javaTestClass: 'InheritedMethodsInterface'.!

javaTestClassName

	^ 'Constructors'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
!

tearDown

	classStatic := nil.
	super tearDown.
!

testBooleanArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(Z)V'.

	args booleanAt: 1 put: true.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'boolean: true'].

	args booleanAt: 1 put: false.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'boolean: false'].!

testByteArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(B)V'.

	args byteAt: 1 put: 0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'byte: 0'].

	args byteAt: 1 put: 100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'byte: 100'].

	args byteAt: 1 put: -100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'byte: -100'].
!

testCharArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(C)V'.

	args charAt: 1 put: $!!.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'char: !!'].

	args charAt: 1 put: $A.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'char: A'].!

testDerivedClassConstructor

	| new |

	new := self derivedClass callConstructor.
	self should: [(new callStringMethod: 'getText') asString = 'String: derived'].!

testDoubleArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(D)V'.

	args doubleAt: 1 put: 0.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: 0.0'].

	args doubleAt: 1 put: 100.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: 100.0'].

	args doubleAt: 1 put: -100.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: -100.0'].

	args doubleAt: 1 put: 1000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: 1000.0'].

	args doubleAt: 1 put: -1000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: -1000.0'].

	args doubleAt: 1 put: 1000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: 1000000.0'].

	args doubleAt: 1 put: -1000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: -1000000.0'].

	args doubleAt: 1 put: 1000000000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: 1.0E12'].		"JVM specific ??"

	args doubleAt: 1 put: -1000000000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'double: -1.0E12'].		"JVM specific ??"!

testFloatArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(F)V'.

	args floatAt: 1 put: 0.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: 0.0'].

	args floatAt: 1 put: 100.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: 100.0'].

	args floatAt: 1 put: -100.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: -100.0'].

	args floatAt: 1 put: 1000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: 1000.0'].

	args floatAt: 1 put: -1000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: -1000.0'].

	args floatAt: 1 put: 1000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: 1000000.0'].

	args floatAt: 1 put: -1000000.0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'float: -1000000.0'].!

testInheritedConstructor

	| args sig |

	"JRockit wrongly calls the superclass ctor"

	args := JNIValueArray new: 1.
	sig := '(Z)V'.
	self
		should: [self derivedClass callConstructorSignature: sig withArguments: args]
		raise: self noSuchMethodExceptionClass.
!

testInstantiateAbstractClass

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(I)V'.
	args intAt: 1 put: 1.

	self
		should: [self abstractClass callConstructorSignature: sig withArguments: args]
		raise: self instantiationExceptionClass.!

testInstantiateAbstractClassDirectly

	self
		should: [self abstractClass new]
		raise: self instantiationExceptionClass.!

testInstantiateAbstractClassWithDefaultConstructor

	self
		should: [self abstractClass callConstructor]
		raise: self instantiationExceptionClass.!

testInstantiateConcreteClass

	| new |

	self
		shouldnt: [new := classStatic callConstructor]
		raise: self instantiationExceptionClass.

	self should: [new static == classStatic].
	self should: [new isInstanceOf: classStatic].
	self should: [(new callStringMethod: 'getText') asString = 'default'].
!

testInstantiateConcreteClassDirectly

	| new |

	self
		shouldnt: [new := classStatic new]
		raise: self instantiationExceptionClass.

	self should: [new static == classStatic].
	self should: [new isInstanceOf: classStatic].
	self should: [(new callStringMethod: 'getText') asString = 'default'].
!

testInstantiateInterface

	self
		should: [self interface callConstructor]
		raise: MessageNotUnderstood.!

testInstantiateInterfaceDirectly

	self
		should: [self interface new]
		raise: MessageNotUnderstood.!

testIntArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(I)V'.

	args intAt: 1 put: 0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: 0'].

	args intAt: 1 put: 100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: 100'].

	args intAt: 1 put: -100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: -100'].

	args intAt: 1 put: 1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: 1000'].

	args intAt: 1 put: -1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: -1000'].

	args intAt: 1 put: 1000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: 1000000'].

	args intAt: 1 put: -1000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: -1000000'].!

testLongArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(J)V'.

	args longAt: 1 put: 0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: 0'].

	args longAt: 1 put: 100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: 100'].

	args longAt: 1 put: -100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: -100'].

	args longAt: 1 put: 1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: 1000'].

	args longAt: 1 put: -1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: -1000'].

	args longAt: 1 put: 1000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: 1000000'].

	args longAt: 1 put: -1000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: -1000000'].

	args longAt: 1 put: 1000000000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: 1000000000000'].

	args longAt: 1 put: -1000000000000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'long: -1000000000000'].!

testNoArguments

	| new |

	new := classStatic callConstructor.
	self should: [(new callStringMethod: 'getText') asString = 'default'].
!

testNullArgument

	| args sig new |

	sig := '(Ljava/lang/String;)V'.
	args := JNIValueArray new: 1.

	args objectAt: 1 put: nil.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'String: null'].
!

testOutOfRangeValues

	| args |

	"we only test the JNIValueArray, since that'll fail before it reaches the JVM (and the
	JVM won't fail it, so there's no point in testing that)"
	args := JNIValueArray new: 1.

	self should: [args byteAt: 1 put: 200] raise: Error.
	self should: [args byteAt: 1 put: 1000] raise: Error.
	self should: [args shortAt: 1 put: 1000000] raise: Error.
	self should: [args intAt: 1 put: 6000000000] raise: Error.
	self should: [args longAt: 1 put: 100 factorial] raise: Error.
!

testPointArgument

	| jap p3x4 args sig new |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new.
	p3x4 setIntField: 'x' to: 3.
	p3x4 setIntField: 'y' to: 4.

	sig := '(Ljava/awt/Point;)V'.
	args := JNIValueArray new: 1.

	args objectAt: 1 put: p3x4.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'Point: java.awt.Point[x=3,y=4]'].		"JVM specific ??"!

testShortArgument

	| args sig new |

	args := JNIValueArray new: 1.
	sig := '(S)V'.

	args shortAt: 1 put: 0.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'short: 0'].

	args shortAt: 1 put: 100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'short: 100'].

	args shortAt: 1 put: -100.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'short: -100'].

	args shortAt: 1 put: 1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'short: 1000'].

	args shortAt: 1 put: -1000.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'short: -1000'].!

testStringArgument

	| args sig string new |

	string := 'Hi there!!' asJavaString: jvm.	"need to hold this in an instvar"
	sig := '(Ljava/lang/String;)V'.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: string;
			yourself.

	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'String: Hi there!!'].!

testTwoArguments

	| args sig new |

	args := JNIValueArray new: 2.
	sig := '(IF)V'.

	args intAt: 1 put: 7.
	args floatAt: 2 put: 1.5.
	new := classStatic callConstructorSignature: sig withArguments: args.
	self should: [(new callStringMethod: 'getText') asString = 'int: 7 float: 1.5'].! !
!JNIPortBasicConstructorsTest categoriesFor: #abstractClass!constants!public! !
!JNIPortBasicConstructorsTest categoriesFor: #derivedClass!constants!public! !
!JNIPortBasicConstructorsTest categoriesFor: #interface!constants!public! !
!JNIPortBasicConstructorsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicConstructorsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicConstructorsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicConstructorsTest categoriesFor: #testBooleanArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testByteArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testCharArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testDerivedClassConstructor!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testDoubleArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testFloatArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInheritedConstructor!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateAbstractClass!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateAbstractClassDirectly!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateAbstractClassWithDefaultConstructor!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateConcreteClass!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateConcreteClassDirectly!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateInterface!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testInstantiateInterfaceDirectly!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testIntArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testLongArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testNoArguments!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testNullArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testPointArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testShortArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testStringArgument!public!unit tests! !
!JNIPortBasicConstructorsTest categoriesFor: #testTwoArguments!public!unit tests! !

JNIPortBasicFieldAccessTest guid: (GUID fromString: '{386CFF01-F998-44AD-921F-4A177EA06246}')!
JNIPortBasicFieldAccessTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Note, we don''t test writing of "final" fields, ''cos it works in every case (i.e. JNI lets us do it) but it means nothing, so there''s
no point in testing it.'!
!JNIPortBasicFieldAccessTest categoriesForClass!Unclassified! !
!JNIPortBasicFieldAccessTest methodsFor!

javaTestClassName

	^ 'FieldAccess'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testReadDefaultClassField

	self should: [(classStatic getIntField: 'defaultStaticIntField') = 103].!

testReadDefaultFinalClassField

	self should: [(classStatic getIntField: 'defaultStaticFinalIntField') = 113].!

testReadDefaultFinalInstanceField

	self should: [(instance getIntField: 'defaultFinalIntField') = 13].!

testReadDefaultInstanceField

	self should: [(instance getIntField: 'defaultIntField') = 3].!

testReadPrivateClassField

	self should: [(classStatic getIntField: 'privateStaticIntField') = 104].!

testReadPrivateFinalClassField

	self should: [(classStatic getIntField: 'privateStaticFinalIntField') = 114].!

testReadPrivateFinalInstanceField

	self should: [(instance getIntField: 'privateFinalIntField') = 14].!

testReadPrivateInstanceField

	self should: [(instance getIntField: 'privateIntField') = 4].!

testReadProtectedClassField

	self should: [(classStatic getIntField: 'protectedStaticIntField') = 102].!

testReadProtectedFinalClassField

	self should: [(classStatic getIntField: 'protectedStaticFinalIntField') = 112].!

testReadProtectedFinalInstanceField

	self should: [(instance getIntField: 'protectedFinalIntField') = 12].!

testReadProtectedInstanceField

	self should: [(instance getIntField: 'protectedIntField') = 2].!

testReadPublicClassField

	self should: [(classStatic getIntField: 'publicStaticIntField') = 101].!

testReadPublicFinalClassField

	self should: [(classStatic getIntField: 'publicStaticFinalIntField') = 111].!

testReadPublicFinalInstanceField

	self should: [(instance getIntField: 'publicFinalIntField') = 11].!

testReadPublicInstanceField

	self should: [(instance getIntField: 'publicIntField') = 1].!

testWriteDefaultClassField

	classStatic setIntField: 'defaultStaticIntField' to: 0.
	self should: [(classStatic getIntField: 'defaultStaticIntField') = 0].
	classStatic setIntField: 'defaultStaticIntField' to: 103.
	self should: [(classStatic getIntField: 'defaultStaticIntField') = 103].
!

testWriteDefaultInstanceField

	self should: [(instance getIntField: 'defaultIntField') = 3].
	instance setIntField: 'defaultIntField' to: 0.
	self should: [(instance getIntField: 'defaultIntField') = 0].
!

testWritePrivateClassField

	classStatic setIntField: 'privateStaticIntField' to: 0.
	self should: [(classStatic getIntField: 'privateStaticIntField') = 0].
	classStatic setIntField: 'privateStaticIntField' to: 104.
	self should: [(classStatic getIntField: 'privateStaticIntField') = 104].
!

testWritePrivateInstanceField

	self should: [(instance getIntField: 'privateIntField') = 4].
	instance setIntField: 'privateIntField' to: 0.
	self should: [(instance getIntField: 'privateIntField') = 0].
!

testWriteProtectedClassField

	classStatic setIntField: 'protectedStaticIntField' to: 0.
	self should: [(classStatic getIntField: 'protectedStaticIntField') = 0].
	classStatic setIntField: 'protectedStaticIntField' to: 102.
	self should: [(classStatic getIntField: 'protectedStaticIntField') = 102].
!

testWriteProtectedInstanceField

	self should: [(instance getIntField: 'protectedIntField') = 2].
	instance setIntField: 'protectedIntField' to: 0.
	self should: [(instance getIntField: 'protectedIntField') = 0].
!

testWritePublicClassField

	classStatic setIntField: 'publicStaticIntField' to: 0.
	self should: [(classStatic getIntField: 'publicStaticIntField') = 0].
	classStatic setIntField: 'publicStaticIntField' to: 101.
	self should: [(classStatic getIntField: 'publicStaticIntField') = 101].
!

testWritePublicInstanceField

	self should: [(instance getIntField: 'publicIntField') = 1].
	instance setIntField: 'publicIntField' to: 0.
	self should: [(instance getIntField: 'publicIntField') = 0].
! !
!JNIPortBasicFieldAccessTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicFieldAccessTest categoriesFor: #setUp!public!running! !
!JNIPortBasicFieldAccessTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadDefaultClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadDefaultFinalClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadDefaultFinalInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadDefaultInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPrivateClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPrivateFinalClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPrivateFinalInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPrivateInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadProtectedClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadProtectedFinalClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadProtectedFinalInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadProtectedInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPublicClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPublicFinalClassField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPublicFinalInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testReadPublicInstanceField!public!reading!unit tests! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWriteDefaultClassField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWriteDefaultInstanceField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWritePrivateClassField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWritePrivateInstanceField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWriteProtectedClassField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWriteProtectedInstanceField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWritePublicClassField!public!unit tests!writing! !
!JNIPortBasicFieldAccessTest categoriesFor: #testWritePublicInstanceField!public!unit tests!writing! !

JNIPortBasicFieldsTest guid: (GUID fromString: '{A5D89B8B-F003-43C3-9EC7-E36845A37AAB}')!
JNIPortBasicFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicFieldsTest methodsFor!

dontTestTypeErrors

	"unfortunately we can't use this test since the JVM doesn't actually check the types
	for fields written via JNI"

	| jap |

	jap := jvm findClass: #'java.awt.Point'.
	self
		should: [subject setObjectField: 'stringField' signature: 'Ljava/lang/String;' to: jap new]
		raise: Error.
	self should: [(subject getStringField: 'stringField') asString = 'Hi there'].!

setUp

	super setUp.
	subject := self subject.!

subject

	self subclassResponsibility.!

tearDown

	subject := nil.
	super tearDown.
!

testAccessBooleanField

	self should: [(subject getBooleanField: 'booleanField') = true].

	subject setBooleanField: 'booleanField' to: false.
	self should: [(subject getBooleanField: 'booleanField') = false].
	self should: [subject callBooleanMethod: 'isBooleanFalse'].

	subject setBooleanField: 'booleanField' to: true.
	self should: [(subject getBooleanField: 'booleanField') = true].
	self should: [subject callBooleanMethod: 'isBooleanTrue'].
!

testAccessByteField

	self should: [(subject getByteField: 'byteField') = 22].

	subject setByteField: 'byteField' to: 0.
	self should: [(subject getByteField: 'byteField') = 0].
	self should: [subject callBooleanMethod: 'isByteZero'].

	subject setByteField: 'byteField' to: 33.
	self should: [(subject getByteField: 'byteField') = 33].
	self should: [subject callBooleanMethod: 'isByte33'].

	subject setByteField: 'byteField' to: -33.
	self should: [(subject getByteField: 'byteField') = -33].
	self should: [subject callBooleanMethod: 'isByteMinus33'].

	subject setByteField: 'byteField' to: 22.
	self should: [(subject getByteField: 'byteField') = 22].
!

testAccessCharField

	self should: [(subject getCharField: 'charField') = $!!].

	subject setCharField: 'charField' to: $M.
	self should: [(subject getCharField: 'charField') = $M].
	self should: [subject callBooleanMethod: 'isCharM'].

	subject setCharField: 'charField' to: 0.
	self should: [(subject getCharField: 'charField') = (Character codePoint: 0)].
	self should: [subject callBooleanMethod: 'isCharZero'].

	subject setCharField: 'charField' to: 16rA.
	self should: [(subject getCharField: 'charField') = (Character codePoint: 16rA)].
	self should: [subject callBooleanMethod: 'isChar0xA'].

	subject setCharField: 'charField' to: 16rAA.
	self should: [(subject getCharField: 'charField') = (Character codePoint: 16rAA)].
	self should: [subject callBooleanMethod: 'isChar0xAA'].

	subject setCharField: 'charField' to: 16rAAA.
	self should: [(subject getCharField: 'charField') = 16rAAA].
	self should: [subject callBooleanMethod: 'isChar0xAAA'].

	subject setCharField: 'charField' to: 16rAAAA.
	self should: [(subject getCharField: 'charField') = 16rAAAA].
	self should: [subject callBooleanMethod: 'isChar0xAAAA'].

	subject setCharField: 'charField' to: $!!.
	self should: [(subject getCharField: 'charField') = $!!].
!

testAccessDoubleField

	self should: [(subject getDoubleField: 'doubleField') = 22.0].

	subject setDoubleField: 'doubleField' to: 0.0.
	self should: [(subject getDoubleField: 'doubleField') = 0.0].
	self should: [subject callBooleanMethod: 'isDoubleZero'].

	subject setDoubleField: 'doubleField' to: 33.0.
	self should: [(subject getDoubleField: 'doubleField') = 33.0].
	self should: [subject callBooleanMethod: 'isDouble33'].

	subject setDoubleField: 'doubleField' to: -33.0.
	self should: [(subject getDoubleField: 'doubleField') = -33.0].
	self should: [subject callBooleanMethod: 'isDoubleMinus33'].

	subject setDoubleField: 'doubleField' to: 33.		"NB: passing an Integer, expecting coercion to double"
	self should: [(subject getDoubleField: 'doubleField') = 33.0].
	self should: [subject callBooleanMethod: 'isDouble33'].

	subject setDoubleField: 'doubleField' to: 22.0.
	self should: [(subject getDoubleField: 'doubleField') = 22.0].
!

testAccessFloatField

	self should: [(subject getFloatField: 'floatField') = 22.0].

	subject setFloatField: 'floatField' to: 0.0.
	self should: [(subject getFloatField: 'floatField') = 0.0].
	self should: [subject callBooleanMethod: 'isFloatZero'].

	subject setFloatField: 'floatField' to: 33.0.
	self should: [(subject getFloatField: 'floatField') = 33.0].
	self should: [subject callBooleanMethod: 'isFloat33'].

	subject setFloatField: 'floatField' to: -33.0.
	self should: [(subject getFloatField: 'floatField') = -33.0].
	self should: [subject callBooleanMethod: 'isFloatMinus33'].

	subject setFloatField: 'floatField' to: 33.		"NB: passing an Integer, expecting coercion to float"
	self should: [(subject getFloatField: 'floatField') = 33.0].
	self should: [subject callBooleanMethod: 'isFloat33'].

	subject setFloatField: 'floatField' to: 22.0.
	self should: [(subject getFloatField: 'floatField') = 22.0].
!

testAccessIntField

	self should: [(subject getIntField: 'intField') = 22].

	subject setIntField: 'intField' to: 0.
	self should: [(subject getIntField: 'intField') = 0].
	self should: [subject callBooleanMethod: 'isIntZero'].

	subject setIntField: 'intField' to: 33.
	self should: [(subject getIntField: 'intField') = 33].
	self should: [subject callBooleanMethod: 'isInt33'].

	subject setIntField: 'intField' to: -33.
	self should: [(subject getIntField: 'intField') = -33].
	self should: [subject callBooleanMethod: 'isIntMinus33'].

	subject setIntField: 'intField' to: 22.
	self should: [(subject getIntField: 'intField') = 22].
!

testAccessLongField

	self should: [(subject getLongField: 'longField') = 22].

	subject setLongField: 'longField' to: 0.
	self should: [(subject getLongField: 'longField') = 0].
	self should: [subject callBooleanMethod: 'isLongZero'].

	subject setLongField: 'longField' to: 33.
	self should: [(subject getLongField: 'longField') = 33].
	self should: [subject callBooleanMethod: 'isLong33'].

	subject setLongField: 'longField' to: -33.
	self should: [(subject getLongField: 'longField') = -33].
	self should: [subject callBooleanMethod: 'isLongMinus33'].

	subject setLongField: 'longField' to: 22.
	self should: [(subject getLongField: 'longField') = 22].
!

testAccessNonExistantField

	| exceptionClass |

	"I'd have thought that NoSuchFieldException would make more sense, but..."
	exceptionClass := jvm findClass: #'java.lang.NoSuchFieldError'.

	self
		should: [subject getStringField: 'nonExistantField']
		raise: exceptionClass.

	self
		should: [subject setStringField: 'nonExistantField' to: nil]
		raise: exceptionClass.
!

testAccessPointField

	| jap p3x4 p33x45 sig point |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new.
	p3x4 setIntField: 'x' to: 3.
	p3x4 setIntField: 'y' to: 4.

	sig := jap jniSignature.
	point := subject getObjectField: 'pointField' signature: sig.
	self should: [point equals: p3x4].

	subject setObjectField: 'pointField' signature: sig to: nil.
	self should: [(subject getObjectField: 'pointField' signature: sig) = nil].
	self should: [subject callBooleanMethod: 'isPointNull'].

	subject setObjectField: 'pointField' signature: sig to: jap new.
	self should: [subject callBooleanMethod: 'isPointZero'].

	p33x45 := jap new.
	p33x45 setIntField: 'x' to: 33.
	p33x45 setIntField: 'y' to: 45.
	subject setObjectField: 'pointField' signature: sig to: p33x45.
	self should: [(subject getObjectField: 'pointField' signature: sig) = p33x45].
	self should: [subject callBooleanMethod: 'isPoint33x45'].

	subject setObjectField: 'pointField' signature: sig to: point.
	self should: [(subject getObjectField: 'pointField' signature: sig) = point].
!

testAccessShortField

	self should: [(subject getShortField: 'shortField') = 22].

	subject setShortField: 'shortField' to: 0.
	self should: [(subject getShortField: 'shortField') = 0].
	self should: [subject callBooleanMethod: 'isShortZero'].

	subject setShortField: 'shortField' to: 33.
	self should: [(subject getShortField: 'shortField') = 33].
	self should: [subject callBooleanMethod: 'isShort33'].

	subject setShortField: 'shortField' to: -33.
	self should: [(subject getShortField: 'shortField') = -33].
	self should: [subject callBooleanMethod: 'isShortMinus33'].

	subject setShortField: 'shortField' to: 22.
	self should: [(subject getShortField: 'shortField') = 22].
!

testAccessStringField

	self should: [(subject getStringField: 'stringField') asString = 'Hi there'].

	subject setStringField: 'stringField' to: nil.
	self should: [(subject getStringField: 'stringField') = nil].
	self should: [subject callBooleanMethod: 'isStringNull'].

	subject setStringField: 'stringField' to: '33'.
	self should: [(subject getStringField: 'stringField') asString = '33'].
	self should: [subject callBooleanMethod: 'isString33'].

	subject setStringField: 'stringField' to: ''.
	self should: [(subject getStringField: 'stringField') isEmpty].
	self should: [subject callBooleanMethod: 'isStringEmpty'].

	subject setStringField: 'stringField' to: 'Hi there'.
	self should: [(subject getStringField: 'stringField') asString = 'Hi there'].
!

testOutOfRangeValues

	self
		should: [subject setByteField: 'byteField' to: 200]
		raise: Error.
	self should: [(subject getByteField: 'byteField') = 22].

	self
		should: [subject setByteField: 'byteField' to: 1000]
		raise: Error.
	self should: [(subject getByteField: 'byteField') = 22].

	self
		should: [subject setShortField: 'shortField' to: 1000000]
		raise: Error.
	self should: [(subject getShortField: 'shortField') = 22].

	self
		should: [subject setIntField: 'intField' to: 6000000000]
		raise: Error.
	self should: [(subject getIntField: 'intField') = 22].

	self
		should: [subject setLongField: 'longField' to: 100 factorial]
		raise: Error.
	self should: [(subject getLongField: 'longField') = 22].! !
!JNIPortBasicFieldsTest categoriesFor: #dontTestTypeErrors!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicFieldsTest categoriesFor: #subject!helpers!public! !
!JNIPortBasicFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessBooleanField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessByteField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessCharField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessDoubleField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessFloatField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessIntField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessLongField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessNonExistantField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessPointField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessShortField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testAccessStringField!public!unit tests! !
!JNIPortBasicFieldsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !

!JNIPortBasicFieldsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortBasicFieldsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortBasicHiddenFieldsTest guid: (GUID fromString: '{4E480A2F-7427-4906-9273-D61CD0C8ACEF}')!
JNIPortBasicHiddenFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicHiddenFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicHiddenFieldsTest methodsFor!

javaTestClassName

	^ 'HidingFields'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testAmbiguousIntFieldViaJVM

	| class |

	"IBM's 1.3 JVM wrongly picks up the class field, i.e. it doesn't interpret bytecodes correctly!!"

	class := self overloadedByTypeClass.
	class isNil ifTrue:
		[Notification signal: (Processor activeProcess topFrame method displayString , ' not run (classfile generation is not installed?)').
		^ self].

	self should: [(class callIntMethod: 'checkAmbiguousIntField') = -100].
!

testClassFieldIsInherited

	self should: [(classStatic getBooleanField: 'staticBooleanField') = true].
!

testOverloadedByTypeField

	| class |

	class := self overloadedByTypeClass.
	class isNil ifTrue:
		[Notification signal: (Processor activeProcess topFrame method displayString , ' not run (classfile generation is not installed?)').
		^ self].

	self should: [(class getIntField: 'field') = 33].
	self should: [(class getBooleanField: 'field') = false].
!

testReadAmbiguousField

	"NB: we should be reading the field in the *interface* "

	"IBM and JRockit wrongly pick up the class field"

	self should: [(classStatic getIntField: 'ambiguousIntField') = -100].!

testReadClassField

	self should: [(classStatic getIntField: 'staticIntField') = 666].!

testReadInstanceField

	self should: [(instance getIntField: 'intField') = 6].!

testReadInterfaceField

	self should: [(classStatic getIntField: 'interfaceIntField') = 66].!

testReadSuperClassField

	self should: [(classStatic super getIntField: 'staticIntField') = 222].!

testReadSuperInstanceField

	self should: [(instance super getIntField: 'intField') = 2].!

testWriteClassField

	classStatic setIntField: 'staticIntField' to: 0.
	self should: [(classStatic getIntField: 'staticIntField') = 0].
	classStatic setIntField: 'staticIntField' to: 666.
	self should: [(classStatic getIntField: 'staticIntField') = 666].
!

testWriteInstanceField

	self should: [(instance getIntField: 'intField') = 6].
	instance setIntField: 'intField' to: 0.
	self should: [(instance getIntField: 'intField') = 0].
!

testWriteSuperClassField

	| sup |

	sup := classStatic super.

	sup setIntField: 'staticIntField' to: 0.
	self should: [(sup getIntField: 'staticIntField') = 0].
	sup setIntField: 'staticIntField' to: 222.
	self should: [(sup getIntField: 'staticIntField') = 222].

	"check that we really have changed the superclass field"
	self should: [(classStatic getIntField: 'staticIntField') = 666].
!

testWriteSuperInstanceField

	| sup |

	sup := instance super.

	self should: [(sup getIntField: 'intField') = 2].
	sup setIntField: 'intField' to: 0.
	self should: [(sup getIntField: 'intField') = 0].

	"check that we really have changed the superclass field"
	self should: [(instance getIntField: 'intField') = 6].
! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testAmbiguousIntFieldViaJVM!public!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testClassFieldIsInherited!public!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testOverloadedByTypeField!public!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadAmbiguousField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadClassField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadInstanceField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadInterfaceField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadSuperClassField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testReadSuperInstanceField!public!reading!unit tests! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testWriteClassField!public!unit tests!writing! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testWriteInstanceField!public!unit tests!writing! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testWriteSuperClassField!public!unit tests!writing! !
!JNIPortBasicHiddenFieldsTest categoriesFor: #testWriteSuperInstanceField!public!unit tests!writing! !

JNIPortBasicHiddenMethodsTest guid: (GUID fromString: '{E53B9776-67B0-4A80-B2C2-4CBC28761C5B}')!
JNIPortBasicHiddenMethodsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicHiddenMethodsTest categoriesForClass!Unclassified! !
!JNIPortBasicHiddenMethodsTest methodsFor!

interface

	^ self javaTestClass: 'InheritedMethodsInterface'.!

javaTestClassName

	^ 'HidingMethods'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testCallClassMethod

	self should: [(classStatic callIntMethod: 'staticIntMethod') = 666].!

testCallInstanceMethod

	self should: [(instance callIntMethod: 'intMethod') = 6].!

testCallMethodAsAnInterface

	| asA |

	asA := instance asInstanceOf: self interface.

	self should: [(asA callIntMethod: 'interfaceIntMethod') = 6666].!

testCallSuperClassMethod

	self should: [(classStatic super callIntMethod: 'staticIntMethod') = 222].!

testCallSuperInstanceMethod

	self should: [(instance super callIntMethod: 'intMethod') = 2].!

testClassMethodIsInherited

	self should: [(classStatic callBooleanMethod: 'staticBooleanMethod') = true].!

testOverloadedByTypeMethod

	| class subject |

	class := self overloadedByTypeClass.
	class isNil ifTrue:
		[Notification signal: (Processor activeProcess topFrame method displayString , ' not run (classfile generation is not installed?)').
		^ self].

	subject := class new.
	self should: [(subject callIntMethod: 'method') = 22].
	self should: [(subject callDoubleMethod: 'method') = 10.0].

! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #interface!constants!public! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testCallClassMethod!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testCallInstanceMethod!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testCallMethodAsAnInterface!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testCallSuperClassMethod!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testCallSuperInstanceMethod!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testClassMethodIsInherited!public!unit tests! !
!JNIPortBasicHiddenMethodsTest categoriesFor: #testOverloadedByTypeMethod!public!unit tests! !

JNIPortBasicInheritedFieldsTest guid: (GUID fromString: '{936CB8D0-7506-4049-831A-55299F2A2480}')!
JNIPortBasicInheritedFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInheritedFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicInheritedFieldsTest methodsFor!

javaTestClassName

	^ 'InheritedFields'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testReadClassField

	self should: [(classStatic getIntField: 'staticIntField') = 222].!

testReadInstanceField

	self should: [(instance getIntField: 'intField') = 2].!

testWriteClassField

	classStatic setIntField: 'staticIntField' to: 0.
	self should: [(classStatic getIntField: 'staticIntField') = 0].
	classStatic setIntField: 'staticIntField' to: 222.
	self should: [(classStatic getIntField: 'staticIntField') = 222].
!

testWriteInstanceField

	self should: [(instance getIntField: 'intField') = 2].
	instance setIntField: 'intField' to: 0.
	self should: [(instance getIntField: 'intField') = 0].
! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #testReadClassField!public!reading!unit tests! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #testReadInstanceField!public!reading!unit tests! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #testWriteClassField!public!unit tests!writing! !
!JNIPortBasicInheritedFieldsTest categoriesFor: #testWriteInstanceField!public!unit tests!writing! !

JNIPortBasicInheritedMethodsTest guid: (GUID fromString: '{FD418BFC-875E-48B8-A5EF-147A8118A8A3}')!
JNIPortBasicInheritedMethodsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInheritedMethodsTest categoriesForClass!Unclassified! !
!JNIPortBasicInheritedMethodsTest methodsFor!

javaTestClassName

	^ 'InheritedMethods'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testCallAbstractMethod

	self should: [(instance callIntMethod: 'abstractIntMethod') = 22].!

testCallClassMethod

	self should: [(classStatic callIntMethod: 'staticIntMethod') = 222].!

testCallInstanceMethod

	self should: [(instance callIntMethod: 'intMethod') = 2].!

testCallInterfaceMethod

	self should: [(instance callIntMethod: 'interfaceIntMethod') = 2222].! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #testCallAbstractMethod!public!unit tests! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #testCallClassMethod!public!unit tests! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #testCallInstanceMethod!public!unit tests! !
!JNIPortBasicInheritedMethodsTest categoriesFor: #testCallInterfaceMethod!public!unit tests! !

JNIPortBasicInhertedInterfaceFieldsTest guid: (GUID fromString: '{DCFBD31B-B7B3-48D7-A7C0-FB6A93493AC4}')!
JNIPortBasicInhertedInterfaceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInhertedInterfaceFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicInhertedInterfaceFieldsTest methodsFor!

javaTestClassName

	^ 'InheritedInterfaceFields'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
!

tearDown

	classStatic := nil.
	super tearDown.
!

testReadField

	self should: [(classStatic getIntField: 'interfaceIntField') = 33].! !
!JNIPortBasicInhertedInterfaceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInhertedInterfaceFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicInhertedInterfaceFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicInhertedInterfaceFieldsTest categoriesFor: #testReadField!public!reading!unit tests! !

JNIPortBasicInterfaceFieldsTest guid: (GUID fromString: '{047DC54E-4D46-4301-B277-A6C5B8FD6CBB}')!
JNIPortBasicInterfaceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInterfaceFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicInterfaceFieldsTest methodsFor!

javaTestClassName

	^ 'InterfaceFields'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
!

tearDown

	classStatic := nil.
	super tearDown.
!

testReadFields

	self should: [(classStatic getIntField: 'publicIntField') = 1].
	self should: [(classStatic getIntField: 'publicFinalIntField') = 2].
	self should: [(classStatic getIntField: 'publicStaticIntField') = 3].
	self should: [(classStatic getIntField: 'publicStaticFinalIntField') = 4].
	self should: [(classStatic getIntField: 'defaultIntField') = 5].
	self should: [(classStatic getIntField: 'defaultFinalIntField') = 6].
	self should: [(classStatic getIntField: 'defaultStaticIntField') = 7].
	self should: [(classStatic getIntField: 'defaultStaticFinalIntField') = 8].
! !
!JNIPortBasicInterfaceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInterfaceFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicInterfaceFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicInterfaceFieldsTest categoriesFor: #testReadFields!public!unit tests! !

JNIPortBasicMethodAccessTest guid: (GUID fromString: '{8DA32EBB-89FD-4191-88D5-DE3C8C2BA2C2}')!
JNIPortBasicMethodAccessTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicMethodAccessTest categoriesForClass!Unclassified! !
!JNIPortBasicMethodAccessTest methodsFor!

javaTestClassName

	^ 'MethodAccess'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testCallDefaultClassMethod

	self should: [(classStatic callIntMethod: 'defaultStaticIntMethod') = 103].!

testCallDefaultFinalClassMethod

	self should: [(classStatic callIntMethod: 'defaultStaticFinalIntMethod') = 113].!

testCallDefaultFinalInstanceMethod

	self should: [(instance callIntMethod: 'defaultFinalIntMethod') = 13].!

testCallDefaultInstanceMethod

	self should: [(instance callIntMethod: 'defaultIntMethod') = 3].!

testCallPrivateClassMethod

	self should: [(classStatic callIntMethod: 'privateStaticIntMethod') = 104].!

testCallPrivateFinalClassMethod

	self should: [(classStatic callIntMethod: 'privateStaticFinalIntMethod') = 114].!

testCallPrivateFinalInstanceMethod

	self should: [(instance callIntMethod: 'privateFinalIntMethod') = 14].!

testCallPrivateInstanceMethod

	self should: [(instance callIntMethod: 'privateIntMethod') = 4].!

testCallProtectedClassMethod

	self should: [(classStatic callIntMethod: 'protectedStaticIntMethod') = 102].!

testCallProtectedFinalClassMethod

	self should: [(classStatic callIntMethod: 'protectedStaticFinalIntMethod') = 112].!

testCallProtectedFinalInstanceMethod

	self should: [(instance callIntMethod: 'protectedFinalIntMethod') = 12].!

testCallProtectedInstanceMethod

	self should: [(instance callIntMethod: 'protectedIntMethod') = 2].!

testCallPublicClassMethod

	self should: [(classStatic callIntMethod: 'publicStaticIntMethod') = 101].!

testCallPublicFinalClassMethod

	self should: [(classStatic callIntMethod: 'publicStaticFinalIntMethod') = 111].!

testCallPublicFinalInstanceMethod

	self should: [(instance callIntMethod: 'publicFinalIntMethod') = 11].!

testCallPublicInstanceMethod

	self should: [(instance callIntMethod: 'publicIntMethod') = 1].! !
!JNIPortBasicMethodAccessTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicMethodAccessTest categoriesFor: #setUp!public!running! !
!JNIPortBasicMethodAccessTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallDefaultClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallDefaultFinalClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallDefaultFinalInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallDefaultInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPrivateClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPrivateFinalClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPrivateFinalInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPrivateInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallProtectedClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallProtectedFinalClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallProtectedFinalInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallProtectedInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPublicClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPublicFinalClassMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPublicFinalInstanceMethod!public!unit tests! !
!JNIPortBasicMethodAccessTest categoriesFor: #testCallPublicInstanceMethod!public!unit tests! !

JNIPortBasicMethodArgumentsTest guid: (GUID fromString: '{79F8CA2C-149E-4274-A37D-E43CABEDF579}')!
JNIPortBasicMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortBasicMethodArgumentsTest methodsFor!

dontTestTypeErrors

	"unfortunately we can't use this test since the JVM doesn't actually check the types
	and therefore tends to crash..."

	| string sig args |

	string := 'Hi there' asJavaString: jvm.	"need to hold this in an instvar"
	sig := '(Ljava/awt/Point;)Ljava/lang/String;'.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: string;
			yourself.
	self
		should: [subject callObjectMethod: 'method' signature: sig withArguments: args]
		raise: JavaException.
!

setUp

	super setUp.
	subject := self subject.!

subject

	self subclassResponsibility.!

tearDown

	subject := nil.
	super tearDown.
!

testBooleanArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(Z)Ljava/lang/String;'.

	args booleanAt: 1 put: true.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'boolean: true'].

	args booleanAt: 1 put: false.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'boolean: false'].!

testByteArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(B)Ljava/lang/String;'.

	args byteAt: 1 put: 0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'byte: 0'].

	args byteAt: 1 put: 100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'byte: 100'].

	args byteAt: 1 put: -100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'byte: -100'].
!

testCharArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(C)Ljava/lang/String;'.

	args charAt: 1 put: $!!.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'char: !!'].

	args charAt: 1 put: $A.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'char: A'].!

testDoubleArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(D)Ljava/lang/String;'.

	args doubleAt: 1 put: 0.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: 0.0'].

	args doubleAt: 1 put: 100.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: 100.0'].

	args doubleAt: 1 put: -100.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: -100.0'].

	args doubleAt: 1 put: 1000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: 1000.0'].

	args doubleAt: 1 put: -1000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: -1000.0'].

	args doubleAt: 1 put: 1000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: 1000000.0'].

	args doubleAt: 1 put: -1000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: -1000000.0'].

	args doubleAt: 1 put: 1000000000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: 1.0E12'].		"JVM specific ??"

	args doubleAt: 1 put: -1000000000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'double: -1.0E12'].		"JVM specific ??"!

testFloatArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(F)Ljava/lang/String;'.

	args floatAt: 1 put: 0.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: 0.0'].

	args floatAt: 1 put: 100.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: 100.0'].

	args floatAt: 1 put: -100.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: -100.0'].

	args floatAt: 1 put: 1000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: 1000.0'].

	args floatAt: 1 put: -1000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: -1000.0'].

	args floatAt: 1 put: 1000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: 1000000.0'].

	args floatAt: 1 put: -1000000.0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'float: -1000000.0'].!

testIntArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(I)Ljava/lang/String;'.

	args intAt: 1 put: 0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: 0'].

	args intAt: 1 put: 100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: 100'].

	args intAt: 1 put: -100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: -100'].

	args intAt: 1 put: 1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: 1000'].

	args intAt: 1 put: -1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: -1000'].

	args intAt: 1 put: 1000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: 1000000'].

	args intAt: 1 put: -1000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: -1000000'].!

testLongArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(J)Ljava/lang/String;'.

	args longAt: 1 put: 0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: 0'].

	args longAt: 1 put: 100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: 100'].

	args longAt: 1 put: -100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: -100'].

	args longAt: 1 put: 1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: 1000'].

	args longAt: 1 put: -1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: -1000'].

	args longAt: 1 put: 1000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: 1000000'].

	args longAt: 1 put: -1000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: -1000000'].

	args longAt: 1 put: 1000000000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: 1000000000000'].

	args longAt: 1 put: -1000000000000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'long: -1000000000000'].!

testNoArguments

	| sig answer |

	sig := '()Ljava/lang/String;'.

	answer := subject callObjectMethod: 'method' signature: sig.
	self should: [answer asString = '<nothing>'].!

testNullArgument

	| args sig answer |

	sig := '(Ljava/lang/String;)Ljava/lang/String;'.
	args := JNIValueArray new: 1.

	args objectAt: 1 put: nil.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'String: null'].
!

testOutOfRangeValues

	| args |

	"we only test the JNIValueArray, since that'll fail before it reaches the JVM (and the
	JVM won't fail it, so there's no point in testing that)"
	args := JNIValueArray new: 1.

	self should: [args byteAt: 1 put: 200] raise: Error.
	self should: [args byteAt: 1 put: 1000] raise: Error.
	self should: [args shortAt: 1 put: 1000000] raise: Error.
	self should: [args intAt: 1 put: 6000000000] raise: Error.
	self should: [args longAt: 1 put: 100 factorial] raise: Error.
!

testPointArgument

	| jap p3x4 args sig answer |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new.
	p3x4 setIntField: 'x' to: 3.
	p3x4 setIntField: 'y' to: 4.

	sig := '(Ljava/awt/Point;)Ljava/lang/String;'.
	args := JNIValueArray new: 1.

	args objectAt: 1 put: p3x4.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'Point: java.awt.Point[x=3,y=4]'].		"JVM specific ??"!

testShortArgument

	| args sig answer |

	args := JNIValueArray new: 1.
	sig := '(S)Ljava/lang/String;'.

	args shortAt: 1 put: 0.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'short: 0'].

	args shortAt: 1 put: 100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'short: 100'].

	args shortAt: 1 put: -100.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'short: -100'].

	args shortAt: 1 put: 1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'short: 1000'].

	args shortAt: 1 put: -1000.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'short: -1000'].!

testStringArgument

	| args sig string answer |

	string := 'Hi there!!' asJavaString: jvm.	"need to hold this in an instvar"
	sig := '(Ljava/lang/String;)Ljava/lang/String;'.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: string;
			yourself.

	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'String: Hi there!!'].!

testTwoArguments

	| args sig answer |

	args := JNIValueArray new: 2.
	sig := '(IF)Ljava/lang/String;'.

	args intAt: 1 put: 7.
	args floatAt: 2 put: 1.5.
	answer := subject callObjectMethod: 'method' signature: sig withArguments: args.
	self should: [answer asString = 'int: 7 float: 1.5'].! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #dontTestTypeErrors!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #subject!helpers!public! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testBooleanArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testByteArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testCharArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testDoubleArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testFloatArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testIntArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testLongArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testNoArguments!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testNullArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testPointArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testShortArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testStringArgument!public!unit tests! !
!JNIPortBasicMethodArgumentsTest categoriesFor: #testTwoArguments!public!unit tests! !

!JNIPortBasicMethodArgumentsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortBasicMethodArgumentsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortBasicMethodReturnsTest guid: (GUID fromString: '{2AEA67B2-044A-4479-81EF-BFBE23B73F10}')!
JNIPortBasicMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortBasicMethodReturnsTest methodsFor!

setUp

	super setUp.
	subject := self subject.!

subject

	self subclassResponsibility.!

tearDown

	subject := nil.
	super tearDown.
!

testBooleanMethodReturn

	self should: [(subject callBooleanMethod: 'booleanMethod') = true].
!

testByteMethodReturn

	self should: [(subject callByteMethod: 'byteMethod') = 20].
!

testCallNonExistantMethod

	| exceptionClass |

	"I'd have thought that NoSuchFieldException would make more sense, but..."
	exceptionClass := jvm findClass: #'java.lang.NoSuchMethodError'.

	self
		should: [subject callStringMethod: 'nonExistantMethod']
		raise: exceptionClass.
!

testCharMethodReturn

	self should: [(subject callCharMethod: 'charMethod') = $!!].!

testDoubleMethodReturn

	self should: [(subject callDoubleMethod: 'doubleMethod') = 25.0].!

testFloatMethodReturn

	self should: [(subject callFloatMethod: 'floatMethod') = 24.0].!

testIntMethodReturn

	self should: [(subject callIntMethod: 'intMethod') = 22].!

testLongMethodReturn

	self should: [(subject callLongMethod: 'longMethod') = 23].!

testNullMethodReturn

	| object |

	object := subject callObjectMethod: 'nullMethod' signature: '()Ljava/lang/Object;'.
	self should: [object isNil].
!

testPointMethodReturn

	| point |

	point := subject callObjectMethod: 'pointMethod' signature: '()Ljava/awt/Point;'.
	self should: [(point getIntField: 'x') = 3].
	self should: [(point getIntField: 'y') = 4].
!

testShortMethodReturn

	self should: [(subject callShortMethod: 'shortMethod') = 21].!

testStringMethodReturn

	self should: [(subject callStringMethod: 'stringMethod') asString = 'Hi there'].!

testVoidMethodReturn

	self should: [(subject callVoidMethod: 'voidMethod') == subject].! !
!JNIPortBasicMethodReturnsTest categoriesFor: #setUp!public!running! !
!JNIPortBasicMethodReturnsTest categoriesFor: #subject!helpers!public! !
!JNIPortBasicMethodReturnsTest categoriesFor: #tearDown!public!running! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testBooleanMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testByteMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testCallNonExistantMethod!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testCharMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testDoubleMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testFloatMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testIntMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testLongMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testNullMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testPointMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testShortMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testStringMethodReturn!public!unit tests! !
!JNIPortBasicMethodReturnsTest categoriesFor: #testVoidMethodReturn!public!unit tests! !

!JNIPortBasicMethodReturnsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortBasicMethodReturnsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortClassLookupTest guid: (GUID fromString: '{980D4721-B442-4254-89FB-936448B652C7}')!
JNIPortClassLookupTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortClassLookupTest categoriesForClass!Unclassified! !
!JNIPortClassLookupTest methodsFor!

testFindArrayClass

	#(#'java.lang.Object' #'java.lang.StringBuffer' #boolean #char #byte #short #int #long #float #double) do:
		[:each || elementClass arrayClass |
		elementClass := jvm findClass: each.
		arrayClass := jvm findClass: (each , '[]').
		self should: [arrayClass isArray].
		self should: [arrayClass elementClass == elementClass]].
!

testFindClass

	| class1 class2 |

	class1 := jvm findClass: #'java.lang.StringBuffer'.

	class2 := jvm findClass: #'java.lang.StringBuffer'.
	self should: [class1 == class2].

	class2 := jvm findClass: 'java.lang.StringBuffer'.
	self should: [class1 == class2].

	class2 := jvm findClass: 'java/lang/StringBuffer'.
	self should: [class1 == class2].!

testFindNonExistantClass

	self
		should: [jvm findClass: 'org.metagnostic.NEVER_NEVER']
		raise: self noSuchClassExceptionClass.!

testFindPrimitiveClass

	#(#void #boolean #char #byte #short #int #long #float #double) do:
		[:each || class |
		class := jvm findClass: each.
		self should: [class isPrimitive].
		self should: [class name = each]].
! !
!JNIPortClassLookupTest categoriesFor: #testFindArrayClass!public!unit tests! !
!JNIPortClassLookupTest categoriesFor: #testFindClass!public!unit tests! !
!JNIPortClassLookupTest categoriesFor: #testFindNonExistantClass!public!unit tests! !
!JNIPortClassLookupTest categoriesFor: #testFindPrimitiveClass!public!unit tests! !

JNIPortComparisonTest guid: (GUID fromString: '{BE8E5A1B-D93C-4C93-9A6F-3A2B807ADC5A}')!
JNIPortComparisonTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortComparisonTest categoriesForClass!Unclassified! !
!JNIPortComparisonTest methodsFor!

setUp

	super setUp.

	string1 := 'a string' asJavaString: jvm.

	"assume that java.lang.String.toString() will return itself"
	string2 := string1 callStringMethod: 'toString'.
!

tearDown

	string1 := string2 := nil.

	super tearDown.!

testEquality

	self should: [string1 = string2].
!

testHash

	self should: [string1 hash = string2 hash].
!

testIdentity

	self shouldnt: [string1 == string2].		"unless we've made String canonical -- NOT a good idea"
!

testJavaEquality

	self should: [string1 equals: ('a string' asJavaString: jvm)].
!

testJavaIdentity

	self should: [string1 isSameAs: string2].
! !
!JNIPortComparisonTest categoriesFor: #setUp!public!running! !
!JNIPortComparisonTest categoriesFor: #tearDown!public!running! !
!JNIPortComparisonTest categoriesFor: #testEquality!public!unit tests! !
!JNIPortComparisonTest categoriesFor: #testHash!public!unit tests! !
!JNIPortComparisonTest categoriesFor: #testIdentity!public!unit tests! !
!JNIPortComparisonTest categoriesFor: #testJavaEquality!public!unit tests! !
!JNIPortComparisonTest categoriesFor: #testJavaIdentity!public!unit tests! !

JNIPortExceptionTest guid: (GUID fromString: '{B37CA120-F2C0-454D-9270-A3550CEADBD3}')!
JNIPortExceptionTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortExceptionTest categoriesForClass!Unclassified! !
!JNIPortExceptionTest methodsFor!

javaTestClassName

	^ 'Exceptions'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
!

tearDown

	classStatic := nil.
	super tearDown.
!

testCatchExceptionByClassStatic

	| caught |

	[classStatic callVoidMethod: 'thrower']
		on: self nullPointerExceptionClass
		do: [:ex | caught := ex].

	self should: [caught isKindOf: JavaException].
	self should: [caught tag isInstanceOf: self nullPointerExceptionClass].
!

testCatchExceptionBySmalltalkClass

	| caught |

	[classStatic callVoidMethod: 'thrower']
		on: JavaException
		do: [:ex | caught := ex].

	self should: [caught isKindOf: JavaException].
	self should: [caught tag isInstanceOf: self nullPointerExceptionClass].
!

testCatchExceptionByWrapperClass

	| caught |

	[classStatic callVoidMethod: 'thrower']
		on: JavaLangException
		do: [:ex | caught := ex].

	self should: [caught isKindOf: JavaException].
	self should: [caught tag isInstanceOf: self nullPointerExceptionClass].
! !
!JNIPortExceptionTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortExceptionTest categoriesFor: #setUp!public!running! !
!JNIPortExceptionTest categoriesFor: #tearDown!public!running! !
!JNIPortExceptionTest categoriesFor: #testCatchExceptionByClassStatic!public!unit tests! !
!JNIPortExceptionTest categoriesFor: #testCatchExceptionBySmalltalkClass!public!unit tests! !
!JNIPortExceptionTest categoriesFor: #testCatchExceptionByWrapperClass!public!unit tests! !

JNIPortStringTest guid: (GUID fromString: '{1C1B036C-6B70-42F4-ADD4-8208234FBB80}')!
JNIPortStringTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortStringTest categoriesForClass!Unclassified! !
!JNIPortStringTest methodsFor!

javaStringFrom: anInteger to: anotherInteger

	| args sig |

	args := JNIValueArray new: 2.
	sig := '(II)Ljava/lang/String;'.

	args intAt: 1 put: anInteger.
	args intAt: 2 put: anotherInteger+1.

	^ classStatic callObjectMethod: 'make' signature: sig withArguments: args.
!

javaTestClassName

	^ 'Strings'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
!

tearDown

	classStatic := nil.
	super tearDown.
!

testAsByteArray16Bit

	| javaString |

	javaString := self javaStringFrom: 256 to: 65535.
	self
		should: [javaString asByteArray]
		raise: BoundsError.		"it's a bounds error because that's what (Character codePoint: 256) raises"!

testAsByteArray8Bit

	| javaString bytes i |

	javaString := self javaStringFrom: 0 to: 255.
	bytes := javaString asByteArray.

	i := 0.
	bytes do: [:each | self should: [each = i]. i := i + 1].!

testAsString16Bit

	| javaString |

	javaString := self javaStringFrom: 256 to: 65535.
	self
		should: [javaString asString]
		raise: BoundsError.		"it's a bounds error because that's what (Character codePoint: 256) raises"!

testAsString8Bit

	| javaString string i |

	javaString := self javaStringFrom: 0 to: 255.
	string := javaString asByteArray.

	i := 0.
	string do: [:each | self should: [each = i]. i := i + 1].!

testConvertEmptyString

	| javaString |

	javaString := '' asJavaString: jvm.
	self should: [javaString asString = ''].!

testConvertString

	| string javaString |

	string := 'a test string'.
	javaString := string asJavaString: jvm.
	self should: [javaString asString = string].!

testConvertWhenHasNulls

	| string javaString |

	string := 'a test String'.
	string at: 2 put: (Character codePoint: 0).
	string at: 7 put: (Character codePoint: 0).
	javaString := string asJavaString: jvm.
	self should: [javaString size = 13].
	self should: [javaString asString = string].
!

testConvertWhenHibitSet

	| string javaString |

	string := ByteArray withAll: (0 to: 255).
	string := string , string reverse.
	string := string asString.
	javaString := string asJavaString: jvm.
	self should: [javaString size = 512].
	self should: [javaString asString = string].
!

testDoLoop1

	| javaString i |

	javaString := self javaStringFrom: 0 to: 255.

	i := 0.
	javaString do: [:each | self should: [each codePoint = i]. i := i + 1].!

testDoLoop2

	| javaString i |

	javaString := self javaStringFrom: 256 to: 65535 .

	i := 256.
	javaString do: [:each | self should: [each = i]. i := i + 1].!

testFromByteArray

	| bytes javaString |

	bytes := #[97 32 116 101 115 116 32 83 116 114 105 110 103].
	javaString := JavaLangString
			fromByteArray: bytes
			jvm: jvm.
	self should: [javaString asByteArray = bytes].!

testFromString

	| string javaString |

	string := 'a test String'.
	javaString := JavaLangString
			fromString: string
			jvm: jvm.
	self should: [javaString asString = string].!

testNilAsString

	| javaString |

	javaString := nil asJavaString: jvm.
	self should: [javaString isNil].! !
!JNIPortStringTest categoriesFor: #javaStringFrom:to:!helpers!private! !
!JNIPortStringTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortStringTest categoriesFor: #setUp!public!running! !
!JNIPortStringTest categoriesFor: #tearDown!public!running! !
!JNIPortStringTest categoriesFor: #testAsByteArray16Bit!public!unit tests! !
!JNIPortStringTest categoriesFor: #testAsByteArray8Bit!public!unit tests! !
!JNIPortStringTest categoriesFor: #testAsString16Bit!public!unit tests! !
!JNIPortStringTest categoriesFor: #testAsString8Bit!public!unit tests! !
!JNIPortStringTest categoriesFor: #testConvertEmptyString!public!unit tests! !
!JNIPortStringTest categoriesFor: #testConvertString!public!unit tests! !
!JNIPortStringTest categoriesFor: #testConvertWhenHasNulls!public!unit tests! !
!JNIPortStringTest categoriesFor: #testConvertWhenHibitSet!public!unit tests! !
!JNIPortStringTest categoriesFor: #testDoLoop1!public!unit tests! !
!JNIPortStringTest categoriesFor: #testDoLoop2!public!unit tests! !
!JNIPortStringTest categoriesFor: #testFromByteArray!public!unit tests! !
!JNIPortStringTest categoriesFor: #testFromString!public!unit tests! !
!JNIPortStringTest categoriesFor: #testNilAsString!public!unit tests! !

JNIPortArrayOfBooleansTest guid: (GUID fromString: '{CB0B3647-062B-48B3-8128-3EFECE4CF0DE}')!
JNIPortArrayOfBooleansTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfBooleansTest categoriesForClass!Unclassified! !
!JNIPortArrayOfBooleansTest methodsFor!

elementClassName

	^ #boolean.!

elements

	^ Array with: true with: false.! !
!JNIPortArrayOfBooleansTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfBooleansTest categoriesFor: #elements!accessing!public! !

JNIPortArrayOfCharsTest guid: (GUID fromString: '{505FF5A3-67F1-4218-BE82-963D0EE61458}')!
JNIPortArrayOfCharsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfCharsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfCharsTest methodsFor!

elementClassName

	^ #char.!

elements

	^ #( $a $b $C ).! !
!JNIPortArrayOfCharsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfCharsTest categoriesFor: #elements!accessing!public! !

JNIPortArrayOfDoublesTest guid: (GUID fromString: '{0DA4220D-DA83-41FE-902C-51BF6F783029}')!
JNIPortArrayOfDoublesTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfDoublesTest categoriesForClass!Unclassified! !
!JNIPortArrayOfDoublesTest methodsFor!

elementClassName

	^ #double.!

elements

	^ #( 0.0 1.0 2.5 ).! !
!JNIPortArrayOfDoublesTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfDoublesTest categoriesFor: #elements!accessing!public! !

JNIPortArrayOfFloatsTest guid: (GUID fromString: '{8B21142F-7229-4372-851B-29FDACBBE8DE}')!
JNIPortArrayOfFloatsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfFloatsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfFloatsTest methodsFor!

elementClassName

	^ #float.!

elements

	^ #( 0.0 1.0 2.5 ).! !
!JNIPortArrayOfFloatsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfFloatsTest categoriesFor: #elements!accessing!public! !

JNIPortArrayOfIntegerTest guid: (GUID fromString: '{2AE6BE8F-DBA6-42EE-9D33-75772E73217A}')!
JNIPortArrayOfIntegerTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfIntegerTest categoriesForClass!Unclassified! !
!JNIPortArrayOfIntegerTest methodsFor!

elements

	#subclassResponsibility.
	^ #( 0 1 2 3 )!

outOfRangeValues

	self subclassResponsibility.
!

testOutOfRangeValues

	| array values |

	array := self elementClass newArray: 1.

	self outOfRangeValues do: [:each | self should: [array at: 1 put: each] raise: Error].


! !
!JNIPortArrayOfIntegerTest categoriesFor: #elements!accessing!public! !
!JNIPortArrayOfIntegerTest categoriesFor: #outOfRangeValues!accessing!public! !
!JNIPortArrayOfIntegerTest categoriesFor: #testOutOfRangeValues!public!unit tests! !

!JNIPortArrayOfIntegerTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortArrayOfIntegerTest class categoriesFor: #isAbstract!public!testing! !

JNIPortArrayOfObjectsTest guid: (GUID fromString: '{6A1FABD8-3883-41E7-AEF2-6D6AF8933F4B}')!
JNIPortArrayOfObjectsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfObjectsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfObjectsTest methodsFor!

elementClassName

	^ #'java.lang.Object'.!

elements

	^ elements.!

setUp

	super setUp.
	elements := #( 'This' 'may' 'be' '' 'a' 'test' ) collect: [:each | each asJavaString: jvm].!

tearDown

	elements := nil.
	super tearDown.
! !
!JNIPortArrayOfObjectsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfObjectsTest categoriesFor: #elements!accessing!public! !
!JNIPortArrayOfObjectsTest categoriesFor: #setUp!public!running! !
!JNIPortArrayOfObjectsTest categoriesFor: #tearDown!public!running! !

JNIPortArrayOfPointsTest guid: (GUID fromString: '{8087BCA7-0A5B-4D95-94AE-975E9A36BD93}')!
JNIPortArrayOfPointsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfPointsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfPointsTest methodsFor!

elementClassName

	^ #'java.awt.Point'.!

elements

	^ elements.!

setUp

	| jap |

	super setUp.

	jap := jvm findClass: #'java.awt.Point'.
	elements := #( (3 4) (9 -2) ) collect:
		[:each | (jap new)
				setIntField: 'x' to: each first;
				setIntField: 'y' to: each second;
				yourself].!

tearDown

	elements := nil.
	super tearDown.
!

testTypeErrors

	| array |

	array := self elementClass newArray: 1.

	self
		should: [array at: 1 put: ('just testing' asJavaString: jvm)]
		raise: self arrayStoreExceptionClass.

	self
		should: [array at: 1 put: (jvm findClass: #'java.lang.Object') new]
		raise: self arrayStoreExceptionClass.

! !
!JNIPortArrayOfPointsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfPointsTest categoriesFor: #elements!accessing!public! !
!JNIPortArrayOfPointsTest categoriesFor: #setUp!public!running! !
!JNIPortArrayOfPointsTest categoriesFor: #tearDown!public!running! !
!JNIPortArrayOfPointsTest categoriesFor: #testTypeErrors!public!unit tests! !

JNIPortArrayOfStringsTest guid: (GUID fromString: '{090DA731-D380-48CC-ADF6-5E4845F3B41B}')!
JNIPortArrayOfStringsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfStringsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfStringsTest methodsFor!

elementClassName

	^ #'java.lang.String'.!

elements

	^ elements.!

setUp

	super setUp.
	elements := #( 'This' 'may' 'be' '' 'a' 'test' ) collect: [:each | each asJavaString: jvm].!

tearDown

	elements := nil.
	super tearDown.
!

testTypeErrors

	| array |

	array := self elementClass newArray: 1.

	self
		should: [array at: 1 put: (jvm findClass: #'java.awt.Point') new]
		raise: self arrayStoreExceptionClass.

	self
		should: [array at: 1 put: (jvm findClass: #'java.lang.Object') new]
		raise: self arrayStoreExceptionClass.

! !
!JNIPortArrayOfStringsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfStringsTest categoriesFor: #elements!accessing!public! !
!JNIPortArrayOfStringsTest categoriesFor: #setUp!public!running! !
!JNIPortArrayOfStringsTest categoriesFor: #tearDown!public!running! !
!JNIPortArrayOfStringsTest categoriesFor: #testTypeErrors!public!unit tests! !

JNIPortArrayOfBytesTest guid: (GUID fromString: '{DDCBFB46-ED28-4582-9B58-6462BD945396}')!
JNIPortArrayOfBytesTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfBytesTest categoriesForClass!Unclassified! !
!JNIPortArrayOfBytesTest methodsFor!

elementClassName

	^ #byte.!

outOfRangeValues

	^ #(-200 200 1000).! !
!JNIPortArrayOfBytesTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfBytesTest categoriesFor: #outOfRangeValues!accessing!public! !

JNIPortArrayOfIntsTest guid: (GUID fromString: '{F23B1B30-1D94-4562-A0A6-A64F0E978C82}')!
JNIPortArrayOfIntsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfIntsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfIntsTest methodsFor!

elementClassName

	^ #int.!

outOfRangeValues

	^ #(-6000000000 6000000000).! !
!JNIPortArrayOfIntsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfIntsTest categoriesFor: #outOfRangeValues!accessing!public! !

JNIPortArrayOfLongsTest guid: (GUID fromString: '{5E9816BD-2857-4359-9D09-86CAF56527A0}')!
JNIPortArrayOfLongsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfLongsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfLongsTest methodsFor!

elementClassName

	^ #long.!

outOfRangeValues

	^ Array with: 100 factorial.! !
!JNIPortArrayOfLongsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfLongsTest categoriesFor: #outOfRangeValues!accessing!public! !

JNIPortArrayOfShortsTest guid: (GUID fromString: '{CC1247A7-1771-4FDC-846A-A70ABD8A06C7}')!
JNIPortArrayOfShortsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortArrayOfShortsTest categoriesForClass!Unclassified! !
!JNIPortArrayOfShortsTest methodsFor!

elementClassName

	^ #short.!

outOfRangeValues

	^ #(-100000 100000).! !
!JNIPortArrayOfShortsTest categoriesFor: #elementClassName!accessing!public! !
!JNIPortArrayOfShortsTest categoriesFor: #outOfRangeValues!accessing!public! !

JNIPortBasicInstanceFieldsTest guid: (GUID fromString: '{5EAFE1CE-4565-42AE-B22E-5ECD977C86F5}')!
JNIPortBasicInstanceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInstanceFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicInstanceFieldsTest methodsFor!

javaTestClassName

	^ 'InstanceFields'.!

subject

	^ self javaTestClass new.! !
!JNIPortBasicInstanceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInstanceFieldsTest categoriesFor: #subject!helpers!public! !

JNIPortBasicStaticFieldsTest guid: (GUID fromString: '{463D17E9-3183-4185-9119-4A2826284F37}')!
JNIPortBasicStaticFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicStaticFieldsTest categoriesForClass!Unclassified! !
!JNIPortBasicStaticFieldsTest methodsFor!

javaTestClassName

	^ 'StaticFields'.!

subject

	^ self javaTestClass.! !
!JNIPortBasicStaticFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicStaticFieldsTest categoriesFor: #subject!helpers!public! !

JNIPortBasicInstanceMethodArgumentsTest guid: (GUID fromString: '{69C9167D-26B4-4882-B99A-B862349AEB46}')!
JNIPortBasicInstanceMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInstanceMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortBasicInstanceMethodArgumentsTest methodsFor!

javaTestClassName

	^ 'InstanceMethodArguments'.!

subject

	^ self javaTestClass new.! !
!JNIPortBasicInstanceMethodArgumentsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInstanceMethodArgumentsTest categoriesFor: #subject!helpers!public! !

JNIPortBasicStaticMethodArgumentsTest guid: (GUID fromString: '{3B19DA0B-5048-43EA-B9E5-F46E98925006}')!
JNIPortBasicStaticMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicStaticMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortBasicStaticMethodArgumentsTest methodsFor!

javaTestClassName

	^ 'StaticMethodArguments'.!

subject

	^ self javaTestClass.! !
!JNIPortBasicStaticMethodArgumentsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicStaticMethodArgumentsTest categoriesFor: #subject!helpers!public! !

JNIPortBasicInstanceMethodReturnsTest guid: (GUID fromString: '{A8DF412E-FE4A-4807-93FE-F1D1B65AE519}')!
JNIPortBasicInstanceMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicInstanceMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortBasicInstanceMethodReturnsTest methodsFor!

javaTestClassName

	^ 'InstanceMethods'.!

subject

	^ self javaTestClass new.! !
!JNIPortBasicInstanceMethodReturnsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicInstanceMethodReturnsTest categoriesFor: #subject!helpers!public! !

JNIPortBasicStaticMethodReturnsTest guid: (GUID fromString: '{CC5C8948-870D-4A3F-8BDA-0BE8F27C4054}')!
JNIPortBasicStaticMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortBasicStaticMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortBasicStaticMethodReturnsTest methodsFor!

javaTestClassName

	^ 'StaticMethods'.!

subject

	^ self javaTestClass.! !
!JNIPortBasicStaticMethodReturnsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortBasicStaticMethodReturnsTest categoriesFor: #subject!helpers!public! !

"Binary Globals"!

"Resources"!

