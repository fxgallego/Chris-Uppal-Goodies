| package |
package := Package name: 'CU Java Wrapper Generation Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Some regression tests for the generated methods level of JNIPort

If you are using this package then the corresponding Java code (shipped in the "Extras\JNIPort-Tests.jar" file) must be on the Java classpath somewhere (one way to ensure this is to add the file to the classpath in the JVM settings).

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.01'.


package classNames
	add: #JNIPortGeneratedAmbiguousSelectorsTest;
	add: #JNIPortGeneratedConstructorsTest;
	add: #JNIPortGeneratedFieldAccessTest;
	add: #JNIPortGeneratedFieldsTest;
	add: #JNIPortGeneratedHiddenFieldsTest;
	add: #JNIPortGeneratedHiddenMethodsTest;
	add: #JNIPortGeneratedInheritedFieldsTest;
	add: #JNIPortGeneratedInheritedMethodsTest;
	add: #JNIPortGeneratedInhertedInterfaceFieldsTest;
	add: #JNIPortGeneratedInstanceFieldsTest;
	add: #JNIPortGeneratedInstanceMethodArgumentsTest;
	add: #JNIPortGeneratedInstanceMethodReturnsTest;
	add: #JNIPortGeneratedInterfaceFieldsTest;
	add: #JNIPortGeneratedMethodAccessTest;
	add: #JNIPortGeneratedMethodArgumentsTest;
	add: #JNIPortGeneratedMethodReturnsTest;
	add: #JNIPortGeneratedStaticFieldsTest;
	add: #JNIPortGeneratedStaticMethodArgumentsTest;
	add: #JNIPortGeneratedStaticMethodReturnsTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU Java Base Tests';
	add: 'CU Java Wrapper Generation';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

GenericJNIPortRegressionTest subclass: #JNIPortGeneratedAmbiguousSelectorsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedConstructorsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedFieldAccessTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedFieldsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedHiddenFieldsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedHiddenMethodsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedInheritedFieldsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedInheritedMethodsTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedInhertedInterfaceFieldsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedInterfaceFieldsTest
	instanceVariableNames: 'classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedMethodAccessTest
	instanceVariableNames: 'instance classStatic'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedMethodArgumentsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortGeneratedMethodReturnsTest
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedFieldsTest subclass: #JNIPortGeneratedInstanceFieldsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedFieldsTest subclass: #JNIPortGeneratedStaticFieldsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedMethodArgumentsTest subclass: #JNIPortGeneratedInstanceMethodArgumentsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedMethodArgumentsTest subclass: #JNIPortGeneratedStaticMethodArgumentsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedMethodReturnsTest subclass: #JNIPortGeneratedInstanceMethodReturnsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JNIPortGeneratedMethodReturnsTest subclass: #JNIPortGeneratedStaticMethodReturnsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

JNIPortGeneratedAmbiguousSelectorsTest guid: (GUID fromString: '{11FBB1A9-CE8C-4DC2-A373-A95BE5472E73}')!
JNIPortGeneratedAmbiguousSelectorsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedAmbiguousSelectorsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedAmbiguousSelectorsTest methodsFor!

javaTestClassName

	^ 'AmbiguousSelectors'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.!

tearDown

	classStatic := instance := nil.
	super tearDown.
!

testFallbackToLongForm

	self should: [instance longForm_Array: nil] raise: AmbiguousJavaMethodError.
	self should: [(instance longForm_javalangreflectArray: nil) asString = 'java.lang.reflect.Array'].
	self should: [(instance longForm_javasqlArray: nil) asString = 'java.sql.Array'].

!

testFallbackToLongFormAndReturnType

	self should: [instance returnType_TestArray: nil] raise: AmbiguousJavaMethodError.
	self should: [(instance returnType_int_orgmetagnosticjniporttestregressionTestArray: nil)  = 2].
	self should: [(instance returnType_double_orgmetagnosticjniporttestregressionTestArray: nil)  = 3.14159].

!

testIncurableAmbiguity

	self should: [instance incurable_TestArray: nil] raise: AmbiguousJavaMethodError.

	"we have no idea which of the two methods is called here"
	#CUtodo.  "this should probably throw a MNU exception"
	instance incurable_void_orgmetagnosticjniporttestregressionTestArray: nil.

! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #testFallbackToLongForm!public!unit tests! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #testFallbackToLongFormAndReturnType!public!unit tests! !
!JNIPortGeneratedAmbiguousSelectorsTest categoriesFor: #testIncurableAmbiguity!public!unit tests! !

JNIPortGeneratedConstructorsTest guid: (GUID fromString: '{644BBCB0-FDB7-403C-8D9B-E4C9CB302E0B}')!
JNIPortGeneratedConstructorsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedConstructorsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedConstructorsTest methodsFor!

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

	self should: [(classStatic new_boolean: true) getText_null asString = 'boolean: true'].
	self should: [(classStatic new_boolean: false) getText_null asString = 'boolean: false'].
!

testByteArgument

	self should: [(classStatic new_byte: 0) getText_null asString = 'byte: 0'].
	self should: [(classStatic new_byte: 100) getText_null asString = 'byte: 100'].
	self should: [(classStatic new_byte: -100) getText_null asString = 'byte: -100'].
!

testCharArgument

	self should: [(classStatic new_char: $!!) getText_null asString = 'char: !!'].
	self should: [(classStatic new_char: $A) getText_null asString = 'char: A'].!

testDerivedClassConstructor

	| new |

	new := self derivedClass new_null.

	self should: [new static = self derivedClass].
	self should: [new getText_null asString = 'String: derived'].!

testDoubleArgument

	self should: [(classStatic new_double: 0.0) getText_null asString = 'double: 0.0'].
	self should: [(classStatic new_double: 100.0) getText_null asString = 'double: 100.0'].
	self should: [(classStatic new_double: -100.0) getText_null asString = 'double: -100.0'].
	self should: [(classStatic new_double: 1000.0) getText_null asString = 'double: 1000.0'].
	self should: [(classStatic new_double: -1000.0) getText_null asString = 'double: -1000.0'].
	self should: [(classStatic new_double: 1000000.0) getText_null asString = 'double: 1000000.0'].
	self should: [(classStatic new_double: -1000000.0) getText_null asString = 'double: -1000000.0'].
	self should: [(classStatic new_double: 1000000000000.0) getText_null asString = 'double: 1.0E12'].		"JVM specific ??"
	self should: [(classStatic new_double: -1000000000000.0) getText_null asString = 'double: -1.0E12'].		"JVM specific ??"!

testFloatArgument

	self should: [(classStatic new_float: 0.0) getText_null asString = 'float: 0.0'].
	self should: [(classStatic new_float: 100.0) getText_null asString = 'float: 100.0'].
	self should: [(classStatic new_float: -100.0) getText_null asString = 'float: -100.0'].
	self should: [(classStatic new_float: 1000.0) getText_null asString = 'float: 1000.0'].
	self should: [(classStatic new_float: -1000.0) getText_null asString = 'float: -1000.0'].
	self should: [(classStatic new_float: 1000000.0) getText_null asString = 'float: 1000000.0'].
	self should: [(classStatic new_float: -1000000.0) getText_null asString = 'float: -1000000.0'].
!

testInheritedConstructor

	| new exception |

	"different behaviour between ghost classes and statically generated wrappers in this case"
	exception := jvm usesGhostClasses
				ifTrue: [Error "i.e. ShouldNotImplement"]
				ifFalse: [self noSuchMethodExceptionClass].

	"JRockit wrongly answers an instance of the superclass"
	self
		should: [new := self derivedClass new_int: 1]
		raise: exception.!

testInstantiateAbstractClass

	self
		should: [self abstractClass new_int: 1]
		raise: MessageNotUnderstood.!

testInstantiateAbstractClassWithDefaultConstructor

	| new exception |

	"different behaviour between ghost classes and statically generated wrappers in this case"
	exception := jvm usesGhostClasses
				ifTrue: [Error "i.e. ShouldNotImplement"]
				ifFalse: [self instantiationExceptionClass].

	self
		should: [new := self abstractClass new_null]
		raise: exception.!

testInstantiateConcreteClass

	| new |

	self
		shouldnt: [new := classStatic new_null]
		raise: MessageNotUnderstood.

	self should: [new static == classStatic].
	self should: [new isInstanceOf: classStatic].
	self should: [new getText_null asString = 'default'].
!

testInstantiateInterface

	self
		should: [self interface new_null]
		raise: MessageNotUnderstood.!

testIntArgument

	self should: [(classStatic new_int: 0) getText_null asString = 'int: 0'].
	self should: [(classStatic new_int: 100) getText_null asString = 'int: 100'].
	self should: [(classStatic new_int: -100) getText_null asString = 'int: -100'].
	self should: [(classStatic new_int: 1000) getText_null asString = 'int: 1000'].
	self should: [(classStatic new_int: -1000) getText_null asString = 'int: -1000'].
	self should: [(classStatic new_int: 1000000) getText_null asString = 'int: 1000000'].
	self should: [(classStatic new_int: -1000000) getText_null asString = 'int: -1000000'].
!

testLongArgument

	self should: [(classStatic new_long: 0) getText_null asString = 'long: 0'].
	self should: [(classStatic new_long: 100) getText_null asString = 'long: 100'].
	self should: [(classStatic new_long: -100) getText_null asString = 'long: -100'].
	self should: [(classStatic new_long: 1000) getText_null asString = 'long: 1000'].
	self should: [(classStatic new_long: -1000) getText_null asString = 'long: -1000'].
	self should: [(classStatic new_long: 1000000) getText_null asString = 'long: 1000000'].
	self should: [(classStatic new_long: -1000000) getText_null asString = 'long: -1000000'].
	self should: [(classStatic new_long: 1000000000000) getText_null asString = 'long: 1000000000000'].
	self should: [(classStatic new_long: -1000000000000) getText_null asString = 'long: -1000000000000'].!

testNoArguments

	self should: [(classStatic new_null) getText_null asString = 'default'].
!

testNullArgument

	self should: [(classStatic new_String: nil) getText_null asString = 'String: null'].
!

testOutOfRangeValues

	self should: [classStatic new_byte: 200] raise: Error.
	self should: [classStatic new_byte: 1000] raise: Error.
	self should: [classStatic new_short: 1000000] raise: Error.
	self should: [classStatic new_int: 6000000000] raise: Error.
	self should: [classStatic new_long: 100 factorial] raise: Error.
!

testPointArgument

	| jap p3x4 |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new_int: 3 int: 4.

	self should: [(classStatic new_Point: p3x4) getText_null asString = 'Point: java.awt.Point[x=3,y=4]'].		"JVM specific ??"!

testShortArgument

	self should: [(classStatic new_short: 0) getText_null asString = 'short: 0'].
	self should: [(classStatic new_short: 100) getText_null asString = 'short: 100'].
	self should: [(classStatic new_short: -100) getText_null asString = 'short: -100'].
	self should: [(classStatic new_short: 1000) getText_null asString = 'short: 1000'].
	self should: [(classStatic new_short: -1000) getText_null asString = 'short: -1000'].
!

testStringArgument

	self should: [(classStatic new_String: 'Hi there!!') getText_null asString = 'String: Hi there!!'].
!

testTwoArguments

	self should: [(classStatic new_int: 1 float: 1.5) getText_null asString = 'int: 1 float: 1.5'].
! !
!JNIPortGeneratedConstructorsTest categoriesFor: #abstractClass!constants!public! !
!JNIPortGeneratedConstructorsTest categoriesFor: #derivedClass!constants!public! !
!JNIPortGeneratedConstructorsTest categoriesFor: #interface!constants!public! !
!JNIPortGeneratedConstructorsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedConstructorsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedConstructorsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testBooleanArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testByteArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testCharArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testDerivedClassConstructor!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testDoubleArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testFloatArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testInheritedConstructor!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testInstantiateAbstractClass!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testInstantiateAbstractClassWithDefaultConstructor!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testInstantiateConcreteClass!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testInstantiateInterface!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testIntArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testLongArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testNoArguments!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testNullArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testPointArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testShortArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testStringArgument!public!unit tests! !
!JNIPortGeneratedConstructorsTest categoriesFor: #testTwoArguments!public!unit tests! !

JNIPortGeneratedFieldAccessTest guid: (GUID fromString: '{FF68ED6B-32A9-48C2-A663-74ABAE7D6ACD}')!
JNIPortGeneratedFieldAccessTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Note, we don''t test writing of "final" fields, ''cos it works in every case (i.e. JNI lets us do it) but it means nothing, so there''s
no point in testing it.'!
!JNIPortGeneratedFieldAccessTest categoriesForClass!Unclassified! !
!JNIPortGeneratedFieldAccessTest methodsFor!

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

testNoGettersGeneratedForNonpublicFields

	self shouldnt: [instance class includesSelector: #get_protectedIntField].
	self shouldnt: [instance class includesSelector: #get_defaultIntField].
	self shouldnt: [instance class includesSelector: #get_privateIntField].

	self shouldnt: [instance class includesSelector: #get_protectedFinalIntField].
	self shouldnt: [instance class includesSelector: #get_defaultFinalIntField].
	self shouldnt: [instance class includesSelector: #get_privateFinalIntField].

	self shouldnt: [classStatic class includesSelector: #get_protectedStaticIntField].
	self shouldnt: [classStatic class includesSelector: #get_defaultStaticIntField].
	self shouldnt: [classStatic class includesSelector: #get_privateStaticIntField].

	self shouldnt: [classStatic class includesSelector: #get_protectedStaticFinalIntField].
	self shouldnt: [classStatic class includesSelector: #get_defaultStaticFinalIntField].
	self shouldnt: [classStatic class includesSelector: #get_privateStaticFinalIntField].!

testNoSettersGeneratedForFinalFields

	self shouldnt: [instance class includesSelector: #set_publicFinalIntField:].
	self shouldnt: [instance class includesSelector: #set_protectedFinalIntField:].
	self shouldnt: [instance class includesSelector: #set_defaultFinalIntField:].
	self shouldnt: [instance class includesSelector: #set_privateFinalIntField:].

	self shouldnt: [classStatic class includesSelector: #set_publicStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_protectedStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_privateStaticFinalIntField:].
!

testNoSettersGeneratedForNonpublicFields

	self shouldnt: [instance class includesSelector: #set_protectedIntField:].
	self shouldnt: [instance class includesSelector: #set_defaultIntField:].
	self shouldnt: [instance class includesSelector: #set_privateIntField:].

	self shouldnt: [instance class includesSelector: #set_protectedFinalIntField:].
	self shouldnt: [instance class includesSelector: #set_defaultFinalIntField:].
	self shouldnt: [instance class includesSelector: #set_privateFinalIntField:].

	self shouldnt: [classStatic class includesSelector: #set_protectedStaticIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultStaticIntField:].
	self shouldnt: [classStatic class includesSelector: #set_privateStaticIntField:].

	self shouldnt: [classStatic class includesSelector: #set_protectedStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_privateStaticFinalIntField:].!

testReadPublicClassField

	self should: [classStatic get_publicStaticIntField = 101].!

testReadPublicFinalClassField

	self should: [classStatic get_publicStaticFinalIntField = 111].!

testReadPublicFinalInstanceField

	self should: [instance get_publicFinalIntField = 11].!

testReadPublicInstanceField

	self should: [instance get_publicIntField = 1].!

testWritePublicClassField

	classStatic set_publicStaticIntField: 0.
	self should: [classStatic get_publicStaticIntField = 0].
	classStatic set_publicStaticIntField: 101.
	self should: [classStatic get_publicStaticIntField = 101].
!

testWritePublicInstanceField

	self should: [instance get_publicIntField = 1].
	instance set_publicIntField: 0.
	self should: [instance get_publicIntField = 0].
! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testNoGettersGeneratedForNonpublicFields!public!reading!unit tests! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testNoSettersGeneratedForFinalFields!public!unit tests!writing! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testNoSettersGeneratedForNonpublicFields!public!unit tests!writing! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testReadPublicClassField!public!reading!unit tests! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testReadPublicFinalClassField!public!reading!unit tests! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testReadPublicFinalInstanceField!public!reading!unit tests! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testReadPublicInstanceField!public!reading!unit tests! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testWritePublicClassField!public!unit tests!writing! !
!JNIPortGeneratedFieldAccessTest categoriesFor: #testWritePublicInstanceField!public!unit tests!writing! !

JNIPortGeneratedFieldsTest guid: (GUID fromString: '{05EE1E87-F8E9-4DFE-A60D-13DF709EDF6A}')!
JNIPortGeneratedFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedFieldsTest methodsFor!

dontTestTypeErrors

	"unfortunately we can't use this test since the JVM doesn't actually check the types
	for fields written via JNI"

	| jap |

	jap := jvm findClass: #'java.awt.Point'.
	self
		should: [subject set_stringField: jap new]
		raise: Error.
	self should: [(subject get_stringField) asString = 'Hi there'].!

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

	self should: [subject get_booleanField = true].

	subject set_booleanField: false.
	self should: [subject get_booleanField = false].
	self should: [subject callBooleanMethod: 'isBooleanFalse'].

	subject set_booleanField: true.
	self should: [subject get_booleanField = true].
	self should: [subject callBooleanMethod: 'isBooleanTrue'].
!

testAccessByteField

	self should: [subject get_byteField = 22].

	subject set_byteField: 0.
	self should: [(subject getByteField: 'byteField') = 0].
	self should: [subject callBooleanMethod: 'isByteZero'].

	subject set_byteField: 33.
	self should: [subject get_byteField = 33].
	self should: [subject callBooleanMethod: 'isByte33'].

	subject set_byteField: -33.
	self should: [subject get_byteField = -33].
	self should: [subject callBooleanMethod: 'isByteMinus33'].

	subject set_byteField: 22.
	self should: [subject get_byteField = 22].
!

testAccessCharField

	self should: [subject get_charField = $!!].

	subject set_charField: $M.
	self should: [subject get_charField = $M].
	self should: [subject callBooleanMethod: 'isCharM'].

	subject set_charField: 0.
	self should: [subject get_charField = (Character codePoint: 0)].
	self should: [subject callBooleanMethod: 'isCharZero'].

	subject set_charField: 16rA.
	self should: [subject get_charField = (Character codePoint: 16rA)].
	self should: [subject callBooleanMethod: 'isChar0xA'].

	subject set_charField: 16rAA.
	self should: [subject get_charField = (Character codePoint: 16rAA)].
	self should: [subject callBooleanMethod: 'isChar0xAA'].

	subject set_charField: 16rAAA.
	self should: [subject get_charField = 16rAAA].
	self should: [subject callBooleanMethod: 'isChar0xAAA'].

	subject set_charField: 16rAAAA.
	self should: [subject get_charField = 16rAAAA].
	self should: [subject callBooleanMethod: 'isChar0xAAAA'].

	subject set_charField: $!!.
	self should: [subject get_charField = $!!].
!

testAccessDoubleField

	self should: [subject get_doubleField = 22.0].

	subject set_doubleField: 0.0.
	self should: [subject get_doubleField = 0.0].
	self should: [subject callBooleanMethod: 'isDoubleZero'].

	subject set_doubleField: 33.0.
	self should: [subject get_doubleField = 33.0].
	self should: [subject callBooleanMethod: 'isDouble33'].

	subject set_doubleField: -33.0.
	self should: [subject get_doubleField = -33.0].
	self should: [subject callBooleanMethod: 'isDoubleMinus33'].

	subject set_doubleField: 33.		"NB: passing an Integer, expecting coercion to double"
	self should: [subject get_doubleField = 33.0].
	self should: [subject callBooleanMethod: 'isDouble33'].

	subject set_doubleField: 22.0.
	self should: [subject get_doubleField = 22.0].
!

testAccessFloatField

	self should: [subject get_floatField = 22.0].

	subject set_floatField: 0.0.
	self should: [subject get_floatField = 0.0].
	self should: [subject callBooleanMethod: 'isFloatZero'].

	subject set_floatField: 33.0.
	self should: [subject get_floatField = 33.0].
	self should: [subject callBooleanMethod: 'isFloat33'].

	subject set_floatField: -33.0.
	self should: [subject get_floatField = -33.0].
	self should: [subject callBooleanMethod: 'isFloatMinus33'].

	subject set_floatField: 33.		"NB: passing an Integer, expecting coercion to float"
	self should: [subject get_floatField = 33.0].
	self should: [subject callBooleanMethod: 'isFloat33'].

	subject set_floatField: 22.0.
	self should: [subject get_floatField = 22.0].
!

testAccessIntField

	self should: [subject get_intField = 22].

	subject set_intField: 0.
	self should: [(subject getIntField: 'intField') = 0].
	self should: [subject callBooleanMethod: 'isIntZero'].

	subject set_intField: 33.
	self should: [subject get_intField = 33].
	self should: [subject callBooleanMethod: 'isInt33'].

	subject set_intField: -33.
	self should: [subject get_intField = -33].
	self should: [subject callBooleanMethod: 'isIntMinus33'].

	subject set_intField: 22.
	self should: [subject get_intField = 22].
!

testAccessLongField

	self should: [subject get_longField = 22].

	subject set_longField: 0.
	self should: [(subject getLongField: 'longField') = 0].
	self should: [subject callBooleanMethod: 'isLongZero'].

	subject set_longField: 33.
	self should: [subject get_longField = 33].
	self should: [subject callBooleanMethod: 'isLong33'].

	subject set_longField: -33.
	self should: [subject get_longField = -33].
	self should: [subject callBooleanMethod: 'isLongMinus33'].

	subject set_longField: 22.
	self should: [subject get_longField = 22].
!

testAccessNonExistantField

	| exceptionClass |

	exceptionClass := MessageNotUnderstood.

	self
		should: [subject get_nonExistantField]
		raise: exceptionClass.

	self
		should: [subject set_nonExistantField: nil]
		raise: exceptionClass.!

testAccessPointField

	| jap p3x4 p33x45 point |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new_int: 3 int: 4.

	point := subject get_pointField.
	self should: [point equals: p3x4].

	subject set_pointField: nil.
	self should: [subject get_pointField = nil].
	self should: [subject callBooleanMethod: 'isPointNull'].

	subject set_pointField: jap new.
	self should: [subject callBooleanMethod: 'isPointZero'].

	p33x45 := jap new_int: 33 int: 45.
	subject set_pointField: p33x45.
	self should: [subject get_pointField = p33x45].
	self should: [subject callBooleanMethod: 'isPoint33x45'].

	subject set_pointField: point.
	self should: [subject get_pointField = point].
!

testAccessShortField

	self should: [subject get_shortField = 22].

	subject set_shortField: 0.
	self should: [(subject getShortField: 'shortField') = 0].
	self should: [subject callBooleanMethod: 'isShortZero'].

	subject set_shortField: 33.
	self should: [subject get_shortField = 33].
	self should: [subject callBooleanMethod: 'isShort33'].

	subject set_shortField: -33.
	self should: [subject get_shortField = -33].
	self should: [subject callBooleanMethod: 'isShortMinus33'].

	subject set_shortField: 22.
	self should: [subject get_shortField = 22].
!

testAccessStringField

	self should: [subject get_stringField asString = 'Hi there'].

	subject set_stringField: nil.
	self should: [subject get_stringField = nil].
	self should: [subject callBooleanMethod: 'isStringNull'].

	subject set_stringField: '33'.
	self should: [subject get_stringField asString = '33'].
	self should: [subject callBooleanMethod: 'isString33'].

	subject set_stringField: ''.
	self should: [subject get_stringField isEmpty].
	self should: [subject callBooleanMethod: 'isStringEmpty'].

	subject set_stringField: 'Hi there'.
	self should: [subject get_stringField asString = 'Hi there'].
!

testOutOfRangeValues

	self
		should: [subject set_byteField: 200]
		raise: Error.
	self should: [subject get_byteField = 22].

	self
		should: [subject set_byteField: 1000]
		raise: Error.
	self should: [subject get_byteField = 22].

	self
		should: [subject set_shortField: 1000000]
		raise: Error.
	self should: [subject get_shortField = 22].

	self
		should: [subject set_intField: 6000000000]
		raise: Error.
	self should: [subject get_intField = 22].

	self
		should: [subject set_longField: 100 factorial]
		raise: Error.
	self should: [subject get_longField = 22].! !
!JNIPortGeneratedFieldsTest categoriesFor: #dontTestTypeErrors!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedFieldsTest categoriesFor: #subject!helpers!public! !
!JNIPortGeneratedFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessBooleanField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessByteField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessCharField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessDoubleField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessFloatField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessIntField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessLongField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessNonExistantField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessPointField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessShortField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testAccessStringField!public!unit tests! !
!JNIPortGeneratedFieldsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !

!JNIPortGeneratedFieldsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortGeneratedFieldsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortGeneratedHiddenFieldsTest guid: (GUID fromString: '{E48FD011-C247-4A83-8F3D-076EAE8DC13E}')!
JNIPortGeneratedHiddenFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedHiddenFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedHiddenFieldsTest methodsFor!

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

testClassFieldIsInherited

	self should: [classStatic get_staticBooleanField = true].!

testNoSettersGenerated

	self shouldnt: [classStatic class includesSelector: #set_interfaceIntField:].
!

testOverloadedByTypeField

	| class |

	class := self overloadedByTypeClass.
	class isNil ifTrue:
		[Notification signal: (Processor activeProcess topFrame method displayString , ' not run (classfile generation is not installed?)').
		^ self].

	self should: [class get_field] raise: AmbiguousJavaMethodError.
	self should: [class get_int_field = 33].
	self should: [class get_boolean_field = false].
!

testReadAmbiguousField

	"NB: we should be reading the field in the *interface* "

	"JRockit wrongly picks up the class field"

	self should: [classStatic get_ambiguousIntField = -100].!

testReadClassField

	self should: [classStatic get_staticIntField = 666].!

testReadInstanceField

	self should: [instance get_intField = 6].!

testReadInterfaceField

	self should: [classStatic get_interfaceIntField = 66].!

testWriteAmbiguousField

	"NB: we would be writing the field in the *interface* "

	self
		should: [classStatic set_ambiguousIntField: 0]
		raise: Error.
!

testWriteClassField

	classStatic set_staticIntField: 0.
	self should: [classStatic get_staticIntField = 0].
	classStatic set_staticIntField: 666.
	self should: [classStatic get_staticIntField = 666].
!

testWriteInstanceField

	self should: [instance get_intField = 6].
	instance set_intField: 0.
	self should: [instance get_intField = 0].
! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testClassFieldIsInherited!public!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testNoSettersGenerated!public!unit tests!writing! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testOverloadedByTypeField!public!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testReadAmbiguousField!public!reading!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testReadClassField!public!reading!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testReadInstanceField!public!reading!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testReadInterfaceField!public!reading!unit tests! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testWriteAmbiguousField!public!unit tests!writing! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testWriteClassField!public!unit tests!writing! !
!JNIPortGeneratedHiddenFieldsTest categoriesFor: #testWriteInstanceField!public!unit tests!writing! !

JNIPortGeneratedHiddenMethodsTest guid: (GUID fromString: '{CA9659F2-A8C6-4C3F-BCC2-6F3DD3104A90}')!
JNIPortGeneratedHiddenMethodsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedHiddenMethodsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedHiddenMethodsTest methodsFor!

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

	self should: [classStatic staticIntMethod_null = 666].!

testCallInstanceMethod

	self should: [instance intMethod_null = 6].!

testCallMethodAsAnInterface

	| asA |

	asA := instance asInstanceOf: self interface.

	"different behaviour between ghost classes and statically generated wrappers in this case"
	jvm usesGhostClasses
		ifTrue:
			["since #as: doesn't fit with ghost clases, this is supposed to DNU,
			unless a static wrapper for the interface has also been generated
			or ghost 'instance' wrappers are enabled for interfaces"
			self
				should: [asA interfaceIntMethod_null = 6666]
				raise: MessageNotUnderstood]
		ifFalse:
			[self should: [asA interfaceIntMethod_null = 6666]].!

testCallSuperClassMethod

	self should: [classStatic super staticIntMethod_null = 222].!

testCallSuperInstanceMethod

	"since #super doesn't fit with ghost clases, this is supposed to DNU"
	self
		should: [instance super intMethod_null = 2]
		raise: MessageNotUnderstood.
!

testClassMethodIsInherited

	self should: [classStatic staticBooleanMethod_null = true].!

testOverloadedByTypeMethod

	| class subject |

	class := self overloadedByTypeClass.
	class isNil ifTrue:
		[Notification signal: (Processor activeProcess topFrame method displayString , ' not run (classfile generation is not installed?)').
		^ self].

	subject := class new.
	self should: [subject method_null] raise: AmbiguousJavaMethodError.
	self should: [subject method_int_null = 22].
	self should: [subject method_double_null = 10.0].
! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #interface!constants!public! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testCallClassMethod!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testCallInstanceMethod!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testCallMethodAsAnInterface!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testCallSuperClassMethod!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testCallSuperInstanceMethod!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testClassMethodIsInherited!public!unit tests! !
!JNIPortGeneratedHiddenMethodsTest categoriesFor: #testOverloadedByTypeMethod!public!unit tests! !

JNIPortGeneratedInheritedFieldsTest guid: (GUID fromString: '{1A720086-B111-44A1-903B-D8F7C3D415EC}')!
JNIPortGeneratedInheritedFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInheritedFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInheritedFieldsTest methodsFor!

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

	self should: [classStatic get_staticIntField = 222].!

testReadInstanceField

	self should: [instance get_intField = 2].!

testWriteClassField

	classStatic set_staticIntField: 0.
	self should: [classStatic get_staticIntField = 0].
	classStatic set_staticIntField: 222.
	self should: [classStatic get_staticIntField = 222].
!

testWriteInstanceField

	self should: [instance get_intField = 2].
	instance set_intField: 0.
	self should: [instance get_intField = 0].
! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #testReadClassField!public!reading!unit tests! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #testReadInstanceField!public!reading!unit tests! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #testWriteClassField!public!unit tests!writing! !
!JNIPortGeneratedInheritedFieldsTest categoriesFor: #testWriteInstanceField!public!unit tests!writing! !

JNIPortGeneratedInheritedMethodsTest guid: (GUID fromString: '{C843A389-4B25-4E72-B5DC-57A406794CB1}')!
JNIPortGeneratedInheritedMethodsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInheritedMethodsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInheritedMethodsTest methodsFor!

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

	self should: [instance abstractIntMethod_null = 22].!

testCallClassMethod

	self should: [classStatic staticIntMethod_null = 222].!

testCallInstanceMethod

	self should: [instance intMethod_null = 2].!

testCallInterfaceMethod

	self should: [instance interfaceIntMethod_null = 2222].! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #testCallAbstractMethod!public!unit tests! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #testCallClassMethod!public!unit tests! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #testCallInstanceMethod!public!unit tests! !
!JNIPortGeneratedInheritedMethodsTest categoriesFor: #testCallInterfaceMethod!public!unit tests! !

JNIPortGeneratedInhertedInterfaceFieldsTest guid: (GUID fromString: '{C28659E8-5915-45D1-9680-A3786B64B110}')!
JNIPortGeneratedInhertedInterfaceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInhertedInterfaceFieldsTest methodsFor!

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

testNoSettersGenerated

	self shouldnt: [classStatic class includesSelector: #set_interfaceIntField:].
!

testReadField

	self should: [classStatic get_interfaceIntField = 33].! !
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesFor: #testNoSettersGenerated!public!unit tests!writing! !
!JNIPortGeneratedInhertedInterfaceFieldsTest categoriesFor: #testReadField!public!reading!unit tests! !

JNIPortGeneratedInterfaceFieldsTest guid: (GUID fromString: '{C5D01F31-92D5-401E-93B5-4B7BA92736BA}')!
JNIPortGeneratedInterfaceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInterfaceFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInterfaceFieldsTest methodsFor!

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

testNoSettersGenerated

	self shouldnt: [classStatic class includesSelector: #set_publicIntField:].
	self shouldnt: [classStatic class includesSelector: #set_publicFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_publicStaticIntField:].
	self shouldnt: [classStatic class includesSelector: #set_publicStaticFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultFinalIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultStaticIntField:].
	self shouldnt: [classStatic class includesSelector: #set_defaultStaticFinalIntField:].

!

testReadFields

	self should: [classStatic get_publicIntField = 1].
	self should: [classStatic get_publicFinalIntField = 2].
	self should: [classStatic get_publicStaticIntField = 3].
	self should: [classStatic get_publicStaticFinalIntField = 4].
	self should: [classStatic get_defaultIntField = 5].
	self should: [classStatic get_defaultFinalIntField = 6].
	self should: [classStatic get_defaultStaticIntField = 7].
	self should: [classStatic get_defaultStaticFinalIntField = 8].
! !
!JNIPortGeneratedInterfaceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInterfaceFieldsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedInterfaceFieldsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedInterfaceFieldsTest categoriesFor: #testNoSettersGenerated!public!unit tests!writing! !
!JNIPortGeneratedInterfaceFieldsTest categoriesFor: #testReadFields!public!unit tests! !

JNIPortGeneratedMethodAccessTest guid: (GUID fromString: '{7153C94C-29F1-4829-BFF5-4FFC4040BEB8}')!
JNIPortGeneratedMethodAccessTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedMethodAccessTest categoriesForClass!Unclassified! !
!JNIPortGeneratedMethodAccessTest methodsFor!

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

testCallPublicClassMethod

	self should: [classStatic publicStaticIntMethod_null = 101].!

testCallPublicFinalClassMethod

	self should: [classStatic publicStaticFinalIntMethod_null = 111].!

testCallPublicFinalInstanceMethod

	self should: [instance publicFinalIntMethod_null = 11].!

testCallPublicInstanceMethod

	self should: [instance publicIntMethod_null = 1].!

testNoWrappersGeneratedForNonpublicMethods

	self shouldnt: [instance class includesSelector: #protectedIntMethod_null].
	self shouldnt: [instance class includesSelector: #defaultIntMethod_null].
	self shouldnt: [instance class includesSelector: #privateIntMethod_null].

	self shouldnt: [instance class includesSelector: #protectedFinalIntMethod_null].
	self shouldnt: [instance class includesSelector: #defaultFinalIntMethod_null].
	self shouldnt: [instance class includesSelector: #privateFinalIntMethod_null].

	self shouldnt: [classStatic class includesSelector: #protectedStaticIntMethod_null].
	self shouldnt: [classStatic class includesSelector: #defaultStaticIntMethod_null].
	self shouldnt: [classStatic class includesSelector: #privateStaticIntMethod_null].

	self shouldnt: [classStatic class includesSelector: #protectedStaticFinalIntMethod_null].
	self shouldnt: [classStatic class includesSelector: #defaultStaticFinalIntMethod_null].
	self shouldnt: [classStatic class includesSelector: #privateStaticFinalIntMethod_null].! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #testCallPublicClassMethod!public!unit tests! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #testCallPublicFinalClassMethod!public!unit tests! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #testCallPublicFinalInstanceMethod!public!unit tests! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #testCallPublicInstanceMethod!public!unit tests! !
!JNIPortGeneratedMethodAccessTest categoriesFor: #testNoWrappersGeneratedForNonpublicMethods!public!unit tests! !

JNIPortGeneratedMethodArgumentsTest guid: (GUID fromString: '{1360CA89-91E5-4B17-8E47-AEABEF184A49}')!
JNIPortGeneratedMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedMethodArgumentsTest methodsFor!

dontTestTypeErrors

	"unfortunately we can't use this test since the JVM doesn't actually check the types
	and therefore tends to crash..."

	| string |

	string := 'Hi there' asJavaString: jvm.
	self
		should: [subject method_Point: string]
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

	self should: [(subject method_boolean: true) asString = 'boolean: true'].
	self should: [(subject method_boolean: false) asString = 'boolean: false'].
!

testByteArgument

	self should: [(subject method_byte: 0) asString = 'byte: 0'].
	self should: [(subject method_byte: 100) asString = 'byte: 100'].
	self should: [(subject method_byte: -100) asString = 'byte: -100'].
!

testCharArgument

	self should: [(subject method_char: $!!) asString = 'char: !!'].
	self should: [(subject method_char: $A) asString = 'char: A'].!

testDoubleArgument

	self should: [(subject method_double: 0.0) asString = 'double: 0.0'].
	self should: [(subject method_double: 100.0) asString = 'double: 100.0'].
	self should: [(subject method_double: -100.0) asString = 'double: -100.0'].
	self should: [(subject method_double: 1000.0) asString = 'double: 1000.0'].
	self should: [(subject method_double: -1000.0) asString = 'double: -1000.0'].
	self should: [(subject method_double: 1000000.0) asString = 'double: 1000000.0'].
	self should: [(subject method_double: -1000000.0) asString = 'double: -1000000.0'].
	self should: [(subject method_double: 1000000000000.0) asString = 'double: 1.0E12'].		"JVM specific ??"
	self should: [(subject method_double: -1000000000000.0) asString = 'double: -1.0E12'].		"JVM specific ??"!

testFloatArgument

	self should: [(subject method_float: 0.0) asString = 'float: 0.0'].
	self should: [(subject method_float: 100.0) asString = 'float: 100.0'].
	self should: [(subject method_float: -100.0) asString = 'float: -100.0'].
	self should: [(subject method_float: 1000.0) asString = 'float: 1000.0'].
	self should: [(subject method_float: -1000.0) asString = 'float: -1000.0'].
	self should: [(subject method_float: 1000000.0) asString = 'float: 1000000.0'].
	self should: [(subject method_float: -1000000.0) asString = 'float: -1000000.0'].
!

testIntArgument

	self should: [(subject method_int: 0) asString = 'int: 0'].
	self should: [(subject method_int: 100) asString = 'int: 100'].
	self should: [(subject method_int: -100) asString = 'int: -100'].
	self should: [(subject method_int: 1000) asString = 'int: 1000'].
	self should: [(subject method_int: -1000) asString = 'int: -1000'].
	self should: [(subject method_int: 1000000) asString = 'int: 1000000'].
	self should: [(subject method_int: -1000000) asString = 'int: -1000000'].
!

testLongArgument

	self should: [(subject method_long: 0) asString = 'long: 0'].
	self should: [(subject method_long: 100) asString = 'long: 100'].
	self should: [(subject method_long: -100) asString = 'long: -100'].
	self should: [(subject method_long: 1000) asString = 'long: 1000'].
	self should: [(subject method_long: -1000) asString = 'long: -1000'].
	self should: [(subject method_long: 1000000) asString = 'long: 1000000'].
	self should: [(subject method_long: -1000000) asString = 'long: -1000000'].
	self should: [(subject method_long: 1000000000000) asString = 'long: 1000000000000'].
	self should: [(subject method_long: -1000000000000) asString = 'long: -1000000000000'].!

testNoArguments

	self should: [(subject method_null) asString = '<nothing>'].
!

testNullArgument

	self should: [(subject method_String: nil) asString = 'String: null'].
!

testOutOfRangeValues

	self should: [subject method_byte: 200] raise: Error.
	self should: [subject method_byte: 1000] raise: Error.
	self should: [subject method_short: 1000000] raise: Error.
	self should: [subject method_int: 6000000000] raise: Error.
	self should: [subject method_long: 100 factorial] raise: Error.
!

testPointArgument

	| jap p3x4 |

	jap := jvm findClass: #'java.awt.Point'.
	p3x4 := jap new_int: 3 int: 4.

	self should: [(subject method_Point: p3x4) asString = 'Point: java.awt.Point[x=3,y=4]'].		"JVM specific ??"!

testShortArgument

	self should: [(subject method_short: 0) asString = 'short: 0'].
	self should: [(subject method_short: 100) asString = 'short: 100'].
	self should: [(subject method_short: -100) asString = 'short: -100'].
	self should: [(subject method_short: 1000) asString = 'short: 1000'].
	self should: [(subject method_short: -1000) asString = 'short: -1000'].
!

testStringArgument

	self should: [(subject method_String: 'Hi there!!') asString = 'String: Hi there!!'].
!

testTwoArguments

	self should: [(subject method_int: 1 float: 1.5) asString = 'int: 1 float: 1.5'].
! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #dontTestTypeErrors!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #subject!helpers!public! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testBooleanArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testByteArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testCharArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testDoubleArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testFloatArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testIntArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testLongArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testNoArguments!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testNullArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testOutOfRangeValues!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testPointArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testShortArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testStringArgument!public!unit tests! !
!JNIPortGeneratedMethodArgumentsTest categoriesFor: #testTwoArguments!public!unit tests! !

!JNIPortGeneratedMethodArgumentsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortGeneratedMethodArgumentsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortGeneratedMethodReturnsTest guid: (GUID fromString: '{4D2ADE4E-EDBA-4CC9-8521-D8597FE9C0A4}')!
JNIPortGeneratedMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedMethodReturnsTest methodsFor!

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

	self should: [subject booleanMethod_null = true].
!

testByteMethodReturn

	self should: [subject byteMethod_null = 20].
!

testCallNonExistantMethod

	self
		should: [subject nonExistantMethod_null]
		raise: MessageNotUnderstood.!

testCharMethodReturn

	self should: [subject charMethod_null = $!!].!

testDoubleMethodReturn

	self should: [subject doubleMethod_null = 25.0].!

testFloatMethodReturn

	self should: [subject floatMethod_null = 24.0].!

testIntMethodReturn

	self should: [subject intMethod_null = 22].!

testLongMethodReturn

	self should: [subject longMethod_null = 23].!

testNullMethodReturn

	self should: [subject nullMethod_null isNil].!

testPointMethodReturn

	| point |

	point := subject pointMethod_null.
	self should: [point get_x = 3].
	self should: [point get_y = 4].
!

testShortMethodReturn

	self should: [subject shortMethod_null = 21].!

testStringMethodReturn

	self should: [subject stringMethod_null asString = 'Hi there'].!

testVoidMethodReturn

	self should: [subject voidMethod_null == subject].! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #setUp!public!running! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #subject!helpers!public! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #tearDown!public!running! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testBooleanMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testByteMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testCallNonExistantMethod!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testCharMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testDoubleMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testFloatMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testIntMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testLongMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testNullMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testPointMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testShortMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testStringMethodReturn!public!unit tests! !
!JNIPortGeneratedMethodReturnsTest categoriesFor: #testVoidMethodReturn!public!unit tests! !

!JNIPortGeneratedMethodReturnsTest class methodsFor!

isAbstract

	^self = ##(self).! !
!JNIPortGeneratedMethodReturnsTest class categoriesFor: #isAbstract!public!testing! !

JNIPortGeneratedInstanceFieldsTest guid: (GUID fromString: '{54B1537E-FA25-4CB1-9A84-D6BF1B08D934}')!
JNIPortGeneratedInstanceFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInstanceFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInstanceFieldsTest methodsFor!

javaTestClassName

	^ 'InstanceFields'.!

subject

	^ self javaTestClass new.! !
!JNIPortGeneratedInstanceFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInstanceFieldsTest categoriesFor: #subject!helpers!public! !

JNIPortGeneratedStaticFieldsTest guid: (GUID fromString: '{36C8D564-7A16-4069-9830-2144231087E7}')!
JNIPortGeneratedStaticFieldsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedStaticFieldsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedStaticFieldsTest methodsFor!

javaTestClassName

	^ 'StaticFields'.!

subject

	^ self javaTestClass.! !
!JNIPortGeneratedStaticFieldsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedStaticFieldsTest categoriesFor: #subject!helpers!public! !

JNIPortGeneratedInstanceMethodArgumentsTest guid: (GUID fromString: '{F3881387-5CA5-4FB4-892C-D5C20F77A45B}')!
JNIPortGeneratedInstanceMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInstanceMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInstanceMethodArgumentsTest methodsFor!

javaTestClassName

	^ 'InstanceMethodArguments'.!

subject

	^ self javaTestClass new.! !
!JNIPortGeneratedInstanceMethodArgumentsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInstanceMethodArgumentsTest categoriesFor: #subject!helpers!public! !

JNIPortGeneratedStaticMethodArgumentsTest guid: (GUID fromString: '{1386BAB6-9B9F-41C4-B5B2-E5381E0E6C60}')!
JNIPortGeneratedStaticMethodArgumentsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedStaticMethodArgumentsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedStaticMethodArgumentsTest methodsFor!

javaTestClassName

	^ 'StaticMethodArguments'.!

subject

	^ self javaTestClass.! !
!JNIPortGeneratedStaticMethodArgumentsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedStaticMethodArgumentsTest categoriesFor: #subject!helpers!public! !

JNIPortGeneratedInstanceMethodReturnsTest guid: (GUID fromString: '{D762EA7D-67FE-42E1-83A2-6F4AF5D7A596}')!
JNIPortGeneratedInstanceMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedInstanceMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedInstanceMethodReturnsTest methodsFor!

javaTestClassName

	^ 'InstanceMethods'.!

subject

	^ self javaTestClass new.! !
!JNIPortGeneratedInstanceMethodReturnsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedInstanceMethodReturnsTest categoriesFor: #subject!helpers!public! !

JNIPortGeneratedStaticMethodReturnsTest guid: (GUID fromString: '{0DA42C2F-07A9-4AEC-9BC9-87E56E179A7E}')!
JNIPortGeneratedStaticMethodReturnsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortGeneratedStaticMethodReturnsTest categoriesForClass!Unclassified! !
!JNIPortGeneratedStaticMethodReturnsTest methodsFor!

javaTestClassName

	^ 'StaticMethods'.!

subject

	^ self javaTestClass.! !
!JNIPortGeneratedStaticMethodReturnsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortGeneratedStaticMethodReturnsTest categoriesFor: #subject!helpers!public! !

"Binary Globals"!

"Resources"!

