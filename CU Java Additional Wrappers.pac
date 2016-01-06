| package |
package := Package name: 'CU Java Additional Wrappers'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Some extra wrapper classes to add to the ''core'' collection in JNIPort (package ''CU Java Base'').  None of these are necessary to use JNIPort but they should be useful in themselves or as examples.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #JavaContainerInterface;
	add: #JavaEnumerationAdaptor;
	add: #JavaExternalIterator;
	add: #JavaIoInputStream;
	add: #JavaIoOutputStream;
	add: #JavaIoReader;
	add: #JavaIoWriter;
	add: #JavaIteratorAdaptor;
	add: #JavaLangError;
	add: #JavaLangException;
	add: #JavaLangNumber;
	add: #JavaLangRuntime;
	add: #JavaReadStream;
	add: #JavaUtilCollection;
	add: #JavaUtilComparator;
	add: #JavaUtilEnumeration;
	add: #JavaUtilIterator;
	add: #JavaUtilList;
	add: #JavaUtilListIterator;
	add: #JavaUtilMap;
	add: #JavaUtilSet;
	add: #JavaUtilSortedMap;
	add: #JavaUtilSortedSet;
	add: #JavaWriteListIteratorAdaptor;
	add: #JavaWriteStream;
	add: #StaticJavaLangByte;
	add: #StaticJavaLangDouble;
	add: #StaticJavaLangFloat;
	add: #StaticJavaLangInteger;
	add: #StaticJavaLangLong;
	add: #StaticJavaLangNumber;
	add: #StaticJavaLangShort;
	yourself.

package methodNames
	add: #JavaInstance -> #asAList;
	add: #JavaInstance -> #asAListIterator;
	add: #JavaInstance -> #asAMap;
	add: #JavaInstance -> #asAnEnumeration;
	add: #JavaInstance -> #asAnIterator;
	add: #JavaInstance -> #asASet;
	add: #JavaInstance -> #asASortedMap;
	add: #JavaInstance -> #asASortedSet;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Abstract Collections';
	add: 'CU Collection Adaptors';
	add: 'CU Java Base';
	add: 'CU JNI';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

JavaLangObject subclass: #JavaLangNumber
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaLangRuntime
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #JavaWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangThrowable subclass: #JavaLangError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangThrowable subclass: #JavaLangException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaReadStream subclass: #JavaIoInputStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaReadStream subclass: #JavaIoReader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaWriteStream subclass: #JavaIoOutputStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaWriteStream subclass: #JavaIoWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInterfaceInstance subclass: #JavaContainerInterface
	instanceVariableNames: 'adaptorCache'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInterfaceInstance subclass: #JavaExternalIterator
	instanceVariableNames: 'adaptor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaInterfaceInstance subclass: #JavaUtilComparator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaContainerInterface subclass: #JavaUtilCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaContainerInterface subclass: #JavaUtilMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaUtilCollection subclass: #JavaUtilList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaUtilCollection subclass: #JavaUtilSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaUtilSet subclass: #JavaUtilSortedSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaUtilMap subclass: #JavaUtilSortedMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaExternalIterator subclass: #JavaUtilEnumeration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaExternalIterator subclass: #JavaUtilIterator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaUtilIterator subclass: #JavaUtilListIterator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticJavaLangNumber
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangByte
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangDouble
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangFloat
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangLong
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangNumber subclass: #StaticJavaLangShort
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractReadStream subclass: #JavaEnumerationAdaptor
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractReadStream subclass: #JavaIteratorAdaptor
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractWriteStream subclass: #JavaWriteListIteratorAdaptor
	instanceVariableNames: 'subject'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaInstance methodsFor!

asAList
	"answer a JavaUtilSet wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.List"

	^ self asA: #'java.util.List'.!

asAListIterator
	"answer a JavaUtilListIterator wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.ListIterator"

	^ self asA: #'java.util.ListIterator'.!

asAMap
	"answer a JavaUtilMap wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.Map"

	^ self asA: #'java.util.Map'.!

asAnEnumeration
	"answer a JavaUtilEnumeration wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.Enumeration"

	^ self asA: #'java.util.Enumeration'.!

asAnIterator
	"answer a JavaUtilIterator wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.Iterator"

	^ self asA: #'java.util.Iterator'.!

asASet
	"answer a JavaUtilSet wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.Set"

	^ self asA: #'java.util.Set'.!

asASortedMap
	"answer a JavaUtilSortedMap wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.SortedMap"

	^ self asA: #'java.util.SortedMap'.!

asASortedSet
	"answer a JavaUtilSortedSet wrapping the same underling object as ourselves.
	NB: there is no check that our underlyng object formally implements java.util.SortedSet"

	^ self asA: #'java.util.SortedSet'.! !
!JavaInstance categoriesFor: #asAList!converting!public! !
!JavaInstance categoriesFor: #asAListIterator!converting!public! !
!JavaInstance categoriesFor: #asAMap!converting!public! !
!JavaInstance categoriesFor: #asAnEnumeration!converting!public! !
!JavaInstance categoriesFor: #asAnIterator!converting!public! !
!JavaInstance categoriesFor: #asASet!converting!public! !
!JavaInstance categoriesFor: #asASortedMap!converting!public! !
!JavaInstance categoriesFor: #asASortedSet!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JavaLangNumber guid: (GUID fromString: '{CBF4E3CE-9158-47E7-83D7-DF464C97E7B8}')!
JavaLangNumber comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of any of the concrete subclasses of Java class java.lang.Number'!
!JavaLangNumber categoriesForClass!Unclassified! !
!JavaLangNumber methodsFor!

byteValue_null
	"answer the result of calling the receiver's public byteValue() Java method"

	^ self callByteMethod: 'byteValue'.
!

doubleValue_null
	"answer the result of calling the receiver's public abstract doubleValue() Java method"

	^ self callDoubleMethod: 'doubleValue'.
!

floatValue_null
	"answer the result of calling the receiver's public abstract floatValue() Java method"

	^ self callFloatMethod: 'floatValue'.
!

intValue_null
	"answer the result of calling the receiver's public abstract intValue() Java method"

	^ self callIntMethod: 'intValue'.
!

longValue_null
	"answer the result of calling the receiver's public abstract longValue() Java method"

	^ self callLongMethod: 'longValue'.
!

shortValue_null
	"answer the result of calling the receiver's public shortValue() Java method"

	^ self callShortMethod: 'shortValue'.
! !
!JavaLangNumber categoriesFor: #byteValue_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangNumber categoriesFor: #doubleValue_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaLangNumber categoriesFor: #floatValue_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaLangNumber categoriesFor: #intValue_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaLangNumber categoriesFor: #longValue_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaLangNumber categoriesFor: #shortValue_null!**auto generated**!Java-methods!Java-public!public! !

!JavaLangNumber class methodsFor!

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
		#byteValue_null
		#doubleValue_null
		#floatValue_null
		#intValue_null
		#longValue_null
		#shortValue_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Number'.
! !
!JavaLangNumber class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangNumber class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangNumber class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangNumber class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangNumber class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaLangRuntime guid: (GUID fromString: '{A236CA7A-3D69-4FA1-A40C-082B469D7FBD}')!
JavaLangRuntime comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.Runtime'!
!JavaLangRuntime categoriesForClass!Unclassified! !
!JavaLangRuntime methodsFor!

addShutdownHook_Thread: aThread1
	"invoke the receiver's public addShutdownHook(java.lang.Thread) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThread1;
			yourself.

	self callVoidMethod: 'addShutdownHook' signature: '(Ljava/lang/Thread;)V' withArguments: args.
!

availableProcessors_null
	"answer the result of calling the receiver's public native availableProcessors() Java method"

	^ self callIntMethod: 'availableProcessors'.
!

exec_String: aString1
	"answer the result of calling the receiver's public exec(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'exec' signature: '(Ljava/lang/String;)Ljava/lang/Process;' withArguments: args.
!

exec_String: aString1 StringArray: aStrings1
	"answer the result of calling the receiver's public exec(java.lang.String, java.lang.String[]) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aStrings1;
			yourself.

	^ self callObjectMethod: 'exec' signature: '(Ljava/lang/String;[Ljava/lang/String;)Ljava/lang/Process;' withArguments: args.
!

exec_String: aString1 StringArray: aStrings1 File: aFile1
	"answer the result of calling the receiver's public exec(java.lang.String, java.lang.String[], java.io.File) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aStrings1;
			objectAt: 3 put: aFile1;
			yourself.

	^ self callObjectMethod: 'exec' signature: '(Ljava/lang/String;[Ljava/lang/String;Ljava/io/File;)Ljava/lang/Process;' withArguments: args.
!

exec_StringArray: aStrings1
	"answer the result of calling the receiver's public exec(java.lang.String[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aStrings1;
			yourself.

	^ self callObjectMethod: 'exec' signature: '([Ljava/lang/String;)Ljava/lang/Process;' withArguments: args.
!

exec_StringArray: aStrings1 StringArray: aStrings2
	"answer the result of calling the receiver's public exec(java.lang.String[], java.lang.String[]) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: aStrings1;
			objectAt: 2 put: aStrings2;
			yourself.

	^ self callObjectMethod: 'exec' signature: '([Ljava/lang/String;[Ljava/lang/String;)Ljava/lang/Process;' withArguments: args.
!

exec_StringArray: aStrings1 StringArray: aStrings2 File: aFile1
	"answer the result of calling the receiver's public exec(java.lang.String[], java.lang.String[], java.io.File) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: aStrings1;
			objectAt: 2 put: aStrings2;
			objectAt: 3 put: aFile1;
			yourself.

	^ self callObjectMethod: 'exec' signature: '([Ljava/lang/String;[Ljava/lang/String;Ljava/io/File;)Ljava/lang/Process;' withArguments: args.
!

exit_int: int1
	"invoke the receiver's public exit(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'exit' signature: '(I)V' withArguments: args.
!

freeMemory_null
	"answer the result of calling the receiver's public native freeMemory() Java method"

	^ self callLongMethod: 'freeMemory'.
!

gc_null
	"invoke the receiver's public native gc() Java method"

	self callVoidMethod: 'gc'.
!

getLocalizedInputStream_InputStream: anInputStream1
	"answer the result of calling the receiver's public getLocalizedInputStream(java.io.InputStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anInputStream1;
			yourself.

	^ self callObjectMethod: 'getLocalizedInputStream' signature: '(Ljava/io/InputStream;)Ljava/io/InputStream;' withArguments: args.
!

getLocalizedOutputStream_OutputStream: anOutputStream1
	"answer the result of calling the receiver's public getLocalizedOutputStream(java.io.OutputStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anOutputStream1;
			yourself.

	^ self callObjectMethod: 'getLocalizedOutputStream' signature: '(Ljava/io/OutputStream;)Ljava/io/OutputStream;' withArguments: args.
!

halt_int: int1
	"invoke the receiver's public halt(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'halt' signature: '(I)V' withArguments: args.
!

load_String: aString1
	"invoke the receiver's public load(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'load' signature: '(Ljava/lang/String;)V' withArguments: args.
!

loadLibrary_String: aString1
	"invoke the receiver's public loadLibrary(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'loadLibrary' signature: '(Ljava/lang/String;)V' withArguments: args.
!

maxMemory_null
	"answer the result of calling the receiver's public native maxMemory() Java method"

	^ self callLongMethod: 'maxMemory'.
!

removeShutdownHook_Thread: aThread1
	"answer the result of calling the receiver's public removeShutdownHook(java.lang.Thread) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aThread1;
			yourself.

	^ self callBooleanMethod: 'removeShutdownHook' signature: '(Ljava/lang/Thread;)Z' withArguments: args.
!

runFinalization_null
	"invoke the receiver's public runFinalization() Java method"

	self callVoidMethod: 'runFinalization'.
!

totalMemory_null
	"answer the result of calling the receiver's public native totalMemory() Java method"

	^ self callLongMethod: 'totalMemory'.
!

traceInstructions_boolean: boolean1
	"invoke the receiver's public native traceInstructions(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'traceInstructions' signature: '(Z)V' withArguments: args.
!

traceMethodCalls_boolean: boolean1
	"invoke the receiver's public native traceMethodCalls(boolean) Java method"

	| args |

	args := (JNIValueArray new: 1)
			booleanAt: 1 put: boolean1;
			yourself.

	self callVoidMethod: 'traceMethodCalls' signature: '(Z)V' withArguments: args.
! !
!JavaLangRuntime categoriesFor: #addShutdownHook_Thread:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #availableProcessors_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_String:StringArray:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_String:StringArray:File:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_StringArray:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_StringArray:StringArray:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exec_StringArray:StringArray:File:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #exit_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #freeMemory_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #gc_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #getLocalizedInputStream_InputStream:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #getLocalizedOutputStream_OutputStream:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #halt_int:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #load_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #loadLibrary_String:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #maxMemory_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #removeShutdownHook_Thread:!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #runFinalization_null!**auto generated**!Java-methods!Java-public!public! !
!JavaLangRuntime categoriesFor: #totalMemory_null!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #traceInstructions_boolean:!**auto generated**!Java-methods!Java-native!Java-public!public! !
!JavaLangRuntime categoriesFor: #traceMethodCalls_boolean:!**auto generated**!Java-methods!Java-native!Java-public!public! !

!JavaLangRuntime class methodsFor!

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
		#addShutdownHook_Thread:
		#availableProcessors_null
		#exec_String:
		#exec_String:StringArray:
		#exec_String:StringArray:File:
		#exec_StringArray:
		#exec_StringArray:StringArray:
		#exec_StringArray:StringArray:File:
		#exit_int:
		#freeMemory_null
		#gc_null
		#getLocalizedInputStream_InputStream:
		#getLocalizedOutputStream_OutputStream:
		#halt_int:
		#load_String:
		#loadLibrary_String:
		#maxMemory_null
		#removeShutdownHook_Thread:
		#runFinalization_null
		#totalMemory_null
		#traceInstructions_boolean:
		#traceMethodCalls_boolean:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Runtime'.
! !
!JavaLangRuntime class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangRuntime class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangRuntime class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangRuntime class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaLangRuntime class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaReadStream guid: (GUID fromString: '{EBA38DA8-7CD1-4205-8B91-D72CB6B63DA8}')!
JavaReadStream comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to the Java classes java.io.InputStream and java.io.Reader and their various subclasses.  Those two classes are not, in fact related in the Java class hierarchy, but there''s some commonality that we capture here.

This class adds a little of the Smalltalk ReadStream protocol, however the emulation is far from complete.  The problem being that the InputStream protocols seem to be built on the assumption that the stream is positionable, but Java input streams are not (they are not *arbitrarily* positionable).  One result of this is that although we can provide #upToEnd, we cannot provide #contents (since #contents reads from the *begining* of the stream, not from the current position).

Instances will be binary or not depending on the Java object''s class.

'!
!JavaReadStream categoriesForClass!Unclassified! !
!JavaReadStream methodsFor!

close
	"invoke the receiver's close() Java method"

	self close_null.!

close_null
	"invoke the receiver's public close() Java method"

	self callVoidMethod: 'close'.
!

isBinary
	"answer whether we read bytes rather than chars"

	self subclassResponsibility.!

isReadable
	"answer whether we can be read from"

	^ true.!

isWriteable
	"answer whether we can be written to"

	^ false.!

mark: anInteger
	"invoke the receiver's mark() Java method"

	self mark_int: anInteger.!

mark_int: int1
	"invoke the receiver's public mark(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'mark' signature: '(I)V' withArguments: args.
!

markSupported
	"answer the result of calling the receiver's markSupported() Java method"

	^ self markSupported_null.!

markSupported_null
	"answer the result of calling the receiver's public markSupported() Java method"

	^ self callBooleanMethod: 'markSupported'.
!

next
	"answer the next element of the stream"

	self subclassResponsibility.!

next: anInteger
	"answer the next anInteger elements of the stream.
	The array will either be a String or a ByteArray depending on the type of the stream"

	self subclassResponsibility.!

read
	"answer the result of calling the receiver's read() Java method"

	^ self read_null.!

read_null
	"answer the result of calling the receiver's public read() Java method"

	^ self callIntMethod: 'read'.
!

reset
	"invoke the receiver's reset() Java method"

	^ self reset_null.!

reset_null
	"invoke the receiver's public synchronized reset() Java method"

	self callVoidMethod: 'reset'.
!

skip: anInteger
	"answer the result of calling the receiver's skip() Java method"

	^ self skip_long: anInteger.!

skip_long: long1
	"answer the result of calling the receiver's public skip(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callLongMethod: 'skip' signature: '(J)J' withArguments: args.
!

upToEnd
	"answer an Array of the remaining elements of the stream.
	The array will either be a String or a ByteArray depending on the type of the stream"

	self subclassResponsibility.
! !
!JavaReadStream categoriesFor: #close!operations!public! !
!JavaReadStream categoriesFor: #close_null!Java-methods!Java-public!public! !
!JavaReadStream categoriesFor: #isBinary!public!testing! !
!JavaReadStream categoriesFor: #isReadable!public!testing! !
!JavaReadStream categoriesFor: #isWriteable!public!testing! !
!JavaReadStream categoriesFor: #mark:!operations!public! !
!JavaReadStream categoriesFor: #mark_int:!Java-methods!Java-public!public! !
!JavaReadStream categoriesFor: #markSupported!public!testing! !
!JavaReadStream categoriesFor: #markSupported_null!Java-methods!Java-public!public! !
!JavaReadStream categoriesFor: #next!public!reading! !
!JavaReadStream categoriesFor: #next:!public!reading! !
!JavaReadStream categoriesFor: #read!public!reading! !
!JavaReadStream categoriesFor: #read_null!Java-methods!Java-public!public! !
!JavaReadStream categoriesFor: #reset!operations!public! !
!JavaReadStream categoriesFor: #reset_null!Java-methods!Java-public!Java-synchronized!public! !
!JavaReadStream categoriesFor: #skip:!operations!public! !
!JavaReadStream categoriesFor: #skip_long:!Java-methods!Java-public!public! !
!JavaReadStream categoriesFor: #upToEnd!public!streaming! !

!JavaReadStream class methodsFor!

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
		#close_null
		#mark_int:
		#markSupported_null
		#read_null
		#reset_null
		#skip_long:
	).
! !
!JavaReadStream class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaReadStream class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaReadStream class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaReadStream class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !

JavaWriteStream guid: (GUID fromString: '{6DE67D91-8BFF-4F19-A32B-D4EFB359A16D}')!
JavaWriteStream comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to the Java classes java.io.OutputStream and java.io.Writer and their various subclasses.  Those two classes are not, in fact related in the Java class hierarchy, but there''s a fair amount of commonality that we capture here.

This class adds a reasonably facsimile of the Smalltalk WriteStream protocol, #nextPut: etc.  Note that whether the stream is considered to be binary will depend on the Java class that we wrap.'!
!JavaWriteStream categoriesForClass!Unclassified! !
!JavaWriteStream methodsFor!

close
	"invoke the receiver's close() Java method"

	self close_null.
!

close_null
	"invoke the receiver's public close() Java method"

	self callVoidMethod: 'close'.
!

cr
	"add the line delimeter sequence (not the <CR> character -- bloody stupid
	ANSI spec)"

	^ self nextPutAll: String lineDelimiter.!

crtab
	"add a line-delimiter and a tab"

	^ self cr; tab.!

crtab: anInteger
	"add a line-delimiter and the given number of tabs"

	^ self cr; tab: anInteger.!

display: anObject
	"tell anObject to display itself on us"

	^ anObject displayOn: self.!

flush
	"invoke the receiver's flush() Java method"

	self flush_null.
!

flush_null
	"invoke the receiver's public flush() Java method"

	self callVoidMethod: 'flush'.
!

isBinary
	"answer whether we write bytes rather than chars"

	self subclassResponsibility.!

isReadable
	"answer whether we can be read from"

	^ false.!

isWriteable
	"answer whether we can be written to"

	^ true.!

next: anInteger put: anObject
	"repeatedly add anObject. 
	Answers anObject."

	anInteger timesRepeat: [self nextPut: anObject].

	^ anObject.!

next: aSize putAll: aSequenceableCollection startingAt: anIndex
	"add aSize elements of aSequenceableCollection from anIndex.
	Answers aSequenceableCollection"

	"slow default implementation"
	anIndex to: anIndex + aSize do: [:i | self nextPut: (aSequenceableCollection at: i)].

	^ aSequenceableCollection.
!

nextPut: aCharacterOrInteger
	"write aCharacterOrInteger answering aCharacterOrInteger"

	self write_int: aCharacterOrInteger asInteger.

	^ aCharacterOrInteger.

!

nextPutAll: aSequenceableCollection
	"add aSequenceableCollection.
	Answers aSequenceableCollection"

	^ self
		next: aSequenceableCollection size
		putAll: aSequenceableCollection
		startingAt: 1.
!

print: anObject
	"tell anObject to print itself on us"

	^ anObject printOn: self.!

space
	"add a space character"

	^ self nextPut: Character space.!

tab
	"add a tab character"

	^ self nextPut: Character tab.!

tab: anInteger
	"add the given number of tabs"

	anInteger timesRepeat: [self tab].!

write: aCharacterOrInteger
	"invoke the receiver's write() Java method"

	self write_int: aCharacterOrInteger asInteger.!

write_int: int1
	"invoke the receiver's public write(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	self callVoidMethod: 'write' signature: '(I)V' withArguments: args.
! !
!JavaWriteStream categoriesFor: #close!operations!public! !
!JavaWriteStream categoriesFor: #close_null!Java-public!public! !
!JavaWriteStream categoriesFor: #cr!public!writing! !
!JavaWriteStream categoriesFor: #crtab!public!writing! !
!JavaWriteStream categoriesFor: #crtab:!public!writing! !
!JavaWriteStream categoriesFor: #display:!public!writing! !
!JavaWriteStream categoriesFor: #flush!operations!public! !
!JavaWriteStream categoriesFor: #flush_null!Java-methods!Java-public!public! !
!JavaWriteStream categoriesFor: #isBinary!public!testing! !
!JavaWriteStream categoriesFor: #isReadable!public!testing! !
!JavaWriteStream categoriesFor: #isWriteable!public!testing! !
!JavaWriteStream categoriesFor: #next:put:!public!writing! !
!JavaWriteStream categoriesFor: #next:putAll:startingAt:!public!writing! !
!JavaWriteStream categoriesFor: #nextPut:!public!writing! !
!JavaWriteStream categoriesFor: #nextPutAll:!public!writing! !
!JavaWriteStream categoriesFor: #print:!public!writing! !
!JavaWriteStream categoriesFor: #space!public!writing! !
!JavaWriteStream categoriesFor: #tab!public!writing! !
!JavaWriteStream categoriesFor: #tab:!public!writing! !
!JavaWriteStream categoriesFor: #write:!public!writing! !
!JavaWriteStream categoriesFor: #write_int:!Java-public!public! !

JavaWriteStream methodProtocol: #puttableStream attributes: #(#ansi #readOnly) selectors: #(#cr #flush #nextPut: #nextPutAll: #space #tab)!

!JavaWriteStream class methodsFor!

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
		#close_null
		#flush_null
		#write_int:
	).
! !
!JavaWriteStream class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaWriteStream class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaWriteStream class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaWriteStream class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !

JavaLangError guid: (GUID fromString: '{085D1DE1-836A-4068-9499-14761484CF74}')!
JavaLangError comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.Error'!
!JavaLangError categoriesForClass!Unclassified! !
!JavaLangError class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.Error'.
! !
!JavaLangError class categoriesFor: #javaClassName!accessing!constants!public! !

JavaLangException guid: (GUID fromString: '{1B0FA51B-4F21-4722-A4F7-3CEF97EA46BB}')!
JavaLangException comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.lang.Exception'!
!JavaLangException categoriesForClass!Unclassified! !
!JavaLangException class methodsFor!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.lang.Exception'.
! !
!JavaLangException class categoriesFor: #javaClassName!accessing!constants!public! !

JavaIoInputStream guid: (GUID fromString: '{ABC2712A-4A73-4E94-ACBC-EB9B219A0424}')!
JavaIoInputStream comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.io.InputStream'!
!JavaIoInputStream categoriesForClass!Unclassified! !
!JavaIoInputStream methodsFor!

available
	"answer the result of calling the receiver's available() Java method"

	^ self available_null.
!

available_null
	"answer the result of calling the receiver's public available() Java method"

	^ self callIntMethod: 'available'.
!

isBinary
	"answer whether we read bytes rather than chars"

	^ true.!

next
	"answer the next byte from the stream"

	^ self read_null.!

next: anInteger
	"answer a ByteArray of the next anInteger elements of the stream"

	| buffer offset |

	buffer := JavaByteArray new: anInteger jvm: self jvm.
	offset := 0.
	[offset < anInteger] whileTrue:
		[| read |
		read := self read_byteArray: buffer int: offset int: anInteger-offset.
		read <= 0 ifTrue: [^ Stream endOfStreamSignal signalWith: self].
		offset := offset + read].

	^ buffer asByteArray.!

read: aJavaByteArray
	"answer the result of calling the receiver's read() Java method"

	^ self read_byteArray: aJavaByteArray.!

read: aJavaByteArray from: aStartIndex to: aStopIndex
	"answer the result of calling the receiver's read() Java method.
	Note that the indexes are Smalltalk-style, so the start and stop positions
	are 1-based, and aStopIndex *is* included in the range unlike the underlying
	Java method"

	^ self
		read_byteArray: aJavaByteArray
		int: aStartIndex - 1
		int: aStopIndex - aStartIndex + 1.!

read_byteArray: bytes1
	"answer the result of calling the receiver's public read(byte[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: bytes1;
			yourself.

	^ self callIntMethod: 'read' signature: '([B)I' withArguments: args.
!

read_byteArray: bytes1 int: int1 int: int2
	"answer the result of calling the receiver's public read(byte[], int, int) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: bytes1;
			intAt: 2 put: int1;
			intAt: 3 put: int2;
			yourself.

	^ self callIntMethod: 'read' signature: '([BII)I' withArguments: args.
!

upToEnd
	"answer a ByteArray of the remaining elements of the stream"

	| gulp buffer ostream read |

	gulp := 16*1024.
	buffer := JavaByteArray new: gulp jvm: self jvm.
	ostream := ByteArray writeStream: gulp.
	[read := self read_byteArray: buffer int: 0 int: gulp.
	read <= 0 ifTrue: [^ ostream contents].
	ostream next: read putAll: buffer asByteArray startingAt: 1]
		repeat.! !
!JavaIoInputStream categoriesFor: #available!accessing!public! !
!JavaIoInputStream categoriesFor: #available_null!Java-methods!Java-public!public! !
!JavaIoInputStream categoriesFor: #isBinary!public!testing! !
!JavaIoInputStream categoriesFor: #next!public!reading! !
!JavaIoInputStream categoriesFor: #next:!public!reading! !
!JavaIoInputStream categoriesFor: #read:!public!reading! !
!JavaIoInputStream categoriesFor: #read:from:to:!public!reading! !
!JavaIoInputStream categoriesFor: #read_byteArray:!Java-methods!Java-public!public! !
!JavaIoInputStream categoriesFor: #read_byteArray:int:int:!Java-methods!Java-public!public! !
!JavaIoInputStream categoriesFor: #upToEnd!public!streaming! !

!JavaIoInputStream class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#available_null
		#read_byteArray:
		#read_byteArray:int:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.io.InputStream'.! !
!JavaIoInputStream class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaIoInputStream class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaIoInputStream class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaIoInputStream class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaIoInputStream class categoriesFor: #javaClassName!constants!public! !

JavaIoReader guid: (GUID fromString: '{C17B3D04-82A4-4BE8-9698-41F8D0D5DC2C}')!
JavaIoReader comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.io.Reader'!
!JavaIoReader categoriesForClass!Unclassified! !
!JavaIoReader methodsFor!

isBinary
	"answer whether we read bytes rather than chars"

	^ false.!

next
	"answer the next Character of the stream"

	^ Character codePoint: (self read_null).!

next: anInteger
	"answer a String of the next anInteger elements of the stream"

	| buffer offset |

	buffer := JavaCharArray new: anInteger jvm: self jvm.
	offset := 0.
	[offset <= anInteger] whileTrue:
		[| read |
		read := self read_charArray: buffer int: offset int: anInteger-offset.
		read <= 0 ifTrue: [^ Stream endOfStreamSignal signalWith: self].
		offset := offset + read].

	^ buffer asString.!

read: aJavaCharArray
	"answer the result of calling the receiver's read() Java method"

	^ self read_charArray: aJavaCharArray.!

read: aJavaCharArray  from: aStartIndex to: aStopIndex
	"answer the result of calling the receiver's read() Java method.
	Note that the indexes are Smalltalk-style, and so the start and stop positions
	are 1-based, and aStopIndex *is* included in the range unlike the underlying
	Java method"

	^ self
		read_charArray: aJavaCharArray
		int: aStartIndex - 1
		int: aStopIndex - aStartIndex + 1.!

read_charArray: chars1
	"answer the result of calling the receiver's public read(char[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: chars1;
			yourself.

	^ self callIntMethod: 'read' signature: '([C)I' withArguments: args.
!

read_charArray: chars1 int: int1 int: int2
	"answer the result of calling the receiver's public abstract read(char[], int, int) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: chars1;
			intAt: 2 put: int1;
			intAt: 3 put: int2;
			yourself.

	^ self callIntMethod: 'read' signature: '([CII)I' withArguments: args.
!

ready
	"answer the result of calling the receiver's ready() Java method"

	^ self ready_null.
!

ready_null
	"answer the result of calling the receiver's public ready() Java method"

	^ self callBooleanMethod: 'ready'.
!

upToEnd
	"answer a String of the remaining elements of the stream"

	| gulp buffer ostream read |

	gulp := 16*1024.
	buffer := JavaCharArray new: gulp jvm: self jvm.
	ostream := String writeStream: gulp.
	[read := self read_charArray: buffer int: 0 int: gulp.
	read <= 0 ifTrue: [^ ostream contents].
	ostream next: read putAll: buffer asByteArray startingAt: 1]
		repeat.! !
!JavaIoReader categoriesFor: #isBinary!public!testing! !
!JavaIoReader categoriesFor: #next!public!reading! !
!JavaIoReader categoriesFor: #next:!public!reading! !
!JavaIoReader categoriesFor: #read:!public!reading! !
!JavaIoReader categoriesFor: #read:from:to:!public!reading! !
!JavaIoReader categoriesFor: #read_charArray:!Java-methods!Java-public!public! !
!JavaIoReader categoriesFor: #read_charArray:int:int:!Java-abstract!Java-methods!Java-public!public! !
!JavaIoReader categoriesFor: #ready!public!testing! !
!JavaIoReader categoriesFor: #ready_null!Java-methods!Java-public!public! !
!JavaIoReader categoriesFor: #upToEnd!public!streaming! !

!JavaIoReader class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#read_charArray:
		#read_charArray:int:int:
		#ready_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.io.Reader'.! !
!JavaIoReader class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaIoReader class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaIoReader class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaIoReader class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaIoReader class categoriesFor: #javaClassName!constants!public! !

JavaIoOutputStream guid: (GUID fromString: '{DD58205E-8DCC-4E10-BAC0-80070CED6255}')!
JavaIoOutputStream comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.io.OutputStream'!
!JavaIoOutputStream categoriesForClass!Unclassified! !
!JavaIoOutputStream methodsFor!

isBinary
	"answer whether we write bytes rather than chars"

	^ true.!

next: aSize putAll: aSequenceableCollection startingAt: anIndex
	"add aSize elements of aSequenceableCollection from anIndex.
	Answers aSequenceableCollection"

	self
		writeBytes: aSequenceableCollection
		from: anIndex
		to: anIndex + aSize.

	^ aSequenceableCollection.
!

nextPutAll: aSequenceableCollection
	"add aSequenceableCollection.
	Answers aSequenceableCollection"

	self writeBytes: aSequenceableCollection.

	^ aSequenceableCollection.!

write_byteArray: bytes1
	"invoke the receiver's public write(byte[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: bytes1;
			yourself.

	self callVoidMethod: 'write' signature: '([B)V' withArguments: args.
!

write_byteArray: bytes1 int: int1 int: int2
	"invoke the receiver's public write(byte[], int, int) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: bytes1;
			intAt: 2 put: int1;
			intAt: 3 put: int2;
			yourself.

	self callVoidMethod: 'write' signature: '([BII)V' withArguments: args.
!

writeBytes: aByteArray
	"invoke the receiver's write() Java method, the argument can be a Java byte[], a
	Smalltalk ByteArray, or a String"

	self write_byteArray: (aByteArray asJavaByteArray: self jvm).!

writeBytes: aByteArray from: aStartIndex to: aStopIndex
	"invoke the receiver's write() Java method, the argument can be a Java byte[], a Smalltalk
	ByteArray, or a String.  Note that the start and stop positions are 1-based, and that aStopIndex
	*is* included in the range unlike the real Java method"

	self
		write_byteArray: (aByteArray asJavaByteArray: self jvm)
		int: aStartIndex - 1
		int: aStopIndex - aStartIndex + 1.! !
!JavaIoOutputStream categoriesFor: #isBinary!public!testing! !
!JavaIoOutputStream categoriesFor: #next:putAll:startingAt:!public!writing! !
!JavaIoOutputStream categoriesFor: #nextPutAll:!public!writing! !
!JavaIoOutputStream categoriesFor: #write_byteArray:!Java-methods!Java-public!public! !
!JavaIoOutputStream categoriesFor: #write_byteArray:int:int:!Java-methods!Java-public!public! !
!JavaIoOutputStream categoriesFor: #writeBytes:!public!writing! !
!JavaIoOutputStream categoriesFor: #writeBytes:from:to:!public!writing! !

!JavaIoOutputStream class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#write_byteArray:
		#write_byteArray:int:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.io.OutputStream'.! !
!JavaIoOutputStream class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaIoOutputStream class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaIoOutputStream class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaIoOutputStream class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaIoOutputStream class categoriesFor: #javaClassName!constants!public! !

JavaIoWriter guid: (GUID fromString: '{F28ADBFD-7D0E-484E-A816-A9001162C6B0}')!
JavaIoWriter comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.io.Writer'!
!JavaIoWriter categoriesForClass!Unclassified! !
!JavaIoWriter methodsFor!

isBinary
	"answer whether we write bytes rather than chars"

	^ false.!

next: aSize putAll: aSequenceableCollection startingAt: anIndex
	"add aSize elements of aSequenceableCollection from anIndex.
	Answers aSequenceableCollection"

	"ugly, should double-dispatch this"
	((aSequenceableCollection isKindOf: JavaCharArray) or: [aSequenceableCollection isKindOf: Array])
		ifTrue: [self writeChars: aSequenceableCollection from: anIndex to: anIndex + aSize]
		ifFalse: [self writeString: aSequenceableCollection from: anIndex to: anIndex + aSize].

	^ aSequenceableCollection.
!

nextPutAll: aSequenceableCollection
	"add aSequenceableCollection.
	Answers aSequenceableCollection"

	"ugly, should double-dispatch this"
	((aSequenceableCollection isKindOf: JavaCharArray) or: [aSequenceableCollection isKindOf: Array])
		ifTrue: [self writeChars: aSequenceableCollection]
		ifFalse: [self writeString: aSequenceableCollection].

	^ aSequenceableCollection.!

write_charArray: chars1
	"invoke the receiver's public write(char[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: chars1;
			yourself.

	self callVoidMethod: 'write' signature: '([C)V' withArguments: args.
!

write_charArray: chars1 int: int1 int: int2
	"invoke the receiver's public abstract write(char[], int, int) Java method"

	| args |

	args := (JNIValueArray new: 3)
			objectAt: 1 put: chars1;
			intAt: 2 put: int1;
			intAt: 3 put: int2;
			yourself.

	self callVoidMethod: 'write' signature: '([CII)V' withArguments: args.
!

write_String: aString1
	"invoke the receiver's public write(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'write' signature: '(Ljava/lang/String;)V' withArguments: args.
!

write_String: aString1 int: int1 int: int2
	"invoke the receiver's public write(java.lang.String, int, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 3)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			intAt: 3 put: int2;
			yourself.

	self callVoidMethod: 'write' signature: '(Ljava/lang/String;II)V' withArguments: args.
!

writeChars: aCharacterArray
	"invoke the receiver's write() Java method; the argument can be a Java char[], a
	Smalltalk ByteArray, or a String"

	self write_charArray: (aCharacterArray asJavaCharArray: self jvm).
!

writeChars: aCharArray from: aStartIndex to: aStopIndex
	"invoke the receiver's write() Java method, the argument can be a Java char[], a Smalltalk
	ByteArray, or a String.  Note that the start and stop positions are 1-based, and that aStopIndex
	*is* included in the range unlike the real Java method"

	self
		write_charArray: (aCharArray asJavaCharArray: self jvm)
		int: aStartIndex - 1
		int: aStopIndex - aStartIndex + 1.!

writeString: aJavaString
	"invoke the receiver's write() Java method, the argument can be a Java String, a
	Smalltalk ByteArray, or a String"

	self write_String: aJavaString.!

writeString: aJavaString from: aStartIndex to: aStopIndex
	"invoke the receiver's write() Java method, the argument can be a Java String, a Smalltalk
	ByteArray, or a String.  Note that the start and stop positions are 1-based, and that aStopIndex
	*is* included in the range unlike the real Java method"

	self
		write_String: aJavaString
		int: aStartIndex - 1
		int: aStopIndex - aStartIndex + 1.! !
!JavaIoWriter categoriesFor: #isBinary!public!testing! !
!JavaIoWriter categoriesFor: #next:putAll:startingAt:!public!writing! !
!JavaIoWriter categoriesFor: #nextPutAll:!public!writing! !
!JavaIoWriter categoriesFor: #write_charArray:!Java-methods!Java-public!public! !
!JavaIoWriter categoriesFor: #write_charArray:int:int:!Java-abstract!Java-methods!Java-public!public! !
!JavaIoWriter categoriesFor: #write_String:!Java-methods!Java-public!public! !
!JavaIoWriter categoriesFor: #write_String:int:int:!Java-methods!Java-public!public! !
!JavaIoWriter categoriesFor: #writeChars:!public!writing! !
!JavaIoWriter categoriesFor: #writeChars:from:to:!public!writing! !
!JavaIoWriter categoriesFor: #writeString:!public!writing! !
!JavaIoWriter categoriesFor: #writeString:from:to:!public!writing! !

!JavaIoWriter class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#write_charArray:
		#write_charArray:int:int:
		#write_String:
		#write_String:int:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.io.Writer'.! !
!JavaIoWriter class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaIoWriter class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaIoWriter class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaIoWriter class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaIoWriter class categoriesFor: #javaClassName!constants!public! !

JavaContainerInterface guid: (GUID fromString: '{A7FD5E06-8984-4B25-B837-D8A998DCAD35}')!
JavaContainerInterface comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Subclasses are wrapers for various Java2 Collections interfaces.  Objects that implement those interfaces can be asked for the appropriate interface wrapper (using #asASet, etc), which will answer an instance of one of our subclasses.  The wrapper implements the basic methods of the corresponding Smalltalk class (#collect:, etc), and can answer a further adaptor that provides a more complete implementation if sent #asCollection.'!
!JavaContainerInterface categoriesForClass!Unclassified! !
!JavaContainerInterface methodsFor!

add: anObject
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

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
	In fact, for these classes we answer a CollectionAdaptor wrapped around the object.  It acts like a
	proper Collection"

	adaptorCache isNil ifTrue: [adaptorCache := CollectionAdaptor for: self].

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

clear_null
	"invoke the receiver's public abstract clear() Java method"

	self callVoidMethod: 'clear'.
!

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
	"one of the root methods for <Collection>"

	self subclassResponsibility.
!

includes: anObject
	"shorthand for:"

	^ self asCollection includes: anObject.!

inject: anObject into: a2Block
	"shorthand for:"

	^ self asCollection inject: anObject into: a2Block.

!

isEmpty
	"answer whether the receiver contains no elements."

	"we don't use:
		^ self size_null = 0.
	since size may not be a constant-time operation"

	^ self isEmpty_null.
!

isEmpty_null
	"answer the result of calling the receiver's public abstract isEmpty() Java method"

	^ self callBooleanMethod: 'isEmpty'.
!

notEmpty
	"answer whether the receiver contains any elements"

	^ self isEmpty not.!

reject: a1Block
	"shorthand for:"

	^ self asCollection reject: a1Block.!

remove: anObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	self subclassResponsibility.!

remove_Object: anObject1
	"answer the result of calling the receiver's public abstract remove(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'remove' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

removeAll
	"remove all the elements of the underlying container"

	self clear_null.
!

replaceFrom: aStartIndex to: aStopIndex with: replacementElements
	"shorthand for:"

	^ self asCollection replaceFrom: aStartIndex to: aStopIndex with: replacementElements.
!

select: a1Block
	"shorthand for:"

	^ self asCollection select: a1Block.!

size
	"answer the number of elements in the underlying container"

	^ self size_null.!

size_null
	"answer the result of calling the receiver's public abstract size() Java method"

	^ self callIntMethod: 'size'.
! !
!JavaContainerInterface categoriesFor: #add:!adding!public! !
!JavaContainerInterface categoriesFor: #allSatisfy:!enumerating!public! !
!JavaContainerInterface categoriesFor: #anySatisfy:!enumerating!public! !
!JavaContainerInterface categoriesFor: #asArray!converting!public! !
!JavaContainerInterface categoriesFor: #asBag!converting!public! !
!JavaContainerInterface categoriesFor: #asCollection!converting!public! !
!JavaContainerInterface categoriesFor: #asIdentitySet!converting!public! !
!JavaContainerInterface categoriesFor: #asOrderedCollection!converting!public! !
!JavaContainerInterface categoriesFor: #asSet!converting!public! !
!JavaContainerInterface categoriesFor: #asSortedCollection!converting!public! !
!JavaContainerInterface categoriesFor: #asSortedCollection:!converting!public! !
!JavaContainerInterface categoriesFor: #clear_null!Java-abstract!Java-methods!Java-public!public! !
!JavaContainerInterface categoriesFor: #collect:!enumerating!public! !
!JavaContainerInterface categoriesFor: #detect:!public!searching! !
!JavaContainerInterface categoriesFor: #detect:ifNone:!public!searching! !
!JavaContainerInterface categoriesFor: #do:!enumerating!public! !
!JavaContainerInterface categoriesFor: #includes:!public!searching! !
!JavaContainerInterface categoriesFor: #inject:into:!enumerating!public! !
!JavaContainerInterface categoriesFor: #isEmpty!public!testing! !
!JavaContainerInterface categoriesFor: #isEmpty_null!Java-abstract!Java-methods!Java-public!public! !
!JavaContainerInterface categoriesFor: #notEmpty!public!testing! !
!JavaContainerInterface categoriesFor: #reject:!enumerating!public! !
!JavaContainerInterface categoriesFor: #remove:ifAbsent:!public!removing! !
!JavaContainerInterface categoriesFor: #remove_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaContainerInterface categoriesFor: #removeAll!public!removing! !
!JavaContainerInterface categoriesFor: #replaceFrom:to:with:!public!replacing! !
!JavaContainerInterface categoriesFor: #select:!enumerating!public! !
!JavaContainerInterface categoriesFor: #size!accessing!public! !
!JavaContainerInterface categoriesFor: #size_null!Java-abstract!Java-methods!Java-public!public! !

!JavaContainerInterface class methodsFor!

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
		#clear_null
		#isEmpty_null
		#remove_Object:
		#size_null
	).
! !
!JavaContainerInterface class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaContainerInterface class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaContainerInterface class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaContainerInterface class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !

JavaExternalIterator guid: (GUID fromString: '{A0953370-CECC-4E2B-9960-2EEDD07AE9A6}')!
JavaExternalIterator comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Abstract class intended to capture the commonality (or hide the differences, if you prefer to see it that way) between the old-style and new-style Java external iterators.  Of course, other iterator classes can be added too.

This class and subclasses add a minimal set of <ReadableStream> protocol methods to Java''s external iterators.  A more complete implementation can be obtained by asking for a ReadStreamAdaptor with #asReadStream.'!
!JavaExternalIterator categoriesForClass!Unclassified! !
!JavaExternalIterator methodsFor!

asReadStream
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Stream hierarchy"

	adaptor isNil ifTrue: [adaptor := ReadStreamAdaptor for: self].

	^ adaptor.!

atEnd
	"part of the normal <ReadStream> protocol"

	self subclassResponsibility.!

next
	"part of the normal <ReadStream> protocol"

	self subclassResponsibility.! !
!JavaExternalIterator categoriesFor: #asReadStream!converting!public! !
!JavaExternalIterator categoriesFor: #atEnd!public!streaming!testing! !
!JavaExternalIterator categoriesFor: #next!public!streaming! !

JavaUtilComparator guid: (GUID fromString: '{0EAAE4E9-6707-4A76-8B06-16E8B8BCF722}')!
JavaUtilComparator comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Comparator'!
!JavaUtilComparator categoriesForClass!Unclassified! !
!JavaUtilComparator methodsFor!

compare: aJavaObject with: anotherJavaObject
	"answer the result of calling the receiver's compare() Java method"

	^ self compare_Object: aJavaObject Object: anotherJavaObject.!

compare_Object: anObject1 Object: anObject2
	"answer the result of calling the receiver's public abstract compare(java.lang.Object, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObject2;
			yourself.

	^ self callIntMethod: 'compare' signature: '(Ljava/lang/Object;Ljava/lang/Object;)I' withArguments: args.
! !
!JavaUtilComparator categoriesFor: #compare:with:!comparing!public! !
!JavaUtilComparator categoriesFor: #compare_Object:Object:!**auto generated**!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilComparator class methodsFor!

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
		#compare_Object:Object:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.util.Comparator'.
! !
!JavaUtilComparator class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilComparator class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilComparator class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilComparator class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilComparator class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaUtilCollection guid: (GUID fromString: '{1FF27510-850F-48D1-A673-535DFDDBB2D9}')!
JavaUtilCollection comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Collection'!
!JavaUtilCollection categoriesForClass!Unclassified! !
!JavaUtilCollection methodsFor!

add: aJavaObject
	"one of the root methods for <Collection>"

	self add_Object: aJavaObject.

	"discard useful return value from the Java method, and instead
	answer this useless datum"
	^ aJavaObject.
!

add_Object: anObject1
	"answer the result of calling the receiver's public abstract add(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'add' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

addAll_Collection: aCollection1
	"answer the result of calling the receiver's public abstract addAll(java.util.Collection) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aCollection1;
			yourself.

	^ self callBooleanMethod: 'addAll' signature: '(Ljava/util/Collection;)Z' withArguments: args.
!

contains_Object: anObject1
	"answer the result of calling the receiver's public abstract contains(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'contains' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

containsAll_Collection: aCollection1
	"answer the result of calling the receiver's public abstract containsAll(java.util.Collection) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aCollection1;
			yourself.

	^ self callBooleanMethod: 'containsAll' signature: '(Ljava/util/Collection;)Z' withArguments: args.
!

do: a1Block
	"one of the root methods for <Collection>"

	^ self iterator do: a1Block.!

includes: aJavaObject
	"answer whether the container contains aJavaObject"

	^ self contains_Object: aJavaObject.!

iterator
	"answer the result of calling the receiver's iterator() Java method.
	Note that the answer is already wrapped in a JavaUtilIterator"

	^ self iterator_null asAnIterator.
!

iterator_null
	"answer the result of calling the receiver's public abstract iterator() Java method"

	^ self callObjectMethod: 'iterator' signature: '()Ljava/util/Iterator;'.
!

remove: aJavaObject ifAbsent: a0Block
	"one of the root methods for <Collection>"

	^ (self remove_Object: aJavaObject)
		ifTrue: [aJavaObject]
		ifFalse: [a0Block value].!

removeAll_Collection: aCollection1
	"answer the result of calling the receiver's public abstract removeAll(java.util.Collection) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aCollection1;
			yourself.

	^ self callBooleanMethod: 'removeAll' signature: '(Ljava/util/Collection;)Z' withArguments: args.
!

retainAll_Collection: aCollection1
	"answer the result of calling the receiver's public abstract retainAll(java.util.Collection) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aCollection1;
			yourself.

	^ self callBooleanMethod: 'retainAll' signature: '(Ljava/util/Collection;)Z' withArguments: args.
!

toArray_null
	"answer the result of calling the receiver's public abstract toArray() Java method"

	^ self callObjectMethod: 'toArray' signature: '()[Ljava/lang/Object;'.
!

toArray_ObjectArray: anObjects1
	"answer the result of calling the receiver's public abstract toArray(java.lang.Object[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObjects1;
			yourself.

	^ self callObjectMethod: 'toArray' signature: '([Ljava/lang/Object;)[Ljava/lang/Object;' withArguments: args.
! !
!JavaUtilCollection categoriesFor: #add:!adding!public! !
!JavaUtilCollection categoriesFor: #add_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #addAll_Collection:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #contains_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #containsAll_Collection:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #do:!enumerating!public! !
!JavaUtilCollection categoriesFor: #includes:!public!searching! !
!JavaUtilCollection categoriesFor: #iterator!accessing!public! !
!JavaUtilCollection categoriesFor: #iterator_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #remove:ifAbsent:!public!removing! !
!JavaUtilCollection categoriesFor: #removeAll_Collection:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #retainAll_Collection:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #toArray_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilCollection categoriesFor: #toArray_ObjectArray:!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilCollection class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#add_Object:
		#addAll_Collection:
		#contains_Object:
		#containsAll_Collection:
		#iterator_null
		#removeAll_Collection:
		#retainAll_Collection:
		#toArray_null
		#toArray_ObjectArray:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.Collection'.
! !
!JavaUtilCollection class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilCollection class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilCollection class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilCollection class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilCollection class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilMap guid: (GUID fromString: '{C75CBED6-0B77-41FE-9BE9-5E0617ED3A1B}')!
JavaUtilMap comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Map'!
!JavaUtilMap categoriesForClass!Unclassified! !
!JavaUtilMap methodsFor!

asCollection
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Collection hierarchy.
	In fact, for these classes we answer a DictionaryAdaptor wrapped around the object.  It acts pretty
	much like a proper Dictionary (the adaptor is limited by the poor internal design of the classic
	Smalltalk Collections hierarchy)"

	adaptorCache isNil ifTrue: [adaptorCache := DictionaryAdaptor for: self].

	^ adaptorCache.!

at: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	| answer |

	"first try the lookup, if that returns null then the result *may* mean that the key wasn't present, or it
	may mean that the value was null.  We only check to see if the key was present in the (hopefully
	rarer) case where the lookup has already failed"

	answer := self get_Object: aKey.

	answer isNil ifFalse: [^ answer].

	^ (self containsKey_Object: aKey)
		ifTrue: [answer]
		ifFalse: [a0Block value].!

at: aKey put: anObject
	"one of the root methods for <Dictionary>"

	^ self put_Object: aKey Object: anObject.!

containsKey_Object: anObject1
	"answer the result of calling the receiver's public abstract containsKey(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'containsKey' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

containsValue_Object: anObject1
	"answer the result of calling the receiver's public abstract containsValue(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callBooleanMethod: 'containsValue' signature: '(Ljava/lang/Object;)Z' withArguments: args.
!

do: a1Block
	"shorthand for:"

	^ self asCollection do: a1Block.!

entrySet
	"answer the result of calling the receiver's entrySet() Java method.
	Note that the answer is already wrapped in a JavaUtilSet"

	^ self entrySet_null asASet.!

entrySet_null
	"answer the result of calling the receiver's public abstract entrySet() Java method"

	^ self callObjectMethod: 'entrySet' signature: '()Ljava/util/Set;'.
!

get_Object: anObject1
	"answer the result of calling the receiver's public abstract get(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'get' signature: '(Ljava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

includesKey: aKey
	"one of the root methods for <Dictionary>"

	^ self containsKey_Object: aKey.
!

keysAndValuesDo: a2Block
	"one of the root methods for <Dictionary>"

	^ self keySet do: [:each | a2Block value: each value: (self at: each ifAbsent: [])].
!

keySet
	"answer the result of calling the receiver's keySet() Java method.
	Note that the answer is already wrapped in a JavaUtilSet"

	^ self keySet_null asASet.!

keySet_null
	"answer the result of calling the receiver's public abstract keySet() Java method"

	^ self callObjectMethod: 'keySet' signature: '()Ljava/util/Set;'.
!

put_Object: anObject1 Object: anObject2
	"answer the result of calling the receiver's public abstract put(java.lang.Object, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObject2;
			yourself.

	^ self callObjectMethod: 'put' signature: '(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

putAll_Map: aMap1
	"invoke the receiver's public abstract putAll(java.util.Map) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aMap1;
			yourself.

	self callVoidMethod: 'putAll' signature: '(Ljava/util/Map;)V' withArguments: args.
!

removeKey: aKey ifAbsent: a0Block
	"one of the root methods for <Dictionary>"

	"have to do the pre-check since there's no way of telling whether a return value of null from the remove
	means that it wasn't there or that it was there but had a null value"
	(self containsKey_Object: aKey)
		ifFalse: [^ a0Block value].

	^ self remove_Object: aKey.
!

values
	"answer the result of calling the receiver's values() Java method.
	Note that the result is already wrapped in a JavaUtilCollection"

	^ self values_null asA: #'java.util.Collection'.!

values_null
	"answer the result of calling the receiver's public abstract values() Java method"

	^ self callObjectMethod: 'values' signature: '()Ljava/util/Collection;'.
! !
!JavaUtilMap categoriesFor: #asCollection!converting!public! !
!JavaUtilMap categoriesFor: #at:ifAbsent:!accessing!public! !
!JavaUtilMap categoriesFor: #at:put:!accessing!public! !
!JavaUtilMap categoriesFor: #containsKey_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #containsValue_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #do:!enumerating!public! !
!JavaUtilMap categoriesFor: #entrySet!accessing!public! !
!JavaUtilMap categoriesFor: #entrySet_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #get_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #includesKey:!public!searching! !
!JavaUtilMap categoriesFor: #keysAndValuesDo:!enumerating!public! !
!JavaUtilMap categoriesFor: #keySet!accessing!public! !
!JavaUtilMap categoriesFor: #keySet_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #put_Object:Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #putAll_Map:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilMap categoriesFor: #removeKey:ifAbsent:!public!removing! !
!JavaUtilMap categoriesFor: #values!accessing!public! !
!JavaUtilMap categoriesFor: #values_null!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilMap class methodsFor!

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

	^ super generatedWrapperSelectors , #(
		#clear_null
		#containsKey_Object:
		#containsValue_Object:
		#entrySet_null
		#equals_Object:
		#get_Object:
		#hashCode_null
		#isEmpty_null
		#keySet_null
		#put_Object:Object:
		#putAll_Map:
		#remove_Object:
		#size_null
		#values_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.Map'.
! !
!JavaUtilMap class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilMap class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilMap class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilMap class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilMap class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilList guid: (GUID fromString: '{EBFC8F4C-4A26-4892-B616-7E6BE4B38817}')!
JavaUtilList comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.List'!
!JavaUtilList categoriesForClass!Unclassified! !
!JavaUtilList methodsFor!

add: anObject afterIndex: anIndex
	"one of the root methods for <SequenceableCollection>"

	self
		add_int: anIndex		"0-based indexing in Java means we don't need to add 1"
		Object: anObject.

	"discard the usefull result of the above, and instead do it the silly Smalltalk way"
	^ anObject.!

add_int: int1 Object: anObject1
	"invoke the receiver's public abstract add(int, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			objectAt: 2 put: anObject1;
			yourself.

	self callVoidMethod: 'add' signature: '(ILjava/lang/Object;)V' withArguments: args.
!

addAll_int: int1 Collection: aCollection1
	"answer the result of calling the receiver's public abstract addAll(int, java.util.Collection) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			objectAt: 2 put: aCollection1;
			yourself.

	^ self callBooleanMethod: 'addAll' signature: '(ILjava/util/Collection;)Z' withArguments: args.
!

addLast: anObject
	"one of the root methods for <SequenceableCollection>"

	self add_Object: anObject.

	"discard the usefull result of the above, and instead do it the silly Smalltalk way"
	^ anObject.
!

asCollection
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Collection hierarchy.
	In fact, for these classes we answer a OrderedCollectionAdaptor wrapped around the
	object.  It acts in all (maybe!!) ways like a proper SequencedGrowableCollection"

	adaptorCache isNil ifTrue: [adaptorCache := OrderedCollectionAdaptor for: self].

	^ adaptorCache.!

at: anIndex
	"one of the root methods for <SequenceableCollection>"

	^ self get_int: anIndex - 1.		"0-based indexing in Java"!

at: anIndex put: aJavaObject
	"one of the root methods for <SequenceableCollection>"

	self
		set_int: anIndex - 1		"0-based indexing in Java"
		Object: aJavaObject.

	"discard useful boolean return value, and instead return this useless value!!
		  ...sigh"
	^ aJavaObject.!

do: a1Block
	"shorthand for:"

	^ self asCollection do: a1Block.!

get_int: int1
	"answer the result of calling the receiver's public abstract get(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'get' signature: '(I)Ljava/lang/Object;' withArguments: args.
!

indexOf_Object: anObject1
	"answer the result of calling the receiver's public abstract indexOf(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callIntMethod: 'indexOf' signature: '(Ljava/lang/Object;)I' withArguments: args.
!

lastIndexOf_Object: anObject1
	"answer the result of calling the receiver's public abstract lastIndexOf(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callIntMethod: 'lastIndexOf' signature: '(Ljava/lang/Object;)I' withArguments: args.
!

listIterator
	"answer the result of calling the receiver's listIterator() Java method.
	The answer is already wrapped in a JavaUtilListIterator"

	^ self listIterator_null asAListIterator.
!

listIterator: anInteger
	"answer the result of calling the receiver's listIterator() Java method.
	Note that the integer index is 1-based, as in Smalltalk, not 0-based as
	in the underlying Java method.
	The answer is already wrapped in a JavaUtilListIterator"

	^ (self listIterator_int: anInteger - 1) asAListIterator.!

listIterator_int: int1
	"answer the result of calling the receiver's public abstract listIterator(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'listIterator' signature: '(I)Ljava/util/ListIterator;' withArguments: args.
!

listIterator_null
	"answer the result of calling the receiver's public abstract listIterator() Java method"

	^ self callObjectMethod: 'listIterator' signature: '()Ljava/util/ListIterator;'.
!

remove_int: int1
	"answer the result of calling the receiver's public abstract remove(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'remove' signature: '(I)Ljava/lang/Object;' withArguments: args.
!

removeAtIndex: anIndex
	"one of the root methods for <SequencedGrowableCollection>.
	Answers the object that was at the index"

	^ self remove_int: anIndex - 1.		"0-based indexing in Java"
!

set_int: int1 Object: anObject1
	"answer the result of calling the receiver's public abstract set(int, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			objectAt: 2 put: anObject1;
			yourself.

	^ self callObjectMethod: 'set' signature: '(ILjava/lang/Object;)Ljava/lang/Object;' withArguments: args.
!

subList_int: int1 int: int2
	"answer the result of calling the receiver's public abstract subList(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callObjectMethod: 'subList' signature: '(II)Ljava/util/List;' withArguments: args.
!

subListFrom: aStartIndex to: aStopIndex
	"answer the result of calling the receiver's subList() Java method.  Note that the
	start and stop positions are 1-based, and that aStopIndex *is* included in the range
	unlike the underlying Java method.
	Note that the answer is already wrapped in a JavaUtilList"

	^ (self subList_int: aStartIndex - 1 int: aStopIndex) asAList.! !
!JavaUtilList categoriesFor: #add:afterIndex:!accessing!public! !
!JavaUtilList categoriesFor: #add_int:Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #addAll_int:Collection:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #addLast:!accessing!public! !
!JavaUtilList categoriesFor: #asCollection!converting!public! !
!JavaUtilList categoriesFor: #at:!accessing!public! !
!JavaUtilList categoriesFor: #at:put:!accessing!public! !
!JavaUtilList categoriesFor: #do:!enumerating!public! !
!JavaUtilList categoriesFor: #get_int:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #indexOf_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #lastIndexOf_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #listIterator!accessing!public! !
!JavaUtilList categoriesFor: #listIterator:!accessing!public! !
!JavaUtilList categoriesFor: #listIterator_int:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #listIterator_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #remove_int:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #removeAtIndex:!public!removing! !
!JavaUtilList categoriesFor: #set_int:Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #subList_int:int:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilList categoriesFor: #subListFrom:to:!accessing!public! !

!JavaUtilList class methodsFor!

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
		#add_int:Object:
		#get_int:
		#hashCode_null
		#lastIndexOf_Object:
		#listIterator_int:
		#listIterator_null
		#remove_int:
		#set_int:Object:
		#subList_int:int:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.List'.
! !
!JavaUtilList class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilList class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilList class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilList class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilList class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilSet guid: (GUID fromString: '{57486513-F2C2-4177-AACA-8323E5E73688}')!
JavaUtilSet comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Set'!
!JavaUtilSet categoriesForClass!Unclassified! !
!JavaUtilSet methodsFor!

asCollection
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Collection hierarchy.
	In fact, for these classes we answer a SetAdaptor wrapped around the object.  It acts pretty much
	like a proper Set (the adaptor is limited by the poor internal design of the classic Smalltalk Collections
	hierarchy)"

	adaptorCache isNil ifTrue: [adaptorCache := SetAdaptor for: self].

	^ adaptorCache.! !
!JavaUtilSet categoriesFor: #asCollection!converting!public! !

!JavaUtilSet class methodsFor!

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

	^ #().
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.Set'.
! !
!JavaUtilSet class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilSet class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSet class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSet class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilSet class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilSortedSet guid: (GUID fromString: '{94B581CC-60AC-471A-B6A3-72D4C832E269}')!
JavaUtilSortedSet comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.SortedSet'!
!JavaUtilSortedSet categoriesForClass!Unclassified! !
!JavaUtilSortedSet methodsFor!

comparator
	"answer the result of calling the receiver's comparator() Java method.
	Note that the result is already wrapped in a JavaUtilComparator"

	^ self comparator_null ifNotNil: [:it | it asA: #'java.util.Comparator'].!

comparator_null
	"answer the result of calling the receiver's public abstract comparator() Java method"

	^ self callObjectMethod: 'comparator' signature: '()Ljava/util/Comparator;'.
!

first
	"answer the result of calling the receiver's first() Java method"

	^ self first_null.!

first_null
	"answer the result of calling the receiver's public abstract first() Java method"

	^ self callObjectMethod: 'first' signature: '()Ljava/lang/Object;'.
!

headSet: aJavaObject
	"answer the result of calling the receiver's headSet() Java method.
	The result is already wrapped in a JavaUtilSortedSet"

	^ (self headSet_Object: aJavaObject) asASortedSet.!

headSet_Object: anObject1
	"answer the result of calling the receiver's public abstract headSet(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'headSet' signature: '(Ljava/lang/Object;)Ljava/util/SortedSet;' withArguments: args.
!

last
	"answer the result of calling the receiver's last() Java method"

	^ self last_null.!

last_null
	"answer the result of calling the receiver's public abstract last() Java method"

	^ self callObjectMethod: 'last' signature: '()Ljava/lang/Object;'.
!

subSet: aJavaObject upTo: anotherJavaObject
	"answer the result of calling the receiver's subSet() Java method.
	The result is already wrapped in a JavaUtilSortedSet"

	^ (self subSet_Object: aJavaObject Object: anotherJavaObject) asASortedSet.
!

subSet_Object: anObject1 Object: anObject2
	"answer the result of calling the receiver's public abstract subSet(java.lang.Object, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObject2;
			yourself.

	^ self callObjectMethod: 'subSet' signature: '(Ljava/lang/Object;Ljava/lang/Object;)Ljava/util/SortedSet;' withArguments: args.
!

tailSet: aJavaObject
	"answer the result of calling the receiver's tailSet() Java method.
	The result is already wrapped in a JavaUtilSortedSet"

	^ (self tailSet_Object: aJavaObject) asASortedSet.!

tailSet_Object: anObject1
	"answer the result of calling the receiver's public abstract tailSet(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'tailSet' signature: '(Ljava/lang/Object;)Ljava/util/SortedSet;' withArguments: args.
! !
!JavaUtilSortedSet categoriesFor: #comparator!accessing!public! !
!JavaUtilSortedSet categoriesFor: #comparator_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedSet categoriesFor: #first!accessing!public! !
!JavaUtilSortedSet categoriesFor: #first_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedSet categoriesFor: #headSet:!accessing!public! !
!JavaUtilSortedSet categoriesFor: #headSet_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedSet categoriesFor: #last!accessing!public! !
!JavaUtilSortedSet categoriesFor: #last_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedSet categoriesFor: #subSet:upTo:!accessing!public! !
!JavaUtilSortedSet categoriesFor: #subSet_Object:Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedSet categoriesFor: #tailSet:!accessing!public! !
!JavaUtilSortedSet categoriesFor: #tailSet_Object:!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilSortedSet class methodsFor!

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
		#comparator_null
		#first_null
		#headSet_Object:
		#last_null
		#subSet_Object:Object:
		#tailSet_Object:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.SortedSet'.
! !
!JavaUtilSortedSet class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedSet class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedSet class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedSet class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedSet class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilSortedMap guid: (GUID fromString: '{B30670FD-D31E-47DB-AA29-48EEB8091657}')!
JavaUtilSortedMap comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.SortedMap'!
!JavaUtilSortedMap categoriesForClass!Unclassified! !
!JavaUtilSortedMap methodsFor!

comparator
	"answer the result of calling the receiver's comparator() Java method.
	Note that the result is already wrapped in a JavaUtilComparator"

	^ self comparator_null ifNotNil: [:it | it asA: #'java.util.Comparator'].!

comparator_null
	"answer the result of calling the receiver's public abstract comparator() Java method"

	^ self callObjectMethod: 'comparator' signature: '()Ljava/util/Comparator;'.
!

firstKey
	"answer the result of calling the receiver's firstKey() Java method"

	^ self firstKey_null.!

firstKey_null
	"answer the result of calling the receiver's public abstract firstKey() Java method"

	^ self callObjectMethod: 'firstKey' signature: '()Ljava/lang/Object;'.
!

headMap: aJavaObject
	"answer the result of calling the receiver's headMap() Java method.
	The result is already wrapped in a JavaUtilSortedMap"

	^ (self headMap_Object: aJavaObject) asASortedMap.!

headMap_Object: anObject1
	"answer the result of calling the receiver's public abstract headMap(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'headMap' signature: '(Ljava/lang/Object;)Ljava/util/SortedMap;' withArguments: args.
!

lastKey
	"answer the result of calling the receiver's lastKey() Java method"

	^ self lastKey_null.!

lastKey_null
	"answer the result of calling the receiver's public abstract lastKey() Java method"

	^ self callObjectMethod: 'lastKey' signature: '()Ljava/lang/Object;'.
!

subMap: aJavaObject upTo: anotherJavaObject
	"answer the result of calling the receiver's subMap() Java method.
	The result is already wrapped in a JavaUtilSortedMap"

	^ (self subMap_Object: aJavaObject Object: anotherJavaObject) asASortedMap.
!

subMap_Object: anObject1 Object: anObject2
	"answer the result of calling the receiver's public abstract subMap(java.lang.Object, java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: anObject2;
			yourself.

	^ self callObjectMethod: 'subMap' signature: '(Ljava/lang/Object;Ljava/lang/Object;)Ljava/util/SortedMap;' withArguments: args.
!

tailMap: aJavaObject
	"answer the result of calling the receiver's tailMap() Java method.
	The result is already wrapped in a JavaUtilSortedMap"

	^ (self tailMap_Object: aJavaObject) asASortedMap.
!

tailMap_Object: anObject1
	"answer the result of calling the receiver's public abstract tailMap(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	^ self callObjectMethod: 'tailMap' signature: '(Ljava/lang/Object;)Ljava/util/SortedMap;' withArguments: args.
! !
!JavaUtilSortedMap categoriesFor: #comparator!accessing!public! !
!JavaUtilSortedMap categoriesFor: #comparator_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedMap categoriesFor: #firstKey!accessing!public! !
!JavaUtilSortedMap categoriesFor: #firstKey_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedMap categoriesFor: #headMap:!accessing!public! !
!JavaUtilSortedMap categoriesFor: #headMap_Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedMap categoriesFor: #lastKey!accessing!public! !
!JavaUtilSortedMap categoriesFor: #lastKey_null!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedMap categoriesFor: #subMap:upTo:!accessing!public! !
!JavaUtilSortedMap categoriesFor: #subMap_Object:Object:!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilSortedMap categoriesFor: #tailMap:!accessing!public! !
!JavaUtilSortedMap categoriesFor: #tailMap_Object:!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilSortedMap class methodsFor!

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
		#comparator_null
		#firstKey_null
		#headMap_Object:
		#lastKey_null
		#subMap_Object:Object:
		#tailMap_Object:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.SortedMap'.
! !
!JavaUtilSortedMap class categoriesFor: #generatedConstructorSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedMap class categoriesFor: #generatedGetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedMap class categoriesFor: #generatedSetterSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedMap class categoriesFor: #generatedWrapperSelectors!constants!listing wrapper methods!public! !
!JavaUtilSortedMap class categoriesFor: #javaClassName!accessing!constants!public! !

JavaUtilEnumeration guid: (GUID fromString: '{31B01289-BE77-47DF-83DA-956728F16B0F}')!
JavaUtilEnumeration comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Enumeration'!
!JavaUtilEnumeration categoriesForClass!Unclassified! !
!JavaUtilEnumeration methodsFor!

atEnd
	"part of the normal <ReadStream> protocol"

	^ self hasMoreElements_null not.!

do: a1Block
	"core method for iterables"

	| testMID getMID last |

	"overrriden to gain efficiency by not repeatedly looking up the method IDs"

	testMID := self findMethod: 'hasMoreElements' signature: '()Z'.
	getMID := self findMethod: 'nextElement' signature: '()Ljava/lang/Object;'.

	"we cache the MethodIDs for the durarion of the loop to avoid needless inefficiency"
	[self callBooleanMID: testMID]
		whileTrue: [last := a1Block value: (self callObjectMID: getMID)].

	^ last.
!

hasMoreElements_null
	"answer the result of calling the receiver's public abstract hasMoreElements() Java method"

	^ self callBooleanMethod: 'hasMoreElements'.
!

next
	"part of the normal <ReadStream> protocol"

	^ self nextElement_null.!

nextElement_null
	"answer the result of calling the receiver's public abstract nextElement() Java method"

	^ self callObjectMethod: 'nextElement' signature: '()Ljava/lang/Object;'.
! !
!JavaUtilEnumeration categoriesFor: #atEnd!public!streaming!testing! !
!JavaUtilEnumeration categoriesFor: #do:!enumerating!public!streaming! !
!JavaUtilEnumeration categoriesFor: #hasMoreElements_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilEnumeration categoriesFor: #next!public!streaming! !
!JavaUtilEnumeration categoriesFor: #nextElement_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilEnumeration class methodsFor!

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
		#hasMoreElements_null
		#nextElement_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.Enumeration'.
! !
!JavaUtilEnumeration class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilEnumeration class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilEnumeration class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilEnumeration class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilEnumeration class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaUtilIterator guid: (GUID fromString: '{AB55E22C-7661-40B8-95D3-1CED922A7D67}')!
JavaUtilIterator comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.Iterator'!
!JavaUtilIterator categoriesForClass!Unclassified! !
!JavaUtilIterator methodsFor!

atEnd
	"part of the normal <ReadStream> protocol"

	^ self hasNext_null not.!

do: a1Block
	"core method for iterables"

	| testMID getMID last |

	"overrriden to gain efficiency by not repeatedly looking up the method IDs"

	testMID := self findMethod: 'hasNext' signature: '()Z'.
	getMID := self findMethod: 'next' signature: '()Ljava/lang/Object;'.

	"we cache the MethodIDs for the durarion of the loop to avoid needless inefficiency"
	[self callBooleanMID: testMID]
		whileTrue: [last := a1Block value: (self callObjectMID: getMID)].

	^ last.
!

hasNext
	"answer the result of calling the Java method: hasNext()"

	^ self hasNext_null.!

hasNext_null
	"answer the result of calling the receiver's public abstract hasNext() Java method"

	^ self callBooleanMethod: 'hasNext'.
!

next
	"answer the result of calling the Java method: next()"

	^ self next_null.!

next_null
	"answer the result of calling the receiver's public abstract next() Java method"

	^ self callObjectMethod: 'next' signature: '()Ljava/lang/Object;'.
!

remove
	"remove the current element of the iteration"

	self remove_null.!

remove_null
	"invoke the receiver's public abstract remove() Java method"

	self callVoidMethod: 'remove'.
! !
!JavaUtilIterator categoriesFor: #atEnd!public!streaming!testing! !
!JavaUtilIterator categoriesFor: #do:!enumerating!public!streaming! !
!JavaUtilIterator categoriesFor: #hasNext!public!testing! !
!JavaUtilIterator categoriesFor: #hasNext_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilIterator categoriesFor: #next!accessing!enumerating!public!streaming! !
!JavaUtilIterator categoriesFor: #next_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilIterator categoriesFor: #remove!public!removing! !
!JavaUtilIterator categoriesFor: #remove_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilIterator class methodsFor!

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
		#hasNext_null
		#next_null
		#remove_null
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.Iterator'.
! !
!JavaUtilIterator class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilIterator class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilIterator class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilIterator class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilIterator class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaUtilListIterator guid: (GUID fromString: '{BADAE965-43BE-4FAD-BF12-71C17F490088}')!
JavaUtilListIterator comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class java.util.ListIterator'!
!JavaUtilListIterator categoriesForClass!Unclassified! !
!JavaUtilListIterator methodsFor!

add: aJavaObject
	"call the Java method: add()"

	self add_Object: aJavaObject.!

add_Object: anObject1
	"invoke the receiver's public abstract add(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	self callVoidMethod: 'add' signature: '(Ljava/lang/Object;)V' withArguments: args.
!

asWriteStream
	"answer an adaptor which makes this Java aggregate object look like something from the Smalltalk
	Stream hierarchy"

	^ WriteStreamAdaptor for: self.!

hasNext_null
	"answer the result of calling the receiver's public abstract hasNext() Java method"

	^ self callBooleanMethod: 'hasNext'.
!

hasPrevious
	"answer the result of calling the Java method: hasPrevious()"

	^ self hasPrevious_null.!

hasPrevious_null
	"answer the result of calling the receiver's public abstract hasPrevious() Java method"

	^ self callBooleanMethod: 'hasPrevious'.
!

next_null
	"answer the result of calling the receiver's public abstract next() Java method"

	^ self callObjectMethod: 'next' signature: '()Ljava/lang/Object;'.
!

nextIndex
	"answer the result of calling the Java method: nextIndex().
	Note that the index is 1-based as is proper for Smalltalk, not 0-based
	as in the real Java method"

	^ 1 + self nextIndex_null.!

nextIndex_null
	"answer the result of calling the receiver's public abstract nextIndex() Java method"

	^ self callIntMethod: 'nextIndex'.
!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	self atEnd
		ifTrue: [self add_Object: anObject]
		ifFalse: [self set_Object: anObject].

	^ anObject.!

previous
	"answer the result of calling the Java method: previous()"

	^ self previous_null.!

previous_null
	"answer the result of calling the receiver's public abstract previous() Java method"

	^ self callObjectMethod: 'previous' signature: '()Ljava/lang/Object;'.
!

previousIndex
	"answer the result of calling the Java method: previousIndex().
	Note that the index is 1-based as is proper for Smalltalk, not 0-based
	as in the real Java method"

	^ 1 + self previousIndex_null.!

previousIndex_null
	"answer the result of calling the receiver's public abstract previousIndex() Java method"

	^ self callIntMethod: 'previousIndex'.
!

remove_null
	"invoke the receiver's public abstract remove() Java method"

	self callVoidMethod: 'remove'.
!

set: aJavaObject
	"call the Java method: set()"

	self set_Object: aJavaObject.!

set_Object: anObject1
	"invoke the receiver's public abstract set(java.lang.Object) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anObject1;
			yourself.

	self callVoidMethod: 'set' signature: '(Ljava/lang/Object;)V' withArguments: args.
! !
!JavaUtilListIterator categoriesFor: #add:!adding!public! !
!JavaUtilListIterator categoriesFor: #add_Object:!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #asWriteStream!converting!public! !
!JavaUtilListIterator categoriesFor: #hasNext_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #hasPrevious!public!testing! !
!JavaUtilListIterator categoriesFor: #hasPrevious_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #next_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #nextIndex!accessing!public! !
!JavaUtilListIterator categoriesFor: #nextIndex_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #nextPut:!accessing!public! !
!JavaUtilListIterator categoriesFor: #previous!accessing!public! !
!JavaUtilListIterator categoriesFor: #previous_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #previousIndex!accessing!public! !
!JavaUtilListIterator categoriesFor: #previousIndex_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #remove_null!**auto generated**!Java-abstract!Java-methods!Java-public!public! !
!JavaUtilListIterator categoriesFor: #set:!operations!public! !
!JavaUtilListIterator categoriesFor: #set_Object:!**auto generated**!Java-abstract!Java-methods!Java-public!public! !

!JavaUtilListIterator class methodsFor!

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
		#add_Object:
		#hasNext_null
		#hasPrevious_null
		#next_null
		#nextIndex_null
		#previous_null
		#previousIndex_null
		#remove_null
		#set_Object:
	).
!

javaClassName
	"answer the (JNI or Java format) name of the Java class to which we most closely correspond"

	^ #'java.util.ListIterator'.
! !
!JavaUtilListIterator class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilListIterator class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilListIterator class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilListIterator class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!JavaUtilListIterator class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangNumber guid: (GUID fromString: '{603B4583-5791-4627-B72A-6AD14A53E8FD}')!
StaticJavaLangNumber comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Number and would provide access to its class-side methods and fields if it had any...'!
!StaticJavaLangNumber categoriesForClass!Unclassified! !
!StaticJavaLangNumber class methodsFor!

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
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Number'.
! !
!StaticJavaLangNumber class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangNumber class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangNumber class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangNumber class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangNumber class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangByte guid: (GUID fromString: '{55DC3C7C-A641-4CAA-91FB-F5AA3D529C47}')!
StaticJavaLangByte comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Byte and provide access to its class-side methods and fields.'!
!StaticJavaLangByte categoriesForClass!Unclassified! !
!StaticJavaLangByte methodsFor!

decode_String: aString1
	"answer the result of calling the receiver's public static decode(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'decode' signature: '(Ljava/lang/String;)Ljava/lang/Byte;' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getByteField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getByteField: 'MIN_VALUE'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

new_byte: byte1
	"answer the result of calling the receiver's public new(byte) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			byteAt: 1 put: byte1;
			yourself.

	^ self callConstructorSignature: '(B)V' withArguments: args.
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

parseByte_String: aString1
	"answer the result of calling the receiver's public static parseByte(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callByteMethod: 'parseByte' signature: '(Ljava/lang/String;)B' withArguments: args.
!

parseByte_String: aString1 int: int1
	"answer the result of calling the receiver's public static parseByte(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callByteMethod: 'parseByte' signature: '(Ljava/lang/String;I)B' withArguments: args.
!

toString_byte: byte1
	"answer the result of calling the receiver's public static toString(byte) Java method"

	| args |

	args := (JNIValueArray new: 1)
			byteAt: 1 put: byte1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(B)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Byte;' withArguments: args.
!

valueOf_String: aString1 int: int1
	"answer the result of calling the receiver's public static valueOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;I)Ljava/lang/Byte;' withArguments: args.
! !
!StaticJavaLangByte categoriesFor: #decode_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #new_byte:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangByte categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangByte categoriesFor: #parseByte_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #parseByte_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #toString_byte:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangByte categoriesFor: #valueOf_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangByte class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_byte:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_TYPE
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
		#decode_String:
		#parseByte_String:
		#parseByte_String:int:
		#toString_byte:
		#valueOf_String:
		#valueOf_String:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Byte'.
! !
!StaticJavaLangByte class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangByte class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangByte class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangByte class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangByte class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangDouble guid: (GUID fromString: '{CABA137A-159E-4CF1-9F13-42844C2D799E}')!
StaticJavaLangDouble comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Double and provide access to its class-side methods and fields.'!
!StaticJavaLangDouble categoriesForClass!Unclassified! !
!StaticJavaLangDouble methodsFor!

compare_double: double1 double: double2
	"answer the result of calling the receiver's public static compare(double, double) Java method"

	| args |

	args := (JNIValueArray new: 2)
			doubleAt: 1 put: double1;
			doubleAt: 2 put: double2;
			yourself.

	^ self callIntMethod: 'compare' signature: '(DD)I' withArguments: args.
!

doubleToLongBits_double: double1
	"answer the result of calling the receiver's public static native doubleToLongBits(double) Java method"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callLongMethod: 'doubleToLongBits' signature: '(D)J' withArguments: args.
!

doubleToRawLongBits_double: double1
	"answer the result of calling the receiver's public static native doubleToRawLongBits(double) Java method"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callLongMethod: 'doubleToRawLongBits' signature: '(D)J' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getDoubleField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getDoubleField: 'MIN_VALUE'.
!

get_NaN
	"answer the value of the receiver's public static final NaN Java field"

	^ self getDoubleField: 'NaN'.
!

get_NEGATIVE_INFINITY
	"answer the value of the receiver's public static final NEGATIVE_INFINITY Java field"

	^ self getDoubleField: 'NEGATIVE_INFINITY'.
!

get_POSITIVE_INFINITY
	"answer the value of the receiver's public static final POSITIVE_INFINITY Java field"

	^ self getDoubleField: 'POSITIVE_INFINITY'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

isInfinite_double: double1
	"answer the result of calling the receiver's public static isInfinite(double) Java method"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callBooleanMethod: 'isInfinite' signature: '(D)Z' withArguments: args.
!

isNaN_double: double1
	"answer the result of calling the receiver's public static isNaN(double) Java method"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callBooleanMethod: 'isNaN' signature: '(D)Z' withArguments: args.
!

longBitsToDouble_long: long1
	"answer the result of calling the receiver's public static native longBitsToDouble(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callDoubleMethod: 'longBitsToDouble' signature: '(J)D' withArguments: args.
!

new_double: double1
	"answer the result of calling the receiver's public new(double) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callConstructorSignature: '(D)V' withArguments: args.
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

parseDouble_String: aString1
	"answer the result of calling the receiver's public static parseDouble(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callDoubleMethod: 'parseDouble' signature: '(Ljava/lang/String;)D' withArguments: args.
!

toString_double: double1
	"answer the result of calling the receiver's public static toString(double) Java method"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(D)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Double;' withArguments: args.
! !
!StaticJavaLangDouble categoriesFor: #compare_double:double:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #doubleToLongBits_double:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #doubleToRawLongBits_double:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_NaN!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_NEGATIVE_INFINITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_POSITIVE_INFINITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #isInfinite_double:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #isNaN_double:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #longBitsToDouble_long:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #new_double:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangDouble categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangDouble categoriesFor: #parseDouble_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #toString_double:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangDouble categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangDouble class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_double:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_NaN
		#get_NEGATIVE_INFINITY
		#get_POSITIVE_INFINITY
		#get_TYPE
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
		#compare_double:double:
		#doubleToLongBits_double:
		#doubleToRawLongBits_double:
		#isInfinite_double:
		#isNaN_double:
		#longBitsToDouble_long:
		#parseDouble_String:
		#toString_double:
		#valueOf_String:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Double'.
! !
!StaticJavaLangDouble class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangDouble class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangDouble class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangDouble class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangDouble class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangFloat guid: (GUID fromString: '{31048B02-83C0-44A0-9B3D-81B241425D71}')!
StaticJavaLangFloat comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Float and provide access to its class-side methods and fields.'!
!StaticJavaLangFloat categoriesForClass!Unclassified! !
!StaticJavaLangFloat methodsFor!

compare_float: float1 float: float2
	"answer the result of calling the receiver's public static compare(float, float) Java method"

	| args |

	args := (JNIValueArray new: 2)
			floatAt: 1 put: float1;
			floatAt: 2 put: float2;
			yourself.

	^ self callIntMethod: 'compare' signature: '(FF)I' withArguments: args.
!

floatToIntBits_float: float1
	"answer the result of calling the receiver's public static native floatToIntBits(float) Java method"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callIntMethod: 'floatToIntBits' signature: '(F)I' withArguments: args.
!

floatToRawIntBits_float: float1
	"answer the result of calling the receiver's public static native floatToRawIntBits(float) Java method"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callIntMethod: 'floatToRawIntBits' signature: '(F)I' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getFloatField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getFloatField: 'MIN_VALUE'.
!

get_NaN
	"answer the value of the receiver's public static final NaN Java field"

	^ self getFloatField: 'NaN'.
!

get_NEGATIVE_INFINITY
	"answer the value of the receiver's public static final NEGATIVE_INFINITY Java field"

	^ self getFloatField: 'NEGATIVE_INFINITY'.
!

get_POSITIVE_INFINITY
	"answer the value of the receiver's public static final POSITIVE_INFINITY Java field"

	^ self getFloatField: 'POSITIVE_INFINITY'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

intBitsToFloat_int: int1
	"answer the result of calling the receiver's public static native intBitsToFloat(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callFloatMethod: 'intBitsToFloat' signature: '(I)F' withArguments: args.
!

isInfinite_float: float1
	"answer the result of calling the receiver's public static isInfinite(float) Java method"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callBooleanMethod: 'isInfinite' signature: '(F)Z' withArguments: args.
!

isNaN_float: float1
	"answer the result of calling the receiver's public static isNaN(float) Java method"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callBooleanMethod: 'isNaN' signature: '(F)Z' withArguments: args.
!

new_double: double1
	"answer the result of calling the receiver's public new(double) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			doubleAt: 1 put: double1;
			yourself.

	^ self callConstructorSignature: '(D)V' withArguments: args.
!

new_float: float1
	"answer the result of calling the receiver's public new(float) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callConstructorSignature: '(F)V' withArguments: args.
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

parseFloat_String: aString1
	"answer the result of calling the receiver's public static parseFloat(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callFloatMethod: 'parseFloat' signature: '(Ljava/lang/String;)F' withArguments: args.
!

toString_float: float1
	"answer the result of calling the receiver's public static toString(float) Java method"

	| args |

	args := (JNIValueArray new: 1)
			floatAt: 1 put: float1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(F)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Float;' withArguments: args.
! !
!StaticJavaLangFloat categoriesFor: #compare_float:float:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #floatToIntBits_float:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #floatToRawIntBits_float:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_NaN!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_NEGATIVE_INFINITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_POSITIVE_INFINITY!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #intBitsToFloat_int:!**auto generated**!Java-methods!Java-native!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #isInfinite_float:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #isNaN_float:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #new_double:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangFloat categoriesFor: #new_float:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangFloat categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangFloat categoriesFor: #parseFloat_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #toString_float:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangFloat categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangFloat class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_double:
		#new_float:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_NaN
		#get_NEGATIVE_INFINITY
		#get_POSITIVE_INFINITY
		#get_TYPE
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
		#compare_float:float:
		#floatToIntBits_float:
		#floatToRawIntBits_float:
		#intBitsToFloat_int:
		#isInfinite_float:
		#isNaN_float:
		#parseFloat_String:
		#toString_float:
		#valueOf_String:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Float'.
! !
!StaticJavaLangFloat class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangFloat class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangFloat class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangFloat class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangFloat class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangInteger guid: (GUID fromString: '{FFB83C55-21D8-4721-965B-5A6905E10613}')!
StaticJavaLangInteger comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Integer and provide access to its class-side methods and fields.'!
!StaticJavaLangInteger categoriesForClass!Unclassified! !
!StaticJavaLangInteger methodsFor!

decode_String: aString1
	"answer the result of calling the receiver's public static decode(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'decode' signature: '(Ljava/lang/String;)Ljava/lang/Integer;' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getIntField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getIntField: 'MIN_VALUE'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

getInteger_String: aString1
	"answer the result of calling the receiver's public static getInteger(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getInteger' signature: '(Ljava/lang/String;)Ljava/lang/Integer;' withArguments: args.
!

getInteger_String: aString1 int: int1
	"answer the result of calling the receiver's public static getInteger(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'getInteger' signature: '(Ljava/lang/String;I)Ljava/lang/Integer;' withArguments: args.
!

getInteger_String: aString1 Integer: anInteger1
	"answer the result of calling the receiver's public static getInteger(java.lang.String, java.lang.Integer) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: anInteger1;
			yourself.

	^ self callObjectMethod: 'getInteger' signature: '(Ljava/lang/String;Ljava/lang/Integer;)Ljava/lang/Integer;' withArguments: args.
!

new_int: int1
	"answer the result of calling the receiver's public new(int) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callConstructorSignature: '(I)V' withArguments: args.
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

parseInt_String: aString1
	"answer the result of calling the receiver's public static parseInt(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callIntMethod: 'parseInt' signature: '(Ljava/lang/String;)I' withArguments: args.
!

parseInt_String: aString1 int: int1
	"answer the result of calling the receiver's public static parseInt(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callIntMethod: 'parseInt' signature: '(Ljava/lang/String;I)I' withArguments: args.
!

toBinaryString_int: int1
	"answer the result of calling the receiver's public static toBinaryString(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'toBinaryString' signature: '(I)Ljava/lang/String;' withArguments: args.
!

toHexString_int: int1
	"answer the result of calling the receiver's public static toHexString(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'toHexString' signature: '(I)Ljava/lang/String;' withArguments: args.
!

toOctalString_int: int1
	"answer the result of calling the receiver's public static toOctalString(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'toOctalString' signature: '(I)Ljava/lang/String;' withArguments: args.
!

toString_int: int1
	"answer the result of calling the receiver's public static toString(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(I)Ljava/lang/String;' withArguments: args.
!

toString_int: int1 int: int2
	"answer the result of calling the receiver's public static toString(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(II)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Integer;' withArguments: args.
!

valueOf_String: aString1 int: int1
	"answer the result of calling the receiver's public static valueOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;I)Ljava/lang/Integer;' withArguments: args.
! !
!StaticJavaLangInteger categoriesFor: #decode_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #getInteger_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #getInteger_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #getInteger_String:Integer:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #new_int:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangInteger categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangInteger categoriesFor: #parseInt_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #parseInt_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #toBinaryString_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #toHexString_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #toOctalString_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #toString_int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #toString_int:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangInteger categoriesFor: #valueOf_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangInteger class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_int:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_TYPE
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
		#decode_String:
		#getInteger_String:
		#getInteger_String:int:
		#getInteger_String:Integer:
		#parseInt_String:
		#parseInt_String:int:
		#toBinaryString_int:
		#toHexString_int:
		#toOctalString_int:
		#toString_int:
		#toString_int:int:
		#valueOf_String:
		#valueOf_String:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Integer'.
! !
!StaticJavaLangInteger class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangInteger class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangInteger class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangInteger class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangInteger class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangLong guid: (GUID fromString: '{C2EB8469-EEFD-48E9-8D3F-7FFE77A4EA0C}')!
StaticJavaLangLong comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Long and provide access to its class-side methods and fields.'!
!StaticJavaLangLong categoriesForClass!Unclassified! !
!StaticJavaLangLong methodsFor!

decode_String: aString1
	"answer the result of calling the receiver's public static decode(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'decode' signature: '(Ljava/lang/String;)Ljava/lang/Long;' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getLongField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getLongField: 'MIN_VALUE'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

getLong_String: aString1
	"answer the result of calling the receiver's public static getLong(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'getLong' signature: '(Ljava/lang/String;)Ljava/lang/Long;' withArguments: args.
!

getLong_String: aString1 long: long1
	"answer the result of calling the receiver's public static getLong(java.lang.String, long) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			longAt: 2 put: long1;
			yourself.

	^ self callObjectMethod: 'getLong' signature: '(Ljava/lang/String;J)Ljava/lang/Long;' withArguments: args.
!

getLong_String: aString1 Long: aLong1
	"answer the result of calling the receiver's public static getLong(java.lang.String, java.lang.Long) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aLong1;
			yourself.

	^ self callObjectMethod: 'getLong' signature: '(Ljava/lang/String;Ljava/lang/Long;)Ljava/lang/Long;' withArguments: args.
!

new_long: long1
	"answer the result of calling the receiver's public new(long) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callConstructorSignature: '(J)V' withArguments: args.
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

parseLong_String: aString1
	"answer the result of calling the receiver's public static parseLong(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callLongMethod: 'parseLong' signature: '(Ljava/lang/String;)J' withArguments: args.
!

parseLong_String: aString1 int: int1
	"answer the result of calling the receiver's public static parseLong(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callLongMethod: 'parseLong' signature: '(Ljava/lang/String;I)J' withArguments: args.
!

toBinaryString_long: long1
	"answer the result of calling the receiver's public static toBinaryString(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callObjectMethod: 'toBinaryString' signature: '(J)Ljava/lang/String;' withArguments: args.
!

toHexString_long: long1
	"answer the result of calling the receiver's public static toHexString(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callObjectMethod: 'toHexString' signature: '(J)Ljava/lang/String;' withArguments: args.
!

toOctalString_long: long1
	"answer the result of calling the receiver's public static toOctalString(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callObjectMethod: 'toOctalString' signature: '(J)Ljava/lang/String;' withArguments: args.
!

toString_long: long1
	"answer the result of calling the receiver's public static toString(long) Java method"

	| args |

	args := (JNIValueArray new: 1)
			longAt: 1 put: long1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(J)Ljava/lang/String;' withArguments: args.
!

toString_long: long1 int: int1
	"answer the result of calling the receiver's public static toString(long, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			longAt: 1 put: long1;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(JI)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Long;' withArguments: args.
!

valueOf_String: aString1 int: int1
	"answer the result of calling the receiver's public static valueOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;I)Ljava/lang/Long;' withArguments: args.
! !
!StaticJavaLangLong categoriesFor: #decode_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #getLong_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #getLong_String:long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #getLong_String:Long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #new_long:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangLong categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangLong categoriesFor: #parseLong_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #parseLong_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #toBinaryString_long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #toHexString_long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #toOctalString_long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #toString_long:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #toString_long:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangLong categoriesFor: #valueOf_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangLong class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_long:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_TYPE
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
		#decode_String:
		#getLong_String:
		#getLong_String:Long:
		#getLong_String:long:
		#parseLong_String:
		#parseLong_String:int:
		#toBinaryString_long:
		#toHexString_long:
		#toOctalString_long:
		#toString_long:
		#toString_long:int:
		#valueOf_String:
		#valueOf_String:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Long'.
! !
!StaticJavaLangLong class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangLong class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangLong class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangLong class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangLong class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticJavaLangShort guid: (GUID fromString: '{0B12F29F-61BB-4D3B-9943-E5DB95EB5992}')!
StaticJavaLangShort comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

Instances reify the Java class java.lang.Short and provide access to its class-side methods and fields.'!
!StaticJavaLangShort categoriesForClass!Unclassified! !
!StaticJavaLangShort methodsFor!

decode_String: aString1
	"answer the result of calling the receiver's public static decode(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'decode' signature: '(Ljava/lang/String;)Ljava/lang/Short;' withArguments: args.
!

get_MAX_VALUE
	"answer the value of the receiver's public static final MAX_VALUE Java field"

	^ self getShortField: 'MAX_VALUE'.
!

get_MIN_VALUE
	"answer the value of the receiver's public static final MIN_VALUE Java field"

	^ self getShortField: 'MIN_VALUE'.
!

get_TYPE
	"answer the value of the receiver's public static final TYPE Java field"

	^ self getObjectField: 'TYPE' signature: 'Ljava/lang/Class;'.
!

new_short: short1
	"answer the result of calling the receiver's public new(short) Java constructor"

	| args |

	args := (JNIValueArray new: 1)
			shortAt: 1 put: short1;
			yourself.

	^ self callConstructorSignature: '(S)V' withArguments: args.
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

parseShort_String: aString1
	"answer the result of calling the receiver's public static parseShort(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callShortMethod: 'parseShort' signature: '(Ljava/lang/String;)S' withArguments: args.
!

parseShort_String: aString1 int: int1
	"answer the result of calling the receiver's public static parseShort(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callShortMethod: 'parseShort' signature: '(Ljava/lang/String;I)S' withArguments: args.
!

toString_short: short1
	"answer the result of calling the receiver's public static toString(short) Java method"

	| args |

	args := (JNIValueArray new: 1)
			shortAt: 1 put: short1;
			yourself.

	^ self callObjectMethod: 'toString' signature: '(S)Ljava/lang/String;' withArguments: args.
!

valueOf_String: aString1
	"answer the result of calling the receiver's public static valueOf(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;)Ljava/lang/Short;' withArguments: args.
!

valueOf_String: aString1 int: int1
	"answer the result of calling the receiver's public static valueOf(java.lang.String, int) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			intAt: 2 put: int1;
			yourself.

	^ self callObjectMethod: 'valueOf' signature: '(Ljava/lang/String;I)Ljava/lang/Short;' withArguments: args.
! !
!StaticJavaLangShort categoriesFor: #decode_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #get_MAX_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #get_MIN_VALUE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #get_TYPE!**auto generated**!Java-fields!Java-final!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #new_short:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangShort categoriesFor: #new_String:!**auto generated**!Java-constructors!Java-public!public! !
!StaticJavaLangShort categoriesFor: #parseShort_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #parseShort_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #toString_short:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #valueOf_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticJavaLangShort categoriesFor: #valueOf_String:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticJavaLangShort class methodsFor!

generatedConstructorSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#new_short:
		#new_String:
	).
!

generatedGetterSelectors
	"answer an Array of the selectors of automatically generated methods.
	Note that this does not include inherited selectors"

	^ #(
		#get_MAX_VALUE
		#get_MIN_VALUE
		#get_TYPE
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
		#decode_String:
		#parseShort_String:
		#parseShort_String:int:
		#toString_short:
		#valueOf_String:
		#valueOf_String:int:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'java.lang.Short'.
! !
!StaticJavaLangShort class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangShort class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangShort class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangShort class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticJavaLangShort class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JavaEnumerationAdaptor guid: (GUID fromString: '{4B3C9182-3994-4508-AA39-FF45C688D70E}')!
JavaEnumerationAdaptor comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of thise will wrap any JavaInstance which implements the java.util.Enumeration interface.  It acts as an adaptor to allow the wrappee to look like a ReadStream.

Note: this uses the wrapper-style messages, #hasMoreElements_null and #nextElement_null, to call the Java methods.'!
!JavaEnumerationAdaptor categoriesForClass!Unclassified! !
!JavaEnumerationAdaptor methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	^ subject hasMoreElements_null not.!

next
	"one of the root methods for <ReadStream>"

	^ subject nextElement_null.!

position: anInteger
	"one of the root methods for positionable Streams"

	"enumerations are not seekable"
	self shouldNotImplement.
!

subject: aJavaClassInstance
	"private -- set the object to which we forward the basic Java enumeration messages"

	subject := aJavaClassInstance.
! !
!JavaEnumerationAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!JavaEnumerationAdaptor categoriesFor: #next!accessing!enumerating!public!streaming! !
!JavaEnumerationAdaptor categoriesFor: #position:!must implement!positioning!public! !
!JavaEnumerationAdaptor categoriesFor: #subject:!initializing!private! !

!JavaEnumerationAdaptor class methodsFor!

for: aJavaClassInstance
	"answer a new instance which wraps aJavaClassInstance (which is assumed to be of a class which
	implments java.util.Enumeration), and makes it act like ReadStream"

	^ (self basicNew)
		subject: aJavaClassInstance;
		yourself.! !
!JavaEnumerationAdaptor class categoriesFor: #for:!instance creation!public! !

JavaIteratorAdaptor guid: (GUID fromString: '{FE71F717-3EF0-4081-BFD5-F31D9CBDAFFA}')!
JavaIteratorAdaptor comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of thise will wrap any JavaClassInstance which implements the java.util.Iterator interface.  It acts as an adaptor to allow the wrappee to look like a ReadStream.

Note: this uses the wrapper-style messages, #hasNext_null and #next_null, to call the Java methods.'!
!JavaIteratorAdaptor categoriesForClass!Unclassified! !
!JavaIteratorAdaptor methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	^ subject hasNext_null not.!

next
	"one of the root methods for <ReadStream>"

	^ subject next_null.!

position: anInteger
	"one of the root methods for positionable Streams"

	"iterations are not seekable"
	self shouldNotImplement.
!

subject: aJavaClassInstance
	"private -- set the object to which we forward the basic Java iteration messages"

	subject := aJavaClassInstance.
! !
!JavaIteratorAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!JavaIteratorAdaptor categoriesFor: #next!accessing!enumerating!public!streaming! !
!JavaIteratorAdaptor categoriesFor: #position:!must implement!positioning!public! !
!JavaIteratorAdaptor categoriesFor: #subject:!initializing!private! !

!JavaIteratorAdaptor class methodsFor!

for: aJavaClassInstance
	"answer a new instance which wraps aJavaClassInstance (which is assumed to be of a class which
	implments java.util.Iterator), and makes it act like ReadStream"

	^ (self basicNew)
		subject: aJavaClassInstance;
		yourself.! !
!JavaIteratorAdaptor class categoriesFor: #for:!instance creation!public! !

JavaWriteListIteratorAdaptor guid: (GUID fromString: '{39401FDD-7457-4783-A62B-C27FDE946F15}')!
JavaWriteListIteratorAdaptor comment: 'Copyright © Chris Uppal, 2001-2003.
chris.uppal@metagnostic.org

One of thise will wrap any JavaInstance which implements the java.util.ListIterator interface.  It acts as an adaptor to allow the wrappee to look like a WriteStream.

Note: this uses the wrapper-style messages, #hasNext_null, #add_Object:, and #set_Object:, to call the Java methods.'!
!JavaWriteListIteratorAdaptor categoriesForClass!Unclassified! !
!JavaWriteListIteratorAdaptor methodsFor!

atEnd
	"one of the root methods for <ReadStream>"

	^ subject hasNext_null not.!

nextPut: anObject
	"one of the root methods of <WriteStream>"

	subject hasNext_null
		ifTrue: [self set_Object: anObject]
		ifFalse: [self add_Object: anObject].

	^ anObject.!

subject: aJavaClassInstance
	"private -- set the object to which we forward the basic Java iteration messages"

	subject := aJavaClassInstance.
! !
!JavaWriteListIteratorAdaptor categoriesFor: #atEnd!public!streaming!testing! !
!JavaWriteListIteratorAdaptor categoriesFor: #nextPut:!public!writing! !
!JavaWriteListIteratorAdaptor categoriesFor: #subject:!initializing!private! !

!JavaWriteListIteratorAdaptor class methodsFor!

for: aJavaClassInstance
	"answer a new instance which wraps aJavaClassInstance (which is assumed to be of a class which
	implements java.util.ListIterator), and makes it act like WriteStream"

	^ (self basicNew)
		subject: aJavaClassInstance;
		yourself.! !
!JavaWriteListIteratorAdaptor class categoriesFor: #for:!instance creation!public! !

"Binary Globals"!

"Resources"!

