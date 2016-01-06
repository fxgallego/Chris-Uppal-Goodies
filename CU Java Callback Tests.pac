| package |
package := Package name: 'CU Java Callback Tests'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A few regression tests for the JNIPort callback facility.

If you are using this package then the corresponding Java code (shipped in the "Extras\JNIPort-Tests.jar" file) must be on the Java classpath somewhere (one way to ensure this is to add the file to the classpath in the JVM settings). The Java source is in the corresponding zip file.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.02'.


package classNames
	add: #JNIPortCallbackEventsTest;
	add: #JNIPortCallbackNotificationsTest;
	add: #JNIPortCallbackRequestsTest;
	add: #OMJTRCallbacks;
	add: #OMJTRNotifications;
	add: #OMJTRTestEventObject;
	add: #OMJTRTestEventSource;
	add: #StaticOMJTRCallbacks;
	add: #StaticOMJTREvents;
	add: #StaticOMJTRNotifications;
	yourself.

package methodNames
	add: #GenericJNIPortRegressionTest -> #pumpMessagesFor:;
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
	add: 'CU Java Callbacks';
	add: 'CU JNI';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package setManualPrerequisites: #(
	'CU Java Callbacks').

package!

"Class Definitions"!

JavaLangObject subclass: #OMJTRCallbacks
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #OMJTRNotifications
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #OMJTRTestEventObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #OMJTRTestEventSource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticOMJTRCallbacks
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticOMJTREvents
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticOMJTRNotifications
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortCallbackEventsTest
	instanceVariableNames: 'classStatic eventSource eventForwarder eventMethod eventsReceived'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortCallbackNotificationsTest
	instanceVariableNames: 'instance classStatic notificationsStatus'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericJNIPortRegressionTest subclass: #JNIPortCallbackRequestsTest
	instanceVariableNames: 'instance classStatic callbacksStatus'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!GenericJNIPortRegressionTest methodsFor!

pumpMessagesFor: anInteger
	"if we are the main loop then pump windows messages for anInteger number of milliseconds
	whilst ignoring keyboard and mouse input.  Returns after the specified delay.
	This is used to wait for callbacks without blocking the main loop"

	| loop |

	loop := Processor activeProcess isMain
					ifTrue: [ModalMsgLoop new]
					ifFalse: [Semaphore new].

	[(Delay forMilliseconds: anInteger) wait. loop signal] fork.

	loop wait.

! !
!GenericJNIPortRegressionTest categoriesFor: #pumpMessagesFor:!helpers!private! !

"End of package definition"!

"Source Globals"!

"Classes"!

OMJTRCallbacks guid: (GUID fromString: '{8B5E3B4E-74C1-461D-84C0-B871AD511BF5}')!
OMJTRCallbacks comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!OMJTRCallbacks categoriesForClass!Unclassified! !
!OMJTRCallbacks methodsFor!

callback0_null
	"invoke the receiver's public callback0() Java method"

	self callVoidMethod: 'callback0'.
!

callback1_null
	"invoke the receiver's public callback1() Java method"

	self callVoidMethod: 'callback1'.
!

callback2_null
	"invoke the receiver's public callback2() Java method"

	self callVoidMethod: 'callback2'.
!

callbackAndReturn_int: int1
	"answer the result of calling the receiver's public callbackAndReturn(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callIntMethod: 'callbackAndReturn' signature: '(I)I' withArguments: args.
!

callbackAndThrowNPE_null
	"answer the result of calling the receiver's public callbackAndThrowNPE() Java method"

	^ self callBooleanMethod: 'callbackAndThrowNPE'.
!

callbackAndThrowOther_null
	"answer the result of calling the receiver's public callbackAndThrowOther() Java method"

	^ self callBooleanMethod: 'callbackAndThrowOther'.
!

callbackInBackground_int: int1 int: int2
	"invoke the receiver's public callbackInBackground(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	self callVoidMethod: 'callbackInBackground' signature: '(II)V' withArguments: args.
!

callbackInLine_null
	"invoke the receiver's public callbackInLine() Java method"

	self callVoidMethod: 'callbackInLine'.
! !
!OMJTRCallbacks categoriesFor: #callback0_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callback1_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callback2_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callbackAndReturn_int:!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callbackAndThrowNPE_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callbackAndThrowOther_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callbackInBackground_int:int:!**auto generated**!Java-methods!Java-public!public! !
!OMJTRCallbacks categoriesFor: #callbackInLine_null!**auto generated**!Java-methods!Java-public!public! !

!OMJTRCallbacks class methodsFor!

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
		#callback0_null
		#callback1_null
		#callback2_null
		#callbackAndReturn_int:
		#callbackAndThrowNPE_null
		#callbackAndThrowOther_null
		#callbackInBackground_int:int:
		#callbackInLine_null
	).
!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later).
	Override in subclasses, to force this"

	"we'll have canonical instances; it isn't *necessary* in order to use
	callbacks, but it's probably the typical case, so we may as do be
	typical in this test class"
	^ true.!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.Callbacks'.
! !
!OMJTRCallbacks class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRCallbacks class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRCallbacks class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRCallbacks class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRCallbacks class categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!OMJTRCallbacks class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

OMJTRNotifications guid: (GUID fromString: '{E318D1FF-2CCA-47AD-9AC5-19E38E898564}')!
OMJTRNotifications comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!OMJTRNotifications categoriesForClass!Unclassified! !
!OMJTRNotifications methodsFor!

notify0_null
	"invoke the receiver's public notify0() Java method"

	self callVoidMethod: 'notify0'.
!

notify1_null
	"invoke the receiver's public notify1() Java method"

	self callVoidMethod: 'notify1'.
!

notify2_null
	"invoke the receiver's public notify2() Java method"

	self callVoidMethod: 'notify2'.
!

notifyAndThrowNPE_null
	"invoke the receiver's public notifyAndThrowNPE() Java method"

	self callVoidMethod: 'notifyAndThrowNPE'.
!

notifyAndThrowOther_null
	"invoke the receiver's public notifyAndThrowOther() Java method"

	self callVoidMethod: 'notifyAndThrowOther'.
!

notifyInBackground_int: int1 int: int2
	"invoke the receiver's public notifyInBackground(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	self callVoidMethod: 'notifyInBackground' signature: '(II)V' withArguments: args.
!

notifyInLine_null
	"invoke the receiver's public notifyInLine() Java method"

	self callVoidMethod: 'notifyInLine'.
! !
!OMJTRNotifications categoriesFor: #notify0_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notify1_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notify2_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notifyAndThrowNPE_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notifyAndThrowOther_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notifyInBackground_int:int:!**auto generated**!Java-methods!Java-public!public! !
!OMJTRNotifications categoriesFor: #notifyInLine_null!**auto generated**!Java-methods!Java-public!public! !

!OMJTRNotifications class methodsFor!

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
		#notify0_null
		#notify1_null
		#notify2_null
		#notifyAndThrowNPE_null
		#notifyAndThrowOther_null
		#notifyInBackground_int:int:
		#notifyInLine_null
	).
!

hasCanonicalInstancesByDefault
	"answer whether we should have canonical instances at startup.
	(if not then it can always be turned on later).
	Override in subclasses, to force this"

	"we'll have canonical instances; it isn't *necessary* in order to use
	callbacks, but it's probably the typical case, so we may as do be
	typical in this test class"
	^ true.!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.Notifications'.
! !
!OMJTRNotifications class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRNotifications class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRNotifications class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRNotifications class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRNotifications class categoriesFor: #hasCanonicalInstancesByDefault!canonical instances!constants!managed objects!public! !
!OMJTRNotifications class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

OMJTRTestEventObject guid: (GUID fromString: '{FE624FBE-8D0E-4091-8624-C1F64FDCA9B0}')!
OMJTRTestEventObject comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!OMJTRTestEventObject categoriesForClass!Unclassified! !
!OMJTRTestEventObject methodsFor!

getText_null
	"answer the result of calling the receiver's public getText() Java method"

	^ self callObjectMethod: 'getText' signature: '()Ljava/lang/String;'.
!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
! !
!OMJTRTestEventObject categoriesFor: #getText_null!**auto generated**!Java-methods!Java-public!public! !
!OMJTRTestEventObject categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !

!OMJTRTestEventObject class methodsFor!

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
		#getText_null
		#toString_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.TestEventObject'.
! !
!OMJTRTestEventObject class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventObject class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventObject class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventObject class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventObject class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

OMJTRTestEventSource guid: (GUID fromString: '{6311D1AD-F5AE-497E-AD2D-DC278B669DFE}')!
OMJTRTestEventSource comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!OMJTRTestEventSource categoriesForClass!Unclassified! !
!OMJTRTestEventSource methodsFor!

addListener_TestEventListener: aTestEventListener1
	"invoke the receiver's public addListener(org.metagnostic.jniport.test.regression.TestEventListener) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aTestEventListener1;
			yourself.

	self callVoidMethod: 'addListener' signature: '(Lorg/metagnostic/jniport/test/regression/TestEventListener;)V' withArguments: args.
!

removeListener_TestEventListener: aTestEventListener1
	"invoke the receiver's public removeListener(org.metagnostic.jniport.test.regression.TestEventListener) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aTestEventListener1;
			yourself.

	self callVoidMethod: 'removeListener' signature: '(Lorg/metagnostic/jniport/test/regression/TestEventListener;)V' withArguments: args.
!

triggerTestEvent_String: aString1
	"invoke the receiver's public triggerTestEvent(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'triggerTestEvent' signature: '(Ljava/lang/String;)V' withArguments: args.
! !
!OMJTRTestEventSource categoriesFor: #addListener_TestEventListener:!**auto generated**!Java-methods!Java-public!public! !
!OMJTRTestEventSource categoriesFor: #removeListener_TestEventListener:!**auto generated**!Java-methods!Java-public!public! !
!OMJTRTestEventSource categoriesFor: #triggerTestEvent_String:!**auto generated**!Java-methods!Java-public!public! !

!OMJTRTestEventSource class methodsFor!

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
		#addListener_TestEventListener:
		#removeListener_TestEventListener:
		#triggerTestEvent_String:
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.TestEventSource'.
! !
!OMJTRTestEventSource class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventSource class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventSource class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventSource class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OMJTRTestEventSource class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticOMJTRCallbacks guid: (GUID fromString: '{DBFFD52A-38A1-4BFE-85CA-D5CE4EB29A92}')!
StaticOMJTRCallbacks comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!StaticOMJTRCallbacks categoriesForClass!Unclassified! !
!StaticOMJTRCallbacks methodsFor!

callback0Tag_null
	"answer the result of calling the receiver's public static callback0Tag() Java method"

	^ self callObjectMethod: 'callback0Tag' signature: '()Ljava/lang/Object;'.
!

callback1Tag_null
	"answer the result of calling the receiver's public static callback1Tag() Java method"

	^ self callObjectMethod: 'callback1Tag' signature: '()Ljava/lang/Object;'.
!

callback2Tag_null
	"answer the result of calling the receiver's public static callback2Tag() Java method"

	^ self callObjectMethod: 'callback2Tag' signature: '()Ljava/lang/Object;'.
!

errorCallbackTag_null
	"answer the result of calling the receiver's public static errorCallbackTag() Java method"

	^ self callObjectMethod: 'errorCallbackTag' signature: '()Ljava/lang/Object;'.
!

failingCallbackTag_null
	"answer the result of calling the receiver's public static failingCallbackTag() Java method"

	^ self callObjectMethod: 'failingCallbackTag' signature: '()Ljava/lang/Object;'.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
! !
!StaticOMJTRCallbacks categoriesFor: #callback0Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRCallbacks categoriesFor: #callback1Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRCallbacks categoriesFor: #callback2Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRCallbacks categoriesFor: #errorCallbackTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRCallbacks categoriesFor: #failingCallbackTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRCallbacks categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !

!StaticOMJTRCallbacks class methodsFor!

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
		#callback0Tag_null
		#callback1Tag_null
		#callback2Tag_null
		#errorCallbackTag_null
		#failingCallbackTag_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.Callbacks'.
! !
!StaticOMJTRCallbacks class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRCallbacks class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRCallbacks class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRCallbacks class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRCallbacks class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticOMJTREvents guid: (GUID fromString: '{A1549D32-1B9D-48EC-A776-69C433BCA11C}')!
StaticOMJTREvents comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!StaticOMJTREvents categoriesForClass!Unclassified! !
!StaticOMJTREvents methodsFor!

getEventMethod_null
	"answer the result of calling the receiver's public static getEventMethod() Java method"

	^ self callObjectMethod: 'getEventMethod' signature: '()Ljava/lang/reflect/Method;'.
!

getEventSource_null
	"answer the result of calling the receiver's public static getEventSource() Java method"

	^ self callObjectMethod: 'getEventSource' signature: '()Lorg/metagnostic/jniport/test/regression/TestEventSource;'.
!

main_StringArray: aStrings1
	"invoke the receiver's public static main(java.lang.String[]) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aStrings1;
			yourself.

	self callVoidMethod: 'main' signature: '([Ljava/lang/String;)V' withArguments: args.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

triggerInBackground_int: int1 int: int2
	"invoke the receiver's public static triggerInBackground(int, int) Java method"

	| args |

	args := (JNIValueArray new: 2)
			intAt: 1 put: int1;
			intAt: 2 put: int2;
			yourself.

	self callVoidMethod: 'triggerInBackground' signature: '(II)V' withArguments: args.
!

triggerInLine_null
	"invoke the receiver's public static triggerInLine() Java method"

	self callVoidMethod: 'triggerInLine'.
! !
!StaticOMJTREvents categoriesFor: #getEventMethod_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTREvents categoriesFor: #getEventSource_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTREvents categoriesFor: #main_StringArray:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTREvents categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticOMJTREvents categoriesFor: #triggerInBackground_int:int:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTREvents categoriesFor: #triggerInLine_null!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticOMJTREvents class methodsFor!

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
		#getEventMethod_null
		#getEventSource_null
		#main_StringArray:
		#triggerInBackground_int:int:
		#triggerInLine_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.Events'.
! !
!StaticOMJTREvents class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTREvents class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTREvents class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTREvents class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTREvents class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticOMJTRNotifications guid: (GUID fromString: '{12548190-C422-4795-BABE-C8A21D6F2BC5}')!
StaticOMJTRNotifications comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org
'!
!StaticOMJTRNotifications categoriesForClass!Unclassified! !
!StaticOMJTRNotifications methodsFor!

errorNotifyTag_null
	"answer the result of calling the receiver's public static errorNotifyTag() Java method"

	^ self callObjectMethod: 'errorNotifyTag' signature: '()Ljava/lang/Object;'.
!

failingNotifyTag_null
	"answer the result of calling the receiver's public static failingNotifyTag() Java method"

	^ self callObjectMethod: 'failingNotifyTag' signature: '()Ljava/lang/Object;'.
!

new_null
	"answer the result of calling the receiver's public default Java constructor"

	^ self callConstructor.
!

notify0Tag_null
	"answer the result of calling the receiver's public static notify0Tag() Java method"

	^ self callObjectMethod: 'notify0Tag' signature: '()Ljava/lang/Object;'.
!

notify1Tag_null
	"answer the result of calling the receiver's public static notify1Tag() Java method"

	^ self callObjectMethod: 'notify1Tag' signature: '()Ljava/lang/Object;'.
!

notify2Tag_null
	"answer the result of calling the receiver's public static notify2Tag() Java method"

	^ self callObjectMethod: 'notify2Tag' signature: '()Ljava/lang/Object;'.
! !
!StaticOMJTRNotifications categoriesFor: #errorNotifyTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRNotifications categoriesFor: #failingNotifyTag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRNotifications categoriesFor: #new_null!**auto generated**!Java-constructors!Java-public!public! !
!StaticOMJTRNotifications categoriesFor: #notify0Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRNotifications categoriesFor: #notify1Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOMJTRNotifications categoriesFor: #notify2Tag_null!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticOMJTRNotifications class methodsFor!

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
		#errorNotifyTag_null
		#failingNotifyTag_null
		#notify0Tag_null
		#notify1Tag_null
		#notify2Tag_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.test.regression.Notifications'.
! !
!StaticOMJTRNotifications class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRNotifications class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRNotifications class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRNotifications class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOMJTRNotifications class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JNIPortCallbackEventsTest guid: (GUID fromString: '{21C8A15A-0F92-4DC0-B348-30777F3973EC}')!
JNIPortCallbackEventsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortCallbackEventsTest categoriesForClass!Unclassified! !
!JNIPortCallbackEventsTest methodsFor!

javaTestClassName

	^ 'Events'.!

onEventReceived: anOMJTRTestEventObject

	eventsReceived addLast: anOMJTRTestEventObject.

	"answer nil in case the test is using Requests rather than Notifications"
	^ nil.
!

setUp

	super setUp.
	classStatic := self javaTestClass.

	eventMethod := classStatic getEventMethod_null.
"	eventForwarder := eventMethod asynchronousEventForwarder: #onTestEvent:.	"
	eventForwarder := eventMethod eventForwarder: #onTestEvent:.

	eventSource := classStatic getEventSource_null.
	eventSource
		beCanonical;
		addListener_TestEventListener: eventForwarder.
!

tearDown

	eventSource removeListener_TestEventListener: eventForwarder.
	jvm callbackRegistry clearCallback: eventMethod.

	classStatic := eventSource := eventMethod := eventForwarder := nil.

	super tearDown.
!

testEventsSentInBackground

	eventsReceived := OrderedCollection new.

	eventSource when: #onTestEvent: send: #onEventReceived: to: self.
	classStatic triggerInBackground_int: 3 int: 1000.
	self pumpMessagesFor: 5000.
	eventSource removeEventsTriggeredFor: self.

	self should: [eventsReceived size = 3].
	self should: [eventsReceived allSatisfy: [:each | each getText_null asString = 'background']].

!

testEventsSentInline

	eventsReceived := OrderedCollection new.

	eventSource when: #onTestEvent: send: #onEventReceived: to: self.
	5 timesRepeat: [classStatic triggerInLine_null].
	eventSource removeEventsTriggeredFor: self.

	self should: [eventsReceived size = 5].
	self should: [eventsReceived allSatisfy: [:each | each getText_null asString = 'inline']].
! !
!JNIPortCallbackEventsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortCallbackEventsTest categoriesFor: #onEventReceived:!event handling!private! !
!JNIPortCallbackEventsTest categoriesFor: #setUp!public!running! !
!JNIPortCallbackEventsTest categoriesFor: #tearDown!public!running! !
!JNIPortCallbackEventsTest categoriesFor: #testEventsSentInBackground!public!unit tests! !
!JNIPortCallbackEventsTest categoriesFor: #testEventsSentInline!public!unit tests! !

JNIPortCallbackNotificationsTest guid: (GUID fromString: '{8A2BDEB3-A749-4378-A55C-75CB3CC9A0E6}')!
JNIPortCallbackNotificationsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortCallbackNotificationsTest categoriesForClass!Unclassified! !
!JNIPortCallbackNotificationsTest methodsFor!

handleErrorNotificationFor: aJavaObject

	Error signal: 'Test Exception thrown from Smalltalk'.!

handleFailingNotificationFor: aJavaObject

	(jvm findClass: 'java.lang.NullPointerException') signal: 'Test Exception thrown from Smalltalk'.!

handleNotificationFor: aJavaObject

	notificationsStatus addLast: (aJavaObject = instance).	"identity test on the Java object"
!

handleNotificationFor: aJavaObject with: anotherJavaObject

	| jliClass value |

	jliClass := jvm findClass: #'java.lang.Integer'.

	notificationsStatus addLast: (aJavaObject = instance).	"identity test on the Java object"
	notificationsStatus addLast: ((anotherJavaObject static = jliClass) and: [anotherJavaObject intValue_null = 1]).
!

handleNotificationFor: aJavaObject withArguments: anArray

	| jliClass jldClass |

	jliClass := jvm findClass: #'java.lang.Integer'.
	jldClass := jvm findClass: #'java.lang.Double'.

	notificationsStatus addLast: (aJavaObject = instance).	"identity test on the Java object"
	notificationsStatus addLast: (anArray size = 2).
	notificationsStatus addLast: ((anArray first static = jliClass) and: [anArray first intValue_null = 1]).
	notificationsStatus addLast: ((anArray second static = jldClass) and: [anArray second floatValue_null = 2.0]).
!

javaTestClassName

	^ 'Notifications'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.

	jvm callbackRegistry
		setCallback: classStatic notify0Tag_null handler: [:this :param | self handleNotificationFor: this];
		setCallback: classStatic notify1Tag_null handler: [:this :param | self handleNotificationFor: this with: param];
		setCallback: classStatic notify2Tag_null handler: [:this :params | self handleNotificationFor: this withArguments: params asArray];
		setCallback: classStatic errorNotifyTag_null handler: [:this :param | self handleErrorNotificationFor: this];
		setCallback: classStatic failingNotifyTag_null handler: [:this :param | self handleFailingNotificationFor: this].

	notificationsStatus := OrderedCollection new.!

tearDown

	jvm callbackRegistry
		clearCallback: classStatic notify0Tag_null;
		clearCallback: classStatic notify1Tag_null;
		clearCallback: classStatic notify2Tag_null;
		clearCallback: classStatic errorNotifyTag_null;
		clearCallback: classStatic failingNotifyTag_null.

	classStatic := instance := nil.
	super tearDown.
!

testNotificationInBackground

	instance notifyInBackground_int: 3 int: 1000.
	self pumpMessagesFor: 5000.

	self should: [notificationsStatus size = 3].
	self should: [notificationsStatus allSatisfy: [:each | each = true]].

!

testNotificationInLine

	instance notifyInLine_null.

	self should: [notificationsStatus size = 1].
	self should: [notificationsStatus allSatisfy: [:each | each = true]].

!

testNotificationThrowingJavaException

	"the thrown error should be absorbed silently"
	instance notifyAndThrowNPE_null.!

testNotificationWithError

	"the thrown error should be absorbed silently"
	instance notifyAndThrowOther_null.!

testNotificationWithNoParameters

	instance notify0_null.

	self should: [notificationsStatus size = 1].
	self should: [notificationsStatus allSatisfy: [:each | each = true]].

!

testNotificationWithOneParameter

	instance notify1_null.

	self should: [notificationsStatus size = 2].
	self should: [notificationsStatus allSatisfy: [:each | each = true]].
!

testNotificationWithTwoParameters

	instance notify2_null.

	self should: [notificationsStatus size = 4].
	self should: [notificationsStatus allSatisfy: [:each | each = true]].

! !
!JNIPortCallbackNotificationsTest categoriesFor: #handleErrorNotificationFor:!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #handleFailingNotificationFor:!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #handleNotificationFor:!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #handleNotificationFor:with:!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #handleNotificationFor:withArguments:!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortCallbackNotificationsTest categoriesFor: #setUp!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #tearDown!public!running! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationInBackground!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationInLine!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationThrowingJavaException!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationWithError!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationWithNoParameters!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationWithOneParameter!public!unit tests! !
!JNIPortCallbackNotificationsTest categoriesFor: #testNotificationWithTwoParameters!public!unit tests! !

JNIPortCallbackRequestsTest guid: (GUID fromString: '{8293BBE5-1323-4BB5-B8DE-CD833D70B7A3}')!
JNIPortCallbackRequestsTest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org'!
!JNIPortCallbackRequestsTest categoriesForClass!Unclassified! !
!JNIPortCallbackRequestsTest methodsFor!

handleCallbackFor: aJavaObject

	callbacksStatus addLast: (aJavaObject = instance).	"identity test on the Java object"

	"we*MUST* answer a Java object or nil"
	^ nil.!

handleCallbackFor: aJavaObject with: anotherJavaObject

	| jliClass value |

	jliClass := jvm findClass: #'java.lang.Integer'.

	callbacksStatus addLast: (aJavaObject = instance).	"identity test on the Java object"
	callbacksStatus addLast: ((anotherJavaObject static = jliClass) and: [anotherJavaObject intValue_null = 1]).

	^ jliClass new_int: 15.!

handleCallbackFor: aJavaObject withArguments: anArray

	| jliClass jldClass |

	jliClass := jvm findClass: #'java.lang.Integer'.
	jldClass := jvm findClass: #'java.lang.Double'.

	callbacksStatus addLast: (aJavaObject = instance).	"identity test on the Java object"
	callbacksStatus addLast: (anArray size = 2).
	callbacksStatus addLast: ((anArray first static = jliClass) and: [anArray first intValue_null = 1]).
	callbacksStatus addLast: ((anArray second static = jldClass) and: [anArray second floatValue_null = 2.0]).

	"we*MUST* answer a Java object or nil"
	^ nil.
!

handleErrorCallbackFor: aJavaObject

	Error signal: 'Test Exception thrown from Smalltalk'.!

handleFailingCallbackFor: aJavaObject

	(jvm findClass: 'java.lang.NullPointerException') signal: 'Test Exception thrown from Smalltalk'.!

javaTestClassName

	^ 'Callbacks'.!

setUp

	super setUp.
	classStatic := self javaTestClass.
	instance := classStatic new.

	jvm callbackRegistry
		setCallback: classStatic callback0Tag_null handler: [:this :param | self handleCallbackFor: this];
		setCallback: classStatic callback1Tag_null handler: [:this :param | self handleCallbackFor: this with: param];
		setCallback: classStatic callback2Tag_null handler: [:this :params | self handleCallbackFor: this withArguments: params asArray];
		setCallback: classStatic errorCallbackTag_null handler: [:this :param | self handleErrorCallbackFor: this];
		setCallback: classStatic failingCallbackTag_null handler: [:this :param | self handleFailingCallbackFor: this].

	callbacksStatus := OrderedCollection new.!

tearDown

	jvm callbackRegistry
		clearCallback: classStatic callback0Tag_null;
		clearCallback: classStatic callback1Tag_null;
		clearCallback: classStatic callback2Tag_null;
		clearCallback: classStatic errorCallbackTag_null;
		clearCallback: classStatic failingCallbackTag_null.

	classStatic := instance := nil.
	super tearDown.
!

testCallbackInBackground

	instance callbackInBackground_int: 3 int: 1000.
	self pumpMessagesFor: 5000.

	self should: [callbacksStatus size = 3].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].

!

testCallbackInLine

	instance callbackInLine_null.

	self should: [callbacksStatus size = 1].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].

!

testCallbackThrowingJavaException

	self should: [instance callbackAndThrowNPE_null].!

testCallbackWithError

	self should: [instance callbackAndThrowOther_null].!

testCallbackWithNoParameters

	instance callback0_null.

	self should: [callbacksStatus size = 1].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].

!

testCallbackWithOneParameter

	instance callback1_null.

	self should: [callbacksStatus size = 2].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].
!

testCallbackWithOneParameterAndReturn

	| answer |

	answer := instance callbackAndReturn_int: 1.

	self should: [callbacksStatus size = 2].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].
	self should: [answer = 15].

!

testCallbackWithTwoParameters

	instance callback2_null.

	self should: [callbacksStatus size = 4].
	self should: [callbacksStatus allSatisfy: [:each | each = true]].

! !
!JNIPortCallbackRequestsTest categoriesFor: #handleCallbackFor:!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #handleCallbackFor:with:!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #handleCallbackFor:withArguments:!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #handleErrorCallbackFor:!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #handleFailingCallbackFor:!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #javaTestClassName!constants!public! !
!JNIPortCallbackRequestsTest categoriesFor: #setUp!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #tearDown!public!running! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackInBackground!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackInLine!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackThrowingJavaException!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackWithError!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackWithNoParameters!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackWithOneParameter!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackWithOneParameterAndReturn!public!unit tests! !
!JNIPortCallbackRequestsTest categoriesFor: #testCallbackWithTwoParameters!public!unit tests! !

"Binary Globals"!

"Resources"!

