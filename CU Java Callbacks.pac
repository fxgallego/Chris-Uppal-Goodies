| package |
package := Package name: 'CU Java Callbacks'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

The JNIPort callback facility.  This package has to be loaded if you want to use callbacks from Java to Smalltalk.

If you are using this package then the corresponding Java classes  (shipped in the "Extras\JNIPort.jar" file) must be on the Java classpath somewhere.  One way to ensure this is to add the file to the #classpath in the JVM settings, alternatively you can add a local classpath entry called #JNIPort which includes the Jar file in its path.

The Java source is in "Extras\JNIPort.zip".

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris'.

package basicPackageVersion: '1.10'.


package classNames
	add: #JavaCallbackHandler;
	add: #JavaCallbackRegistry;
	add: #JavaEventTrigger;
	add: #JavaMessageSend;
	add: #JVMWithCallbacks;
	add: #OrgMetagnosticJniportAbstractDolphinRequest;
	add: #OrgMetagnosticJniportDolphinNotification;
	add: #OrgMetagnosticJniportDolphinRequest;
	add: #StaticOrgMetagnosticJniportDolphinNotifierThread;
	add: #StaticOrgMetagnosticJniportDolphinRequestQueue;
	yourself.

package methodNames
	add: #JavaLangReflectMethod -> #asynchronousEventForwarder:;
	add: #JavaLangReflectMethod -> #eventForwarder:;
	add: #JavaStatic -> #implementNative:by:;
	add: #JavaStatic -> #makeCallbackInvoking:;
	add: #JVMSettings -> #supportsCallbacks;
	add: #JVMSettings -> #supportsCallbacks:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'CU Java Base';
	add: 'CU JNI';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #JavaCallbackHandler
	instanceVariableNames: 'symbol'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #JavaCallbackRegistry
	instanceVariableNames: 'jvm callbacks connected mutex notifierClass queueClass trace'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaCallbackHandler subclass: #JavaEventTrigger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaCallbackHandler subclass: #JavaMessageSend
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JavaLangObject subclass: #OrgMetagnosticJniportAbstractDolphinRequest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OrgMetagnosticJniportAbstractDolphinRequest subclass: #OrgMetagnosticJniportDolphinNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OrgMetagnosticJniportAbstractDolphinRequest subclass: #OrgMetagnosticJniportDolphinRequest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangObject subclass: #StaticOrgMetagnosticJniportDolphinRequestQueue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StaticJavaLangThread subclass: #StaticOrgMetagnosticJniportDolphinNotifierThread
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JVM subclass: #JVMWithCallbacks
	instanceVariableNames: 'jniEnvStack callbackRegistry'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JavaLangReflectMethod methodsFor!

asynchronousEventForwarder: aSymbol
	"answer an instance of a temporary Java class that will implement this
	method, assumed to be a notification event, by triggering aSymbol off
	the  (Smalltalk proxy for the) source of the event. Tthe handler will be
	asynchronous, i.e. there is no guarantee that the notification will have
	been handled in Smalltalk before the event method return in Java.
	Note that the source of the event had better have been made canonical
	or else the event will be triggered off an ephemeral JavaInstance, and you
	won't be able to listen for it.
	Note also that you can't have more than one asynchronousEventForwarder
	or eventForwarder active for the same Java method at the same time"

	| proxy args |

	"we can only use java.lang.reflect.proxy on interface classes"
	self assert: [self declaredIn isInterface].

	"we require that the method takes 1 argument, and that that is a subtype of
	java.util.EventObject"
	self assert: [self parameterTypes size = 1].
	self assert: [self parameterTypes first isDerivedFrom: (self jvm findClass: #'java.util.EventObject')].

	"we requre that the event's return type is void"
	self assert: [self type isVoid].

	"create the proxy"
	args := (JNIValueArray new: 2)
			objectAt: 1 put: self;
			booleanAt: 2 put: true;
			yourself.
	proxy := (self jvm findClass: #'org.metagnostic.jniport.EventForwarder')
			callObjectMethod: 'forwarderFor'
			signature: '(Ljava/lang/reflect/Method;Z)Ljava/lang/Object;'
			withArguments: args.

	"ensure that the callback is registered"
	self jvm callbackRegistry
			setCallback: self
			triggerWithOneArgument: aSymbol.

	^ proxy.!

eventForwarder: aSymbol
	"answer an instance of a temporary Java class that will implement this
	method, assumed to be a notification event, by triggering aSymbol off
	the (Smalltalk proxy for the) source of the event.
	Note that the source of the event had better have been made canonical
	or else the event will be triggered off an ephemeral JavaInstance, and you
	won't be able to listen for it.
	Note also that you can't have more than one asynchronousEventForwarder
	or eventForwarder active for the same Java method at the same time"

	| proxy args |

	"we can only use java.lang.reflect.proxy on interface classes"
	self assert: [self declaredIn isInterface].

	"we require that the method takes 1 argument, and that that is a subtype of
	java.util.EventObject"
	self assert: [self parameterTypes size = 1].
	self assert: [self parameterTypes first isDerivedFrom: (self jvm findClass: #'java.util.EventObject')].

	"create the proxy"
	args := (JNIValueArray new: 2)
			objectAt: 1 put: self;
			booleanAt: 2 put: false;
			yourself.
	proxy := (self jvm findClass: #'org.metagnostic.jniport.EventForwarder')
			callObjectMethod: 'forwarderFor'
			signature: '(Ljava/lang/reflect/Method;Z)Ljava/lang/Object;'
			withArguments: args.

	"ensure that the callback is registered"
	self jvm callbackRegistry
			setCallback: self
			triggerWithOneArgument: aSymbol.

	^ proxy.! !
!JavaLangReflectMethod categoriesFor: #asynchronousEventForwarder:!Java callbacks!public! !
!JavaLangReflectMethod categoriesFor: #eventForwarder:!Java callbacks!public! !

!JavaStatic methodsFor!

implementNative: aJavaLangReflectMethod by: a0Block
	"register the <niladicValuable>, a0Block as the implementation of the given
	native method.  Note that we are unable to handle native methods that take
	any parameters or return any value (and so we cannot handle non-static
	methods either)"

	|  jniMethod itsName itsSignature itsCallback |

	"this could screw up quite easily, so..."
	self assert: [self jvm supportsCallbacks].
	self assert: [aJavaLangReflectMethod declaredIn: self].
	self assert: [aJavaLangReflectMethod isNative].
	self assert: [aJavaLangReflectMethod isStatic].
	self assert: [aJavaLangReflectMethod type isVoid].
	self assert: [aJavaLangReflectMethod argumentCount = 0].

	"create the JNIMethod struct; note that we hold onto references to the name, etc,
	so they are protected against GC"
	itsName := aJavaLangReflectMethod name asJavaQuasiUTF8EncodedString.
	itsSignature := "aJavaLangReflectMethod jniSignature" '()V'.
	itsCallback := self makeCallbackInvoking: a0Block.
	jniMethod := (JNINativeMethod new)
				name: itsName;
				signature: itsSignature;
				callback: itsCallback;
				yourself.

	"technically we should pass an array of JNINativeMethods, but we only need one"
	self jniEnv
		RegisterNatives_class: self jniObject
		methods: jniMethod
		nMethods: 1
		onException: [:ex | jvm throwJavaException: ex].

	"maybe, with luck, we haven't now crashed or destabilised Dolphin..."

	"get our owning JVM to hold onto the external callback to prevent it getting GCed"
	self jvm rememberExternalCallback: itsCallback.!

makeCallbackInvoking: a0Block
	"private -- make an external callback that will result in us invoking
	aBlock with no arguments"

	"IMPORTANT: we are obliged to tell the JVM that it has to use a new JNIEnv, but we cannot
	tell it to use the one supplied, since that may have been produced by the JVM running on a
	different OS thread, and transferred over to this one by the Dolphin VM.
	For that reason, the jniObject ref supplied with the callback *must* be ignored, since it can
	only be accessed from the thread that *originated* the callback (its local to the supplied
	JNIEnv).
	Thus, in turn, we can only supply native implementations of static void Java methods with
	no parameters"
	 
	^ ExternalCallback
		block: [:jniEnv :jniObject | self jvm asCallbackDo: a0Block. jniObject:= nil. nil]
		descriptor: (ExternalDescriptor fromString: 'stdcall: void JNIEnv* JNIObject').! !
!JavaStatic categoriesFor: #implementNative:by:!Java native methods!public! !
!JavaStatic categoriesFor: #makeCallbackInvoking:!Java native methods!private! !

!JVMSettings methodsFor!

supportsCallbacks
	"answer whether these settings are such that a JVM configured from
	them would support callbacks.
	This intended just as a convenience method"

	^ self jniPortSettings jvmClass = JVMWithCallbacks.!

supportsCallbacks: aBool
	"set whether these settings are such that a JVM configured from
	them would support callbacks.
	This intended just as a convenience method"

	^ self jniPortSettings jvmClass: (aBool ifTrue: [JVMWithCallbacks] ifFalse: [JVMWithoutCallbacks]).! !
!JVMSettings categoriesFor: #supportsCallbacks!accessing!public! !
!JVMSettings categoriesFor: #supportsCallbacks:!accessing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

JavaCallbackHandler guid: (GUID fromString: '{47F20006-D879-4BD2-B0A4-C3F99C171094}')!
JavaCallbackHandler comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

A callback handler is an object that understands the #value:value: message.  They can be used as the "handlers" set in the callback registry.

The Java class org.metagnostic.jniport.AbstractDolphinRequest defines a callback request as having (at least) an "originator" (often the object that sent the callback), and a "parameter" (an unconstained Java object).  This abstract class is a framework for handlers that use such conventions.'!
!JavaCallbackHandler categoriesForClass!Unclassified! !
!JavaCallbackHandler methodsFor!

displayOn: aStream
	"append a user-friendly description of ourself to aStream"

	"use #print because we want it to look like a selector"
	aStream print: symbol.!

executeOn: anObject withArguments: anArray
	"private -- implement the heart of our function"

	self subclassResponsibility.!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		display: self class;
		nextPutAll: ' selector: ';
		display: self.!

symbol
	"answer the Symbol we 'use'"

	^ symbol.!

symbol: aSymbol
	"private -- set the symbol that we hold.  The use of the symbol is determined by the subclass"

	symbol :=  aSymbol.!

value: aJavaObject value: aJavaArrayOrNil
	"implement this part of the <dyadicValueable> protocol by interpreting
	our symbol (in a way defined by the subclass) with aJavaObject as its
	targer and with arguments from aJavaArrayOrNil.
	Answers the result"

	| params |

	params := aJavaArrayOrNil
			ifNil: [#()]
			ifNotNil: [:it | it asArray].

	^ self
		executeOn: aJavaObject
		withArguments: params.! !
!JavaCallbackHandler categoriesFor: #displayOn:!displaying!public! !
!JavaCallbackHandler categoriesFor: #executeOn:withArguments:!operations!private! !
!JavaCallbackHandler categoriesFor: #printOn:!printing!public! !
!JavaCallbackHandler categoriesFor: #symbol!accessing!public! !
!JavaCallbackHandler categoriesFor: #symbol:!initializing!private! !
!JavaCallbackHandler categoriesFor: #value:value:!evaluating!public! !

!JavaCallbackHandler class methodsFor!

symbol: aSymbol
	"answer a new instance that holds the given symbol.  What it does with it is
	determined by the subclass"

	^ (self new)
		symbol: aSymbol;
		yourself. ! !
!JavaCallbackHandler class categoriesFor: #symbol:!instance creation!public! !

JavaCallbackRegistry guid: (GUID fromString: '{BFA847EF-E19C-49CB-9831-6EA1CBADD3F8}')!
JavaCallbackRegistry comment: 'Copyright © Chris Uppal, 2002 - 2005.
chris.uppal@metagnostic.org

The callback registry holds a mapping from ''tag'' objects (which are unique Java objects used to identity each callback) to ''handlers''.

It is designed to work with the Java helper classes for callbacks.  The idea is that since we can''t easily handle Java callbacks *directly* (because of thread problems), we can build on the very restricted subset of real callbacks that do work, to implement a message queue holding requests and notifications.  When the Java queue notifies us that it has some content (this 0-bit notification is the only safe way to cross the boundary) we can then use the normal facilities of JNIPort to pull the messages our of the queue and service them.

Handlers are objects that implement the #value:#value: message.  The first arguement is the ''originator'' of the message, the second argument is the ''parameter'' to the message.  The returned value *must* be a JavaObject or nil, it will be returned to Java as the result (not necessarily used) of the request.  If the handler throws an exception (Java or Smalltalk) then that will be converted to a Java exception and rethrown (if the caller is still waiting for a response) in the Java space.'!
!JavaCallbackRegistry categoriesForClass!Unclassified! !
!JavaCallbackRegistry methodsFor!

averageProbesPerElement
	"answer the <Float> average number of probes to find an item already in the callback table"

	^ mutex critical: [callbacks averageProbesPerElement].!

beConnected
	"private -- ensure that we are connected to the org.metagnostic.jniport.DolphinRequest system"

	mutex critical: [connected ifTrue: [^ self]].

	"this stuff can be slow, so we do it outside the mutex; it may
	end up getting done twice, but it's idempotent so it doesn't matter.
	Also note that if this fails with a class-not-found exception then we
	leave ourselves in a state where the next attempt to connect will
	cause another attempt to find the class"
	notifierClass := self findNotifierClass.
	queueClass := self findRequestQueueClass.

	"now try to connect (may be a nullop)"
	mutex critical: [connected ifFalse: [self connectToNotifierClass]].
!

clearCallback: aJavaTagObject
	"remove the registered callback for aJavaTagObject.
	Any attempt to invoke this callback from Java space will henceforth trigger a
	RequestNotHandledException in the caller"

	mutex critical:
		[self isDead ifTrue: [^ self].
		callbacks removeKey: aJavaTagObject ifAbsent: []].!

connectToNotifierClass
	"private -- establish our connection to the notifier thread.
	NB: is only called under the protection of our mutex"

	connected := true.

	"tell the notifier class which thread we are on"
	notifierClass setDolphinNativeThread.

	"set up the basic callback"
	notifierClass implementNotifierMethodBy: self makeHandlerBlock.!

dispatch: aRequest to: a2Block
	"private -- handle the given org.metagnostic.jniport.DolphinRequest object
	by passing its originator and parameter to a2Block.  Note that trapping
	exceptions is implemented in our caller (#safeDispatch:to:)"

	| answer |

	self trace: aRequest with: 'dispatch -- enter'.

	answer := a2Block
			value: aRequest originator
			value: aRequest parameter.

	aRequest requiresAcknowledgement ifTrue:
		["this assert will cause a RequestNotHandledException to be triggered in
		the Java caller.
		Possibly someday we could coece integers etc, automatically"
		self assert: [answer isNil or: [answer isKindOf: JavaObject]].
		self trace: answer with: 'dispatch -- answer'.
		aRequest notifyAnswer: answer].

	self trace: aRequest with: 'dispatch -- exit'.!

findNotifierClass
	"private -- find and answer the DolphinNotifierThread class static"

	^ jvm findJNIPortClass: self class notifierClassName.!

findRequestQueueClass
	"private -- find and answr the class static for the DolphinRequestQueue class"

	^ jvm findJNIPortClass: self class requestQueueClassName.
!

handleOutstandingRequests
	"check the queue of requests in Java space and handle any that are
	outstanding.
	This can be called directly, but is normally called when the notifier Java
	thread determines that it is necessary"

	| requests |

	self trace: 'handleOutstandingRequests -- enter'.

	[self isConnected and: [(requests := queueClass nextRequests: 100) notNil]]
		whileTrue: [self handleRequests: requests].

	self trace: 'handleOutstandingRequests -- exit'.
!

handleRequest: aRequest
	"private -- handle the given org.metagnostic.jniport.DolphinRequest object"

	| handler |

	self trace: aRequest with: 'handleRequest -- enter'.

	handler := mutex critical: [callbacks at: aRequest tag ifAbsent: [nil]].
	handler isNil
		ifTrue: [aRequest notifyIgnored]
		ifFalse: [self safeDispatch: aRequest to: handler].

	self trace: aRequest with: 'handleRequest -- exit'.
!

handleRequests: aJavaArray
	"private -- handle the given org.metagnostic.jniport.DolphinRequest object"

	self trace: 'handleRequests -- enter'.

	aJavaArray do: [:each | self handleRequest: each].

	aJavaArray free.	"it seems that these don't get scavenged very efficiently, but we know we can
				dispose of this array manually here"

	self trace: 'handleRequests -- exit'.!

initialize
	"private -- establish a coherent initial state"

	"note that this uses #= and #hash for comparison, i.e. it is comparing Java objects
	by identity (two JavaInstances are #= iff they refer to the same Java object)"
	callbacks := LookupTable new.

#CUtodo.  "can this Mutex cause deadlocks with the main shared mutex (in the JVM) ?"
	mutex := Mutex new.
	connected := false.!

isConnected
	"answer whether we are connected to the runtime system"

	^ self isLive and: [connected].!

isDead
	"answer whether we are in doornail mode"

	^ jvm isNil or: [jvm isDead].!

isLive
	"answer whether we are still alive"

	^ jvm notNil and: [jvm isLive].!

jvm: aJVM
	"private -- set the JVM that we will organise callbacks for"

	jvm := aJVM.
!

makeHandlerBlock
	"private -- answer the <niladicValuable> that we will install
	as the hook for notifications from Java"

	^ [self handleOutstandingRequests].!

maxQueueLength
	"answer how long the queue of requests has ever been"

	^ self isConnected
		ifTrue: [queueClass requestQueueMax_null]
		ifFalse: [-1].!

printOn: aStream
	"write a developer oriented representation of the receiver to aStream"

	aStream nextPutAll: 'a '.
	self isDead
		ifTrue: [aStream nextPutAll: 'dead ']
		ifFalse: [connected ifFalse: [aStream nextPutAll: 'disconnected ']].
	aStream nextPutAll: self class name.
!

queueLength
	"answer how long the queue of requests is"

	^ self isConnected
		ifTrue: [queueClass requestQueueLength_null]
		ifFalse: [-1].!

registeredCallbackCount
	"answer how many callbacks are registered"

	^ self isConnected
		ifTrue: [callbacks size]
		ifFalse: [-1].!

registeredCallbacks
	"answer the dictionary mapping JavaObject 'tags' to handler blocks"

	^ callbacks.!

requestsCompleted
	"answer how many requests have been completed"

	^ self isConnected
		ifTrue: [queueClass requestsCompleted_null]
		ifFalse: [-1].!

safeDispatch: aRequest to: a2Block
	"private -- invoke the handler for aRequest in an environment that will ensure that
	some sort of response is always sent back to the requester.  This is *vital* since otherwise
	we will almost certainly deadlock ourselves"

	[
		[self dispatch: aRequest to: a2Block]
			on: JavaException
			do: [:err | aRequest notifyError: err]
			on: Exception
			do: [:err | aRequest notifyIgnored: 'The Dolphin handler threw a non-Java exception (', err printString , ')']
	] ifCurtailed: [aRequest notifyIgnored: 'The Dolphin handler did not complete normally'].!

setCallback: aJavaTagObject handler: a2Block
	"add/change the registered callback for aJavaTagObject.  Here, aJavaObject is
	the 'tag' supplied with an org.metagnostic.jniport.DolphinRequest object.
	The <diadicValuable> will be evaluated with the request's 'originator' as its first argument,
	and the 'parameter' (a single Java object that might be an Array, or nil) as its second.
	If any exception is thrown from a2Block, then (provided it is a Java exception) that
	will be passed back to the caller in Java space.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	"ensure we are connected"
	self beConnected.

	mutex critical:
		[self isDead ifTrue: [^ self].
		callbacks at: aJavaTagObject put: a2Block].!

setCallback: aJavaTagObject performWithArguments: aSelector
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSelector will be sent to the 'originator' object
	of the request with the parameter object interpretted as an array of arguments
	to the message send.
	This may not be much use unless a 'canonical' has been 	set, since it'll send aSelector
	to the ephemeral JavaInstance created to wrap the 'originator' field.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: (JavaMessageSend symbol: aSelector).

!

setCallback: aJavaTagObject performWithNoArguments: aSelector
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSelector will be sent	to the 'originator' object
	of the request ignoring the parameter object.
	This may not be much use unless a 'canonical' has been 	set, since it'll send aSelector
	to the ephemeral JavaInstance created to wrap the 'originator' field.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: [:originator :parameter | originator perform: aSelector].

!

setCallback: aJavaTagObject performWithOneArgument: aSelector
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSelector will be sent	to the 'originator' object
	of the request with the parameter object interpretted as the single argument
	to the message send.
	This may not be much use unless a 'canonical' has been 	set, since it'll send aSelector
	to the ephemeral JavaInstance created to wrap the 'originator' field.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: [:originator :parameter | originator perform: aSelector with: parameter].

!

setCallback: aJavaTagObject triggerWithArguments: aSymbol
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSymbol will be triggered off the 'originator' object of the
	request, and interpreting the parameter as an array of arguments to the event.
	Note that this is no use at all unless a 'canonical' has been set, since it'd
	just trigger off the ephemeral JavaObject created to wrap the 'originator' field, and
	(by definition) no one will be listening to that object.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: (JavaEventTrigger symbol: aSymbol).

!

setCallback: aJavaTagObject triggerWithNoArguments: aSymbol
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSymbol will be triggered off the 'originator' object of the
	request ignoring the parameter.
	Note that this is no use at all unless a 'canonical' has been set, since it'd
	just trigger off the ephemeral JavaObject created to wrap the 'originator' field, and
	(by definition) no one will be listening to that object.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: [:originator :parameter | originator trigger: aSymbol].

!

setCallback: aJavaTagObject triggerWithOneArgument: aSymbol
	"add/change the registered callback for aJavaTagObject.  This convenience
	operator just arranges that aSymbol will be triggered off the 'originator' object of the
	request, with the parameter interpreted as the single argument to the event.
	Note that this is no use at all unless a 'canonical' has been set, since it'd
	just trigger off the ephemeral JavaObject created to wrap the 'originator' field, and
	(by definition) no one will be listening to that object.
	Note that we hold onto a *strong* reference to aJavaTagObject"

	self setCallback: aJavaTagObject handler: [:originator :parameter | originator trigger: aSymbol with: parameter].

!

shutdown
	"private -- discard any connection to the JVM and unload any callbacks"

	mutex critical:
		[jvm := nil.
		callbacks := nil.
		queueClass := notifierClass := nil.
		connected := false].
!

shutdownJava
	"private -- close down any runnng code in the Java space -- this is called just
	before our JVM closes down, and it may help to reduce problems caused by
	pending callbacks"

	mutex critical:
		[self isConnected ifTrue: [queueClass shutdown].
		connected := false].!

stopTracing

	self traceTo: nil.!

tableLoad
	"answer the <Float> percentage load on the callbacks table"

	^ mutex critical: [callbacks size * 100.0 / callbacks basicSize].!

tableSize
	"answer how many callbacks are registered"

	^ mutex critical: [callbacks size].!

trace: anObject

	trace notNil ifTrue:
		[trace
			display: jvm callbackDepth;
			nextPutAll: '[';
			display: Processor activeProcess callbackDepth;
			space;
			display: Processor activeProcess;
			nextPutAll: '] ';
			display: anObject;
			cr].!

trace: anObject with: aString

	trace notNil ifTrue:
		[trace
			display: jvm callbackDepth;
			nextPutAll: '[';
			display: Processor activeProcess callbackDepth;
			space;
			display: Processor activeProcess;
			nextPutAll: '] ';
			display: aString;
			nextPutAll: ': ';
			display: anObject;
			cr].!

traceTo: aWriteStream

	trace := aWriteStream.!

traceToDebug

	self traceTo: (Smalltalk at: #Trace).!

traceToTranscript

	self traceTo: (Smalltalk at: #Transcript).! !
!JavaCallbackRegistry categoriesFor: #averageProbesPerElement!measuring!public! !
!JavaCallbackRegistry categoriesFor: #beConnected!initializing!private! !
!JavaCallbackRegistry categoriesFor: #clearCallback:!operations!public! !
!JavaCallbackRegistry categoriesFor: #connectToNotifierClass!initializing!private! !
!JavaCallbackRegistry categoriesFor: #dispatch:to:!operations!private! !
!JavaCallbackRegistry categoriesFor: #findNotifierClass!helpers!private! !
!JavaCallbackRegistry categoriesFor: #findRequestQueueClass!helpers!private! !
!JavaCallbackRegistry categoriesFor: #handleOutstandingRequests!operations!public! !
!JavaCallbackRegistry categoriesFor: #handleRequest:!operations!private! !
!JavaCallbackRegistry categoriesFor: #handleRequests:!operations!private! !
!JavaCallbackRegistry categoriesFor: #initialize!initializing!private! !
!JavaCallbackRegistry categoriesFor: #isConnected!public!testing! !
!JavaCallbackRegistry categoriesFor: #isDead!public!testing! !
!JavaCallbackRegistry categoriesFor: #isLive!public!testing! !
!JavaCallbackRegistry categoriesFor: #jvm:!event handling!initializing!private! !
!JavaCallbackRegistry categoriesFor: #makeHandlerBlock!initializing!private! !
!JavaCallbackRegistry categoriesFor: #maxQueueLength!accessing!public! !
!JavaCallbackRegistry categoriesFor: #printOn:!printing!public! !
!JavaCallbackRegistry categoriesFor: #queueLength!accessing!public! !
!JavaCallbackRegistry categoriesFor: #registeredCallbackCount!accessing!public! !
!JavaCallbackRegistry categoriesFor: #registeredCallbacks!accessing!public! !
!JavaCallbackRegistry categoriesFor: #requestsCompleted!accessing!public! !
!JavaCallbackRegistry categoriesFor: #safeDispatch:to:!operations!private! !
!JavaCallbackRegistry categoriesFor: #setCallback:handler:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:performWithArguments:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:performWithNoArguments:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:performWithOneArgument:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:triggerWithArguments:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:triggerWithNoArguments:!operations!public! !
!JavaCallbackRegistry categoriesFor: #setCallback:triggerWithOneArgument:!operations!public! !
!JavaCallbackRegistry categoriesFor: #shutdown!event handling!initializing!private! !
!JavaCallbackRegistry categoriesFor: #shutdownJava!initializing!private! !
!JavaCallbackRegistry categoriesFor: #stopTracing!public!tracing! !
!JavaCallbackRegistry categoriesFor: #tableLoad!measuring!public! !
!JavaCallbackRegistry categoriesFor: #tableSize!accessing!measuring!public! !
!JavaCallbackRegistry categoriesFor: #trace:!public!tracing! !
!JavaCallbackRegistry categoriesFor: #trace:with:!public!tracing! !
!JavaCallbackRegistry categoriesFor: #traceTo:!public!tracing! !
!JavaCallbackRegistry categoriesFor: #traceToDebug!public!tracing! !
!JavaCallbackRegistry categoriesFor: #traceToTranscript!public!tracing! !

!JavaCallbackRegistry class methodsFor!

new
	"private -- create a new instance unconnected to any JVM"

	^ (super new)
		initialize;
		yourself.!

notifierClassName
	"answer the (Java) class of the method that instances 'hook' to in
	order to handle callback requests"

	^ #'org.metagnostic.jniport.DolphinNotifierThread'.!

requestQueueClassName
	"answer the (Java) class of the callback request queues"

	^ #'org.metagnostic.jniport.DolphinRequestQueue'.!

withJvm: aJVM
	"private -- create a new instance connected to the given JVM"

	^ (self new)
		jvm: aJVM;
		yourself.! !
!JavaCallbackRegistry class categoriesFor: #new!instance creation!private! !
!JavaCallbackRegistry class categoriesFor: #notifierClassName!constants!public! !
!JavaCallbackRegistry class categoriesFor: #requestQueueClassName!constants!public! !
!JavaCallbackRegistry class categoriesFor: #withJvm:!instance creation!private! !

JavaEventTrigger guid: (GUID fromString: '{EDAF69D5-0BBD-4275-8E40-9E1CE9BF3351}')!
JavaEventTrigger comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

This class is designed to help with writing callback handlers for the case where an event is to be triggered off (the proxy for) the orgininator and  the parameter is a (Java) array of arguments to the event.'!
!JavaEventTrigger categoriesForClass!Unclassified! !
!JavaEventTrigger methodsFor!

executeOn: anObject withArguments: anArray
	"private -- implement the heart of our function"

	anObject trigger: symbol withArguments: anArray.

	"we never answer anything"
	^ nil.! !
!JavaEventTrigger categoriesFor: #executeOn:withArguments:!operations!private! !

JavaMessageSend guid: (GUID fromString: '{F9A94146-A0F1-444B-8D9A-086841944D5B}')!
JavaMessageSend comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

This class is designed to help with writing callback handlers for the case where the (proxy for the) originator is to be the target of a message and  the parameter is a (Java) array of parameters.'!
!JavaMessageSend categoriesForClass!Unclassified! !
!JavaMessageSend methodsFor!

executeOn: anObject withArguments: anArray
	"private -- implement the heart of our function"

	^ anObject perform: symbol withArguments: anArray.! !
!JavaMessageSend categoriesFor: #executeOn:withArguments:!operations!private! !

OrgMetagnosticJniportAbstractDolphinRequest guid: (GUID fromString: '{9B46ADBD-5856-4938-BB97-B9325A4DE492}')!
OrgMetagnosticJniportAbstractDolphinRequest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class org.metagnostic.jniport.AbstractDolphinRequest'!
!OrgMetagnosticJniportAbstractDolphinRequest categoriesForClass!Unclassified! !
!OrgMetagnosticJniportAbstractDolphinRequest methodsFor!

enqueue_null
	"invoke the receiver's public enqueue() Java method"

	self callVoidMethod: 'enqueue'.
!

getOriginator_null
	"answer the result of calling the receiver's public getOriginator() Java method"

	^ self callObjectMethod: 'getOriginator' signature: '()Ljava/lang/Object;'.
!

getParameter_null
	"answer the result of calling the receiver's public getParameter() Java method"

	^ self callObjectMethod: 'getParameter' signature: '()Ljava/lang/Object;'.
!

getTag_null
	"answer the result of calling the receiver's public getTag() Java method"

	^ self callObjectMethod: 'getTag' signature: '()Ljava/lang/Object;'.
!

isNotificationOnly_null
	"answer the result of calling the receiver's public isNotificationOnly() Java method"

	^ self callBooleanMethod: 'isNotificationOnly'.
!

isStarted_null
	"answer the result of calling the receiver's public synchronized isStarted() Java method"

	^ self callBooleanMethod: 'isStarted'.
!

notifyAnswer: aJavaObject
	"tell the request, and any waiting (Java) threads that an answer is now
	available"

	self
		notifyCompleted_Object: aJavaObject
		Throwable: nil.!

notifyCompleted_Object: anObject1 Throwable: aThrowable1
	"invoke the receiver's  notifyCompleted(java.lang.Object, java.lang.Throwable) Java method"

	| args |

	args := (JNIValueArray new: 2)
			objectAt: 1 put: anObject1;
			objectAt: 2 put: aThrowable1;
			yourself.

	self callVoidMethod: 'notifyCompleted' signature: '(Ljava/lang/Object;Ljava/lang/Throwable;)V' withArguments: args.
!

notifyError: aJavaException
	"tell the request, and any waiting (Java) threads that an error occured"

	self
		notifyCompleted_Object: nil
		Throwable: aJavaException tag.!

notifyIgnored
	"tell this request, and any waiting (Java) threads that it has not been handled
	because there is no registered handler for its tag"

	self notifyIgnored_null.!

notifyIgnored: aString
	"tell this request, and anywaiting (Java) threads that it has not been handled
	because there is no registered handler for it's tag"

	self notifyIgnored_String: aString.!

notifyIgnored_null
	"invoke the receiver's  notifyIgnored() Java method"

	self callVoidMethod: 'notifyIgnored'.
!

notifyIgnored_String: aString1
	"invoke the receiver's  notifyIgnored(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'notifyIgnored' signature: '(Ljava/lang/String;)V' withArguments: args.
!

originator
	"answer the originator of this request"

	^ self getOriginator_null.!

parameter
	"answer the parameter object of this request"

	^ self getParameter_null.!

requiresAcknowledgement
	"answer whether this request requres an answer"

	^ self isNotificationOnly_null not.!

tag
	"answer the identifying tag object of this request"

	^ self getTag_null.!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #enqueue_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #getOriginator_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #getParameter_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #getTag_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #isNotificationOnly_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #isStarted_null!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyAnswer:!notifying!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyCompleted_Object:Throwable:!**auto generated**!Java-methods!private! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyError:!notifying!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyIgnored!notifying!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyIgnored:!notifying!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyIgnored_null!**auto generated**!Java-methods!private! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #notifyIgnored_String:!**auto generated**!Java-methods!private! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #originator!accessing!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #parameter!accessing!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #requiresAcknowledgement!accessing!public!testing! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #tag!accessing!public! !
!OrgMetagnosticJniportAbstractDolphinRequest categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !

!OrgMetagnosticJniportAbstractDolphinRequest class methodsFor!

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
		#enqueue_null
		#getOriginator_null
		#getParameter_null
		#getTag_null
		#isNotificationOnly_null
		#isStarted_null
		#notifyCompleted_Object:Throwable:
		#notifyIgnored_null
		#notifyIgnored_String:
		#toString_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.AbstractDolphinRequest'.
! !
!OrgMetagnosticJniportAbstractDolphinRequest class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportAbstractDolphinRequest class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportAbstractDolphinRequest class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportAbstractDolphinRequest class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportAbstractDolphinRequest class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

OrgMetagnosticJniportDolphinNotification guid: (GUID fromString: '{073DFFFE-2957-4D7D-A55F-16EA3E9886A3}')!
OrgMetagnosticJniportDolphinNotification comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class org.metagnostic.jniport.DolphinNotification'!
!OrgMetagnosticJniportDolphinNotification categoriesForClass!Unclassified! !
!OrgMetagnosticJniportDolphinNotification methodsFor!

requiresAcknowledgement
	"answer whether this request requres an answer"

	"overridden 'cos we already know the answer"
	^ false.
!

send_null
	"invoke the receiver's public send() Java method"

	self callVoidMethod: 'send'.
! !
!OrgMetagnosticJniportDolphinNotification categoriesFor: #requiresAcknowledgement!accessing!public!testing! !
!OrgMetagnosticJniportDolphinNotification categoriesFor: #send_null!**auto generated**!Java-methods!Java-public!public! !

!OrgMetagnosticJniportDolphinNotification class methodsFor!

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
		#send_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.DolphinNotification'.
! !
!OrgMetagnosticJniportDolphinNotification class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinNotification class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinNotification class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinNotification class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinNotification class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

OrgMetagnosticJniportDolphinRequest guid: (GUID fromString: '{8BF13DEE-4CF3-49C7-9C89-FC17F8173675}')!
OrgMetagnosticJniportDolphinRequest comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances wrap references to instances of the Java class org.metagnostic.jniport.DolphinRequest'!
!OrgMetagnosticJniportDolphinRequest categoriesForClass!Unclassified! !
!OrgMetagnosticJniportDolphinRequest methodsFor!

checkForException_null
	"answer the result of calling the receiver's public checkForException() Java method"

	^ self callBooleanMethod: 'checkForException'.
!

getException_null
	"answer the result of calling the receiver's public getException() Java method"

	^ self callObjectMethod: 'getException' signature: '()Ljava/lang/Throwable;'.
!

getReturnValue_null
	"answer the result of calling the receiver's public getReturnValue() Java method"

	^ self callObjectMethod: 'getReturnValue' signature: '()Ljava/lang/Object;'.
!

isComplete_null
	"answer the result of calling the receiver's public synchronized isComplete() Java method"

	^ self callBooleanMethod: 'isComplete'.
!

isNotificationOnly_null
	"answer the result of calling the receiver's public isNotificationOnly() Java method"

	^ self callBooleanMethod: 'isNotificationOnly'.
!

requiresAcknowledgement
	"answer whether this request requres an answer"

	"overridden 'cos we already know the answer"
	^ true.!

startEvaluation_null
	"invoke the receiver's public startEvaluation() Java method"

	self callVoidMethod: 'startEvaluation'.
!

toString_null
	"answer the result of calling the receiver's public toString() Java method"

	^ self callObjectMethod: 'toString' signature: '()Ljava/lang/String;'.
!

value_null
	"answer the result of calling the receiver's public value() Java method"

	^ self callObjectMethod: 'value' signature: '()Ljava/lang/Object;'.
!

waitForResponse_null
	"invoke the receiver's public synchronized waitForResponse() Java method"

	self callVoidMethod: 'waitForResponse'.
! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #checkForException_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #getException_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #getReturnValue_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #isComplete_null!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #isNotificationOnly_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #requiresAcknowledgement!accessing!public!testing! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #startEvaluation_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #toString_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #value_null!**auto generated**!Java-methods!Java-public!public! !
!OrgMetagnosticJniportDolphinRequest categoriesFor: #waitForResponse_null!**auto generated**!Java-methods!Java-public!Java-synchronized!public! !

!OrgMetagnosticJniportDolphinRequest class methodsFor!

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
		#checkForException_null
		#getException_null
		#getReturnValue_null
		#isComplete_null
		#isNotificationOnly_null
		#startEvaluation_null
		#toString_null
		#value_null
		#waitForResponse_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.DolphinRequest'.
! !
!OrgMetagnosticJniportDolphinRequest class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinRequest class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinRequest class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinRequest class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!OrgMetagnosticJniportDolphinRequest class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticOrgMetagnosticJniportDolphinRequestQueue guid: (GUID fromString: '{A0E6F9C3-CA74-4DD2-B856-C9918F7C2635}')!
StaticOrgMetagnosticJniportDolphinRequestQueue comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances reify the Java class org.metagnostic.jniport.DolphinRequestQueue and provide access to its class-side methods.'!
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesForClass!Unclassified! !
!StaticOrgMetagnosticJniportDolphinRequestQueue methodsFor!

dequeue_int: int1
	"answer the result of calling the receiver's private static synchronized dequeue(int) Java method"

	| args |

	args := (JNIValueArray new: 1)
			intAt: 1 put: int1;
			yourself.

	^ self callObjectMethod: 'dequeue' signature: '(I)[Lorg/metagnostic/jniport/AbstractDolphinRequest;' withArguments: args.!

dequeue_null
	"answer the result of calling the receiver's private static synchronized dequeue() Java method"

	^ self callObjectMethod: 'dequeue' signature: '()Lorg/metagnostic/jniport/AbstractDolphinRequest;'.
!

enqueue_AbstractDolphinRequest: anAbstractDolphinRequest1
	"invoke the receiver's static synchronized enqueue(org.metagnostic.jniport.AbstractDolphinRequest) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: anAbstractDolphinRequest1;
			yourself.

	self callVoidMethod: 'enqueue' signature: '(Lorg/metagnostic/jniport/AbstractDolphinRequest;)V' withArguments: args.
!

log_String: aString1
	"invoke the receiver's public static log(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'log' signature: '(Ljava/lang/String;)V' withArguments: args.
!

log_String: aString1 AbstractDolphinRequest: anAbstractDolphinRequest1
	"invoke the receiver's public static log(java.lang.String, org.metagnostic.jniport.AbstractDolphinRequest) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: anAbstractDolphinRequest1;
			yourself.

	self callVoidMethod: 'log' signature: '(Ljava/lang/String;Lorg/metagnostic/jniport/AbstractDolphinRequest;)V' withArguments: args.
!

log_String: aString1 DolphinNotifierThread: aDolphinNotifierThread1
	"invoke the receiver's public static log(java.lang.String, org.metagnostic.jniport.DolphinNotifierThread) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 2)
			objectAt: 1 put: aString1Ref;
			objectAt: 2 put: aDolphinNotifierThread1;
			yourself.

	self callVoidMethod: 'log' signature: '(Ljava/lang/String;Lorg/metagnostic/jniport/DolphinNotifierThread;)V' withArguments: args.
!

nextRequest
	"answer the next enqueued request or nil"

	^ self dequeue_null.!

nextRequests: anInteger
	"answer an array of the next anInteger requests or nil"

	^ self dequeue_int: anInteger.!

requestQueueLength_null
	"answer the result of calling the receiver's public static synchronized requestQueueLength() Java method"

	^ self callIntMethod: 'requestQueueLength'.
!

requestQueueMax_null
	"answer the result of calling the receiver's public static synchronized requestQueueMax() Java method"

	^ self callIntMethod: 'requestQueueMax'.
!

requestsCompleted_null
	"answer the result of calling the receiver's public static synchronized requestsCompleted() Java method"

	^ self callLongMethod: 'requestsCompleted'.
!

shutdown
	"ask the request queue to shutdown and fail any pending requests"

	self shutdown_null.!

shutdown_null
	"invoke the receiver's public static synchronized shutdown() Java method"

	self callVoidMethod: 'shutdown'.
!

startLogging_PrintStream: aPrintStream1
	"invoke the receiver's public static startLogging(java.io.PrintStream) Java method"

	| args |

	args := (JNIValueArray new: 1)
			objectAt: 1 put: aPrintStream1;
			yourself.

	self callVoidMethod: 'startLogging' signature: '(Ljava/io/PrintStream;)V' withArguments: args.
!

startLogging_String: aString1
	"invoke the receiver's public static startLogging(java.lang.String) Java method"

	| args aString1Ref |

	aString1Ref := aString1 asJavaString: self jvm.
	args := (JNIValueArray new: 1)
			objectAt: 1 put: aString1Ref;
			yourself.

	self callVoidMethod: 'startLogging' signature: '(Ljava/lang/String;)V' withArguments: args.
!

stopLogging_null
	"invoke the receiver's public static stopLogging() Java method"

	self callVoidMethod: 'stopLogging'.
! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #dequeue_int:!**auto generated**!Java-methods!Java-private!Java-static!Java-synchronized!private! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #dequeue_null!**auto generated**!Java-methods!Java-private!Java-static!Java-synchronized!private! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #enqueue_AbstractDolphinRequest:!**auto generated**!Java-methods!Java-static!Java-synchronized!private! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #log_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #log_String:AbstractDolphinRequest:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #log_String:DolphinNotifierThread:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #nextRequest!accessing!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #nextRequests:!accessing!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #requestQueueLength_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #requestQueueMax_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #requestsCompleted_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #shutdown!operations!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #shutdown_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #startLogging_PrintStream:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #startLogging_String:!**auto generated**!Java-methods!Java-public!Java-static!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue categoriesFor: #stopLogging_null!**auto generated**!Java-methods!Java-public!Java-static!public! !

!StaticOrgMetagnosticJniportDolphinRequestQueue class methodsFor!

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
		#dequeue_int:
		#dequeue_null
		#enqueue_AbstractDolphinRequest:
		#log_String:
		#log_String:AbstractDolphinRequest:
		#log_String:DolphinNotifierThread:
		#requestQueueLength_null
		#requestQueueMax_null
		#requestsCompleted_null
		#shutdown_null
		#startLogging_PrintStream:
		#startLogging_String:
		#stopLogging_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.DolphinRequestQueue'.
! !
!StaticOrgMetagnosticJniportDolphinRequestQueue class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinRequestQueue class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

StaticOrgMetagnosticJniportDolphinNotifierThread guid: (GUID fromString: '{EAC5BCE2-8132-4EBD-8BB9-45DDABFABC23}')!
StaticOrgMetagnosticJniportDolphinNotifierThread comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Instances reify the Java class org.metagnostic.jniport.DolphinNotifierThread and provide access to its class-side methods.'!
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesForClass!Unclassified! !
!StaticOrgMetagnosticJniportDolphinNotifierThread methodsFor!

getDemon_null
	"answer the result of calling the receiver's public static synchronized getDemon() Java method"

	^ self callObjectMethod: 'getDemon' signature: '()Lorg/metagnostic/jniport/DolphinNotifierThread;'.
!

implementNotifierMethodBy: a0Block
	"private -- set the implementation of our native 'notifier' method"

	self
		implementNative: self notifierMethod
		by: a0Block.!

notifierMethod
	"private -- answer our native 'notifier' method"

	^ self classObject
		getDeclaredMethod_String: 'dolphinNotifierMethod'
		ClassArray: nil.
!

setDolphinNativeThread
	"invoke the receiver's private static synchronized setDolphinNativeThread() Java method"

	self setDolphinNativeThread_null.
!

setDolphinNativeThread_null
	"invoke the receiver's private static synchronized setDolphinNativeThread() Java method"

	self callVoidMethod: 'setDolphinNativeThread'.
! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #getDemon_null!**auto generated**!Java-methods!Java-public!Java-static!Java-synchronized!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #implementNotifierMethodBy:!Java native methods!private! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #notifierMethod!Java native methods!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #setDolphinNativeThread!operations!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread categoriesFor: #setDolphinNativeThread_null!**auto generated**!Java-methods!Java-private!Java-static!Java-synchronized!private! !

!StaticOrgMetagnosticJniportDolphinNotifierThread class methodsFor!

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
		#getDemon_null
		#setDolphinNativeThread_null
	).
!

javaClassName
	"answer the Symbol name of the Java class we stand for"

	^ #'org.metagnostic.jniport.DolphinNotifierThread'.
! !
!StaticOrgMetagnosticJniportDolphinNotifierThread class categoriesFor: #generatedConstructorSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread class categoriesFor: #generatedGetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread class categoriesFor: #generatedSetterSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread class categoriesFor: #generatedWrapperSelectors!**auto generated**!constants!listing wrapper methods!public! !
!StaticOrgMetagnosticJniportDolphinNotifierThread class categoriesFor: #javaClassName!**auto generated**!accessing!constants!public! !

JVMWithCallbacks guid: (GUID fromString: '{508DD794-6041-4836-B96E-E76190B11B6D}')!
JVMWithCallbacks comment: 'Copyright © Chris Uppal, 2002, 2003.
chris.uppal@metagnostic.org

Special subclass of JVM that knows how to do callbacks.  The principle differences are:
	- we force *all* JNI references to be global as soon as we see them.
	- we don''t (therefore) bother to keep track of the local pool size.
	- we do set up a callback registry during initialisation.

Please note: despite the subclass relationship, and despite the fact that this class is in a separate (and separable) package from the superclass, and the other concrete subclass, this class and the other two are not independent in design.'!
!JVMWithCallbacks categoriesForClass!Unclassified! !
!JVMWithCallbacks methodsFor!

asCallbackDo: a0Block
	"answer the result of evaluating a0Block in an environment where we have
	replaced the current JNIEnv with the currently valid one.  This is used -- this *MUST*
	be used to implement callbacks/native methods"

	"Note: this isn't really reliable, and I don't see any way to make it so, since
	this is called *after* we've entered the callback, and there may already have
	been some other Process using the superseeded JNIEnv.  Similarly as we
	leave the callback, so other process may use the reinstated JNIEnv before
	we actually do leave the callback"

	self pushJniEnv.
	^ a0Block ensure: [self popJniEnv].
!

basicOwn: aJavaInstance
	"private -- aJavaInstance has just been born, keep track of it from now on.
	This is the implementation of #own: except that it is called under the protection
	of our shared mutex"

	super basicOwn: aJavaInstance.

	"ensure that the instance is global"
	aJavaInstance beGlobalRef.!

callbackDepth
	"answer how deeply nested we are in callbacks"

	^ jniEnvStack size.!

callbackRegistry
	"answer our registry of callbacks.
	Note that this gets set quite late in the bootstrap sequence, after class registration for instance,
	so if callbackRegistry is nil, then you are probably trying to use callbacks before bootstrap is complete"

	^ callbackRegistry.!

callbacks
	"answer how many callbacks have been serviced"

	^ callbackRegistry requestsCompleted.!

convertToGlobal: aJNIObject
	"private -- ensure that JNIObject is a global reference.
	We assume that aJNIObject is not Null.
	Note that we deliberately do NOT update the local/global object counts since this is
	used only for converting local refs to global as they are first encountered and before
	they have been wrapped"

	| answer |

	aJNIObject isGlobalRef ifTrue: [^ self].

	answer := aJNIObject
			getGlobalRef: jniEnv
			onException: [:jex | "what can we do here?" ^ self].
	aJNIObject
		releaseRef: jniEnv
		onException: [:jex | "can only ignore it"].

	"have to use #become: since there may/will be other refs to the object"
	answer become: aJNIObject.!

findClassFor: aJNIObject
	"private -- find/make the JavaStatic that will wrap aJNIObject"

	"we have to ensure that the ref is converted to global if necessary *before* the class
	lookup, since that might take considerable time during which callbacks might happen
	(especially if new ghost classes have to be constructed).
	Hence we convert it now, even though we will 'convert' again later (in #own:)"

	"NB: this will #become: the JNIObject into a global ref if necessary"
	self convertToGlobal: aJNIObject.

	^ super findClassFor: aJNIObject.!

initializeCallbackRegistry
	"private -- called during initialisation.  We are now sufficiently initialized for a callback registry
	to work; set one up now.
	NB: at this point the class registry etc, have been set up but the wrapper classes have not"

	jniEnvStack := OrderedCollection new: 10.
	callbackRegistry := JavaCallbackRegistry withJvm: self.
!

notifyNowLocal: aJavaInstance
	"private -- aJavaInstance has just become a global ref, adjust the counts accordingly"

	"we don't support local references"
	self assert: [false].!

popJniEnv
	"private -- we are just about to leave a callback, so reinstate the old
	JNIEnv as quickly as possible"

	[javaVM isNil ifTrue: [^ self].
	jniEnv := jniEnvStack removeLast]
		critical.!

pushJniEnv
	"private -- we have just been told that we are in a callback, so get a valid new
	JNIEnv as quickly as possible"

	[javaVM isNil ifTrue: [^ self].
	jniEnvStack addLast: jniEnv.
	jniEnv := javaVM newJNIEnvVersion: JNI_VERSION_1_2]
		critical.!

shutdownCallbackRegistry
	"private -- shutdown any callback registry we have"

	jniEnvStack := OrderedCollection new.

	"we close down the registry, but keep a handle on it (I can't remember why,
	but there must have been a reason once)"
	callbackRegistry notNil ifTrue: [callbackRegistry shutdown].! !
!JVMWithCallbacks categoriesFor: #asCallbackDo:!Java callbacks!public! !
!JVMWithCallbacks categoriesFor: #basicOwn:!events!managed objects!private! !
!JVMWithCallbacks categoriesFor: #callbackDepth!accessing!Java callbacks!public! !
!JVMWithCallbacks categoriesFor: #callbackRegistry!accessing!Java callbacks!public! !
!JVMWithCallbacks categoriesFor: #callbacks!accessing!Java callbacks!public! !
!JVMWithCallbacks categoriesFor: #convertToGlobal:!events!Java callbacks!managed objects!private! !
!JVMWithCallbacks categoriesFor: #findClassFor:!managed objects!private! !
!JVMWithCallbacks categoriesFor: #initializeCallbackRegistry!initializing!Java callbacks!private! !
!JVMWithCallbacks categoriesFor: #notifyNowLocal:!events!managed objects!private! !
!JVMWithCallbacks categoriesFor: #popJniEnv!Java callbacks!private! !
!JVMWithCallbacks categoriesFor: #pushJniEnv!Java callbacks!private! !
!JVMWithCallbacks categoriesFor: #shutdownCallbackRegistry!private!shutting down! !

"Binary Globals"!

"Resources"!

