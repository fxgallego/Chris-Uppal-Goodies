| package |
package := Package name: 'CU Wave Out'.
package paxVersion: 0;
	basicComment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Simple direct manipulation of (some of) the low-level Windows sound APIs.

The terms under which you may use this software are:
	You must not claim that you wrote it.
	You must not claim that you own it.
	You use it at your own risk.

I''d appreciate it if you didn''t distribute modified versions without at least trying to tell me about the changes you thought worthwhile.

	-- chris
'.

package basicPackageVersion: '1.08'.

package basicScriptAt: #preinstall put: 'Win32Constants

	at: ''WAVE_MAPPER''			put: -1;

	at: ''MM_WOM_OPEN''			put: 16r3BB;
	at: ''MM_WOM_CLOSE''			put: 16r3BC;
	at: ''MM_WOM_DONE''			put: 16r3BD;

	at: ''WAVE_FORMAT_QUERY''		put: 16r0001;
	at: ''WAVE_ALLOWSYNC''			put: 16r0002;
	at: ''WAVE_MAPPED''			put: 16r0004;
	at: ''WAVE_FORMAT_DIRECT''		put: 16r0008;
	at: ''WAVE_FORMAT_DIRECT_QUERY''	put: 16r0009;

	at: ''CALLBACK_NULL''			put: 16r00000000;
	at: ''CALLBACK_WINDOW''			put: 16r00010000;
	at: ''CALLBACK_FUNCTION''		put: 16r00030000;
	at: ''CALLBACK_THREAD''			put: 16r00020000;
	at: ''CALLBACK_EVENT''			put: 16r00050000;

	at: ''WHDR_DONE''				put: 16r00000001;
	at: ''WHDR_PREPARED''			put: 16r00000002;
	at: ''WHDR_BEGINLOOP''			put: 16r00000004;
	at: ''WHDR_ENDLOOP''			put: 16r00000008;
	at: ''WHDR_INQUEUE''			put: 16r00000010;

	at: ''WAVE_FORMAT_PCM''			put: 1;

	at: ''WAVE_INVALIDFORMAT''		put: 16r00000000;
	at: ''WAVE_FORMAT_1M08''		put: 16r00000001;
	at: ''WAVE_FORMAT_1S08''			put: 16r00000002;
	at: ''WAVE_FORMAT_1M16''		put: 16r00000004;
	at: ''WAVE_FORMAT_1S16''			put: 16r00000008;
	at: ''WAVE_FORMAT_2M08''		put: 16r00000010;
	at: ''WAVE_FORMAT_2S08''			put: 16r00000020;
	at: ''WAVE_FORMAT_2M16''		put: 16r00000040;
	at: ''WAVE_FORMAT_2S16''			put: 16r00000080;
	at: ''WAVE_FORMAT_4M08''		put: 16r00000100;
	at: ''WAVE_FORMAT_4S08''			put: 16r00000200;
	at: ''WAVE_FORMAT_4M16''		put: 16r00000400;
	at: ''WAVE_FORMAT_4S16''			put: 16r00000800;
	at: ''WAVE_FORMAT_48M08''		put: 16r00001000;
	at: ''WAVE_FORMAT_48S08''		put: 16r00002000;
	at: ''WAVE_FORMAT_48M16''		put: 16r00004000;
	at: ''WAVE_FORMAT_48S16''		put: 16r00008000;
	at: ''WAVE_FORMAT_96M08''		put: 16r00010000;
	at: ''WAVE_FORMAT_96S08''		put: 16r00020000;
	at: ''WAVE_FORMAT_96M16''		put: 16r00040000;
	at: ''WAVE_FORMAT_96S16''		put: 16r00080000;

	at: ''WAVECAPS_PITCH''			put: 16r0001;
	at: ''WAVECAPS_PLAYBACKRATE''	put: 16r0002;
	at: ''WAVECAPS_VOLUME''			put: 16r0004;
	at: ''WAVECAPS_LRVOLUME''		put: 16r0008;
	at: ''WAVECAPS_SYNC''			put: 16r0010;
	at: ''WAVECAPS_SAMPLEACCURATE''	put: 16r0020;

	at: ''TIME_MS''				put: 16r0001;
	at: ''TIME_SAMPLES''			put: 16r0002;
	at: ''TIME_BYTES''				put: 16r0004;
	at: ''TIME_SMPTE''				put: 16r0008;
	at: ''TIME_MIDI''				put: 16r0010;
	at: ''TIME_TICKS''				put: 16r0020;

	yourself.
!!'.

package classNames
	add: #JangleGenerator;
	add: #MMTIME;
	add: #WAVEFORMATEX;
	add: #WAVEHDR;
	add: #WAVEOUTCAPS;
	add: #WaveOutError;
	add: #WaveOutHelperView;
	add: #WaveOutPlayer;
	yourself.

package methodNames
	add: #View -> #mmWomClose:wParam:lParam:;
	add: #View -> #mmWomDone:wParam:lParam:;
	add: #View -> #mmWomOpen:wParam:lParam:;
	add: #WinMMLibrary -> #waveOutBreakLoop:;
	add: #WinMMLibrary -> #waveOutClose:;
	add: #WinMMLibrary -> #waveOutGetDevCaps:pwoc:cbwoc:;
	add: #WinMMLibrary -> #waveOutGetErrorText:pszText:cchText:;
	add: #WinMMLibrary -> #waveOutGetNumDevs;
	add: #WinMMLibrary -> #waveOutGetPitch:pdwPitch:;
	add: #WinMMLibrary -> #waveOutGetPlaybackRate:pdwRate:;
	add: #WinMMLibrary -> #waveOutGetPosition:pmmt:cbmmt:;
	add: #WinMMLibrary -> #waveOutGetVolume:pdwVolume:;
	add: #WinMMLibrary -> #waveOutOpen:uDeviceID:pwfx:dwCallback:dwCallbackInstance:fdwOpen:;
	add: #WinMMLibrary -> #waveOutPause:;
	add: #WinMMLibrary -> #waveOutPrepareHeader:pwh:cbwh:;
	add: #WinMMLibrary -> #waveOutProcDescriptor;
	add: #WinMMLibrary -> #waveOutReset:;
	add: #WinMMLibrary -> #waveOutRestart:;
	add: #WinMMLibrary -> #waveOutSetPitch:dwPitch:;
	add: #WinMMLibrary -> #waveOutSetPlaybackRate:dwRate:;
	add: #WinMMLibrary -> #waveOutSetVolume:dwVolume:;
	add: #WinMMLibrary -> #waveOutUnprepareHeader:pwh:cbwh:;
	add: #WinMMLibrary -> #waveOutWrite:pwh:cbwh:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Program Files\Dolphin Smalltalk 5.1\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #JangleGenerator
	instanceVariableNames: 'notes decayPeriod samplesPerSec channels randoms scale offset scaledFrequency volumeLeft volumeRight decayPeriodSamples decayFactor tick'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #WaveOutPlayer
	instanceVariableNames: 'isPaused deviceIndex capabilities format handle preparedBuffers queuedBuffers recycler helper'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Error subclass: #WaveOutError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #MMTIME
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #WAVEFORMATEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #WAVEHDR
	instanceVariableNames: 'data'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #WAVEOUTCAPS
	instanceVariableNames: 'formatNames'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
View subclass: #WaveOutHelperView
	instanceVariableNames: 'player'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!View methodsFor!

mmWomClose: message wParam: wParam lParam: lParam
	"Private - Default handler for a MM_WOM_CLOSE message."
#CUadded.

	^ nil.!

mmWomDone: message wParam: wParam lParam: lParam
	"Private - Default handler for a MM_WOM_DONE message."
#CUadded.

	^ nil.!

mmWomOpen: message wParam: wParam lParam: lParam
	"Private - Default handler for a MM_WOM_OPEN message."
#CUadded.

	^ nil.! !
!View categoriesFor: #mmWomClose:wParam:lParam:!event handling-win32!private! !
!View categoriesFor: #mmWomDone:wParam:lParam:!event handling-win32!private! !
!View categoriesFor: #mmWomOpen:wParam:lParam:!event handling-win32!private! !

!WinMMLibrary methodsFor!

waveOutBreakLoop: anExternalHandle
	"The waveOutBreakLoop function breaks a loop on the given waveform-audio output device
	and allows playback to continue with the next block in the driver list.

		MMRESULT waveOutBreakLoop(
					HWAVEOUT	hwo
				);
	"

	<stdcall: dword waveOutBreakLoop handle>
#CUadded.
	^ self invalidCall.
!

waveOutClose: anExternalHandle
	"The waveOutClose function closes the given waveform-audio output device.

		MMRESULT waveOutClose(
					HWAVEOUT	hwo  
				);
	"

	<stdcall: dword waveOutClose handle>
#CUadded.
	^ self invalidCall.
!

waveOutGetDevCaps: a32BitValue pwoc: anWAVEOUTCAPS cbwoc: isSize
	"The waveOutGetDevCaps function retrieves the capabilities of a given waveform-audio output device.

		MMRESULT waveOutGetDevCaps(
					UINT_PTR		uDeviceID,
					LPWAVEOUTCAPS pwoc,
					UINT			cbwoc
				);

	"

	<stdcall: dword waveOutGetDevCapsA DWORD WAVEOUTCAPS* dword>
#CUadded.
	^ self invalidCall.
!

waveOutGetErrorText: anInteger pszText: aString cchText: itsSize
	"The waveOutGetErrorText function retrieves a textual description of the error identified by the given error number.

		MMRESULT waveOutGetErrorText(
					MMRESULT	mmrError,
					LPSTR	pszText,
					UINT		cchText
			);
	"

	<stdcall: dword waveOutGetErrorTextA dword lpstr dword>
#CUadded.
	^ self invalidCall.
!

waveOutGetNumDevs
	"The waveOutGetNumDevs function retrieves the number of waveform-audio output devices present in the system.

		UINT waveOutGetNumDevs(VOID);
	"

	<stdcall: dword waveOutGetNumDevs>
#CUadded.
	^ self invalidCall.
!

waveOutGetPitch: anExternalHandle pdwPitch: anAddress
	"The waveOutGetPitch function retrieves the current pitch setting for the specified waveform-audio output device.

		MMRESULT waveOutGetPitch(
					HWAVEOUT	hwo,
					LPDWORD	pdwPitch
				);
	"

	<stdcall: dword waveOutGetPitch handle dword*>
#CUadded.
	^ self invalidCall.
!

waveOutGetPlaybackRate: anExternalHandle pdwRate: anAddress
	"The waveOutGetPlaybackRate function retrieves the current playback rate for the specified waveform-audio output device.

		MMRESULT waveOutGetPlaybackRate(
					HWAVEOUT		hwo,
					LPDWORD		pdwRate
				);
	"

	<stdcall: dword waveOutGetPlaybackRate handle dword*>
#CUadded.
	^ self invalidCall.
!

waveOutGetPosition: anExternalHandle pmmt: anMMTIME cbmmt: itsSize
	"The waveOutGetPosition function retrieves the current playback position of the given waveform-audio output device.

		MMRESULT waveOutGetPosition(
					HWAVEOUT	hwo,
					LPMMTIME	pmmt,
					UINT		cbmmt
				);
	"

	<stdcall: dword waveOutGetPosition handle MMTIME* dword>
#CUadded.
	^ self invalidCall.
!

waveOutGetVolume: anExternalHandle pdwVolume: anAddress
	"The waveOutGetVolume function retrieves the current volume level of the specified waveform-audio output device.

		MMRESULT waveOutGetVolume(
					HWAVEOUT	hwo,      
					LPDWORD	pdwVolume
				);
	"

	<stdcall: dword waveOutGetVolume handle dword*>
#CUadded.
	^ self invalidCall.
!

waveOutOpen: anAddress uDeviceID: anIndex pwfx: aWAVEFORMATEX dwCallback: a32BitValue dwCallbackInstance: anObject fdwOpen: anInteger
	"The waveOutOpen function opens the given waveform-audio output device for playback.

		MMRESULT waveOutOpen(
					LPHWAVEOUT		phwo, 
					UINT_PTR			uDeviceID,
					LPWAVEFORMATEX	pwfx,
					DWORD_PTR		dwCallback,
					DWORD_PTR		dwCallbackInstance,
					DWORD			fdwOpen
				);
	"

	<stdcall: dword waveOutOpen handle dword WAVEFORMATEX* dword* void* dword>
#CUadded.
	^ self invalidCall.
!

waveOutPause: anExternalHandle
	"The waveOutPause function pauses playback on the given waveform-audio output device. The current position is saved.
	Use the waveOutRestart function to resume playback from the current position.

		MMRESULT waveOutPause(
					HWAVEOUT hwo  
				);
	"

	<stdcall: dword waveOutPause handle>
#CUadded.
	^ self invalidCall.
!

waveOutPrepareHeader: anExternalHandle pwh: aWAVEHDR cbwh: itsSize
	"The waveOutPrepareHeader function prepares a waveform-audio data block for playback.

		MMRESULT waveOutPrepareHeader(
					HWAVEOUT		hwo,
					LPWAVEHDR	pwh,
					UINT			cbwh
				);
	"

	<stdcall: dword waveOutPrepareHeader handle void* dword>
#CUadded.
	^ self invalidCall.
!

waveOutProcDescriptor
	"Answers an ExternalDescriptor suitable for representing the type of the callback paramter to the waveOutOpen function.

	You should note the following quote from the Windows documentation (reformated):
			Applications should not call any system-defined functions from inside a callback function, except for
				EnterCriticalSection, LeaveCriticalSection,
				midiOutLongMsg, midiOutShortMsg,
				OutputDebugString,
				PostMessage,
				PostThreadMessage,
				SetEvent,
				timeGetSystemTime, timeGetTime,
				timeKillEvent, and timeSetEvent.
			Calling other wave functions will cause deadlock.
	Ugh!!

		void CALLBACK waveOutProc(
					HWAVEOUT		hwo,
					UINT			uMsg,
					DWORD		dwInstance,
					DWORD		dwParam1,
					DWORD		dwParam2
				);
	"

#CUadded.
	^ ExternalDescriptor fromString: 'stdcall: void handle dword dword dword dword'.!

waveOutReset: anExternalHandle
	"The waveOutReset function stops playback on the given waveform-audio output device and resets the current position to zero.
	All pending playback buffers are marked as done and returned to the application.

		MMRESULT waveOutReset(
					HWAVEOUT hwo  
				);
	"

	<stdcall: dword waveOutReset handle>
#CUadded.
	^ self invalidCall.
!

waveOutRestart: anExternalHandle
	"The waveOutRestart function resumes playback on a paused waveform-audio output device.

		MMRESULT waveOutRestart(
				HWAVEOUT	hwo
			);
	"

	<stdcall: dword waveOutRestart handle>
#CUadded.
	^ self invalidCall.
!

waveOutSetPitch: anExternalHandle dwPitch: anInteger
	"The waveOutSetPitch function sets the pitch for the specified waveform-audio output device.

		MMRESULT waveOutSetPitch(
					HWAVEOUT	hwo,
					DWORD	dwPitch
				);
	"

	<stdcall: dword waveOutSetPitch handle dword>
#CUadded.
	^ self invalidCall.
!

waveOutSetPlaybackRate: anExternalHandle dwRate: anInteger
	"The waveOutSetPitch function sets the playback rate for the specified waveform-audio output device.

		MMRESULT waveOutSetPlaybackRate(
					HWAVEOUT	hwo,
					DWORD	dwPitch
				);
	"

	<stdcall: dword waveOutSetPlaybackRate handle dword>
#CUadded.
	^ self invalidCall.
!

waveOutSetVolume: anExternalHandle dwVolume: anInteger
	"The waveOutSetPitch function sets the volume for the specified waveform-audio output device.

		MMRESULT waveOutSetVolume(
					HWAVEOUT	hwo,
					DWORD	dwPitch
				);
	"

	<stdcall: dword waveOutSetVolume handle dword>
#CUadded.
	^ self invalidCall.
!

waveOutUnprepareHeader: anExternalHandle pwh: anObject cbwh: anInteger
	"The waveOutUnprepareHeader function cleans up the preparation performed by the waveOutPrepareHeader function.
	This function must be called after the device driver is finished with a data block. You must call this function before freeing the buffer.

		MMRESULT waveOutUnprepareHeader(
					HWAVEOUT		hwo,
					LPWAVEHDR	pwh,
					UINT			cbwh
				);
	"

	<stdcall: dword waveOutPrepareHeader handle void* dword>
#CUadded.
	^ self invalidCall.
!

waveOutWrite: anExternalHandle pwh: aWAVEHDR cbwh: itsSize
	"The waveOutWrite function sends a data block to the given waveform-audio output device.

		MMRESULT waveOutWrite(
					HWAVEOUT		hwo,
					LPWAVEHDR	pwh,
					UINT			cbwh
				);
	"

	<stdcall: dword waveOutWrite handle WAVEHDR* dword>
#CUadded.
	^ self invalidCall.
! !
!WinMMLibrary categoriesFor: #waveOutBreakLoop:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutClose:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetDevCaps:pwoc:cbwoc:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetErrorText:pszText:cchText:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetNumDevs!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetPitch:pdwPitch:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetPlaybackRate:pdwRate:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetPosition:pmmt:cbmmt:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutGetVolume:pdwVolume:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutOpen:uDeviceID:pwfx:dwCallback:dwCallbackInstance:fdwOpen:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutPause:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutPrepareHeader:pwh:cbwh:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutProcDescriptor!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutReset:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutRestart:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutSetPitch:dwPitch:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutSetPlaybackRate:dwRate:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutSetVolume:dwVolume:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutUnprepareHeader:pwh:cbwh:!public!win32 functions-wave audio! !
!WinMMLibrary categoriesFor: #waveOutWrite:pwh:cbwh:!public!win32 functions-wave audio! !

"End of package definition"!

"Source Globals"!

"Classes"!

JangleGenerator guid: (GUID fromString: '{8D4A7664-1CE1-4A23-AABA-E1E00D42E05E}')!
JangleGenerator comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Example of a possible #recycler for use with a WaveOutPlayer.  See WaveOutPlayer class>>example3.

(Actually, with the default settings the sound is not so much a jangle as a mournfull chiming)

Performance note:  on my 1.5 GHz machine, this takes about 240 milliseconds to fill a buffer with 1 second''s-worth of 44.1 kHz stereo data.  Ii''s only just keeping up, and it''s doing anything too complicated either.  The problem is that the floating point calculations create *LOTS* of intermediate Float objects, and that takes a lot of time.  It''s normally  a good idea to move this kind of processing into an external DLL.  (I tried doing that, and the externally implemented clone of the same functionality produces the same data in about 5.9 milliseconds -- a factor of 25 difference...)'!
!JangleGenerator categoriesForClass!Unclassified! !
!JangleGenerator methodsFor!

decayPeriod
	"answer how long, in seconds, notes take to die away"

	^ decayPeriod.!

decayPeriod: aNumber
	"set how long, in seconds, notes take to die away"

	decayPeriod := aNumber.!

format: aWAVEFORMATEX
	"private -- set the format we will generate, and the range of notes that we may create"

	| values |

	channels := aWAVEFORMATEX nChannels.	"1 or 2"
	samplesPerSec := aWAVEFORMATEX nSamplesPerSec.

	"work out how to scale value in the range [-1.0, +1.0] into the range implied by the number
	of bits per sample.  Unfortunately, for 8-bits we want a range [0, 255], but for 9 bits or more we
	want [-2**bits-1, +2**bits-1].   Sigh..."
	scale := 2 ** aWAVEFORMATEX wBitsPerSample / 2 - 1.
	offset := (scale <= 127) ifTrue: [1.0] ifFalse: [0.0].

	randoms := self makeRandomStream.
	tick := decayPeriodSamples := 0.!

generateNextNote
	"private -- use our random number generator to select the next note to play and its volume"

	scaledFrequency := self nextFrequency.
	volumeLeft := self nextVolume.
	volumeRight := channels = 2 ifTrue:[self nextVolume].
	decayPeriodSamples := self nextDecayPeriod.

	tick := 0.
!

initialize
	"private -- establish a coherent intial state"

	decayPeriod := self class defaultDecayPeriod.
	notes := self class defaultNotes.!

makeRandomStream
	"private -- answer the stream of random Floats that we will use to generate
	random numbers"

	| seed |

	seed := Time microsecondClockValue + self identityHash + 1.

	^ Random seed: seed.!

nextDecayPeriod
	"private -- use our random number generator to select how long, in samples, to play our next note"

	| smudge |

	smudge := randoms next * 0.1 + 0.95 .

	^ (decayPeriod * smudge * samplesPerSec) rounded.

!

nextFrequency
	"private -- use our random number generator to select the next note to play"

	| note |

	note := 1 + (notes size * randoms next) truncated.

	"it's not clear from the definition of Random>>next whether it can ever answer 1.0;
	this test is just in case it ever does"
	note > notes size ifTrue: [note := 1].

	^ (notes at: note)  * ##(Float pi * 2) / samplesPerSec.!

nextValue
	"private -- answer rhe next value to use, the answer should between -1.0 and 1.0"

	| angle |

	tick >= decayPeriodSamples ifTrue: [self generateNextNote].

	angle := tick * scaledFrequency.
	decayFactor := ((decayPeriodSamples - tick) asFloat / decayPeriodSamples asFloat).

	tick := tick + 1.

	^ angle sin.
!

nextVolume
	"private -- use our random number generator to select the next volume to play a note at"

	#CUtodo.  "should this be non-linear ?"
	^ randoms next.!

notes
	"answer the range of notes that we may create"

	^ notes.!

notes: anArray
	"set the range of notes that we may create"

	notes := anArray.!

populateInMono: anArray
	"private -- fill in an array with our next batch of audio data"

	1 to: anArray size do:
		[:i || value int |
		value := self nextValue.
		int := self scaleToLeft: value.
		anArray at: i put: int rounded].!

populateInStereo: anArray
	"private -- fill in an array with our next batch of audio data"

	1 to: anArray size / 2 do:
		[:i || value left right |
		value := self nextValue.
		left := self scaleToLeft: value.
		right := self scaleToRight: value.
		anArray
			at: 2*i-1 put: left rounded;
			at: 2*i put: right rounded].!

scaleToLeft: aFloat
	"private -- answer aFloat scaled by our left channel volume and converted to
	an integer in the correct range for our format"

	^ aFloat * decayFactor * volumeLeft + offset * scale.!

scaleToRight: aFloat
	"private -- answer aFloat scaled by our right channel volume and converted to
	an integer in the correct range for our format"

	^ aFloat * decayFactor * volumeRight + offset * scale.
!

value: anArray
	"called to fill in anArray with audio wave data that we generate.
	Should answer whether to continue (we always do)"

	channels = 1
		ifTrue: [self populateInMono: anArray]
		ifFalse: [self populateInStereo: anArray].

	^ true.! !
!JangleGenerator categoriesFor: #decayPeriod!accessing!public! !
!JangleGenerator categoriesFor: #decayPeriod:!initializing!public! !
!JangleGenerator categoriesFor: #format:!initializing!private! !
!JangleGenerator categoriesFor: #generateNextNote!helpers!private! !
!JangleGenerator categoriesFor: #initialize!initializing!private! !
!JangleGenerator categoriesFor: #makeRandomStream!helpers!private! !
!JangleGenerator categoriesFor: #nextDecayPeriod!helpers!private! !
!JangleGenerator categoriesFor: #nextFrequency!helpers!private! !
!JangleGenerator categoriesFor: #nextValue!generating!private! !
!JangleGenerator categoriesFor: #nextVolume!helpers!private! !
!JangleGenerator categoriesFor: #notes!accessing!public! !
!JangleGenerator categoriesFor: #notes:!initializing!public! !
!JangleGenerator categoriesFor: #populateInMono:!generating!private! !
!JangleGenerator categoriesFor: #populateInStereo:!generating!private! !
!JangleGenerator categoriesFor: #scaleToLeft:!generating!private! !
!JangleGenerator categoriesFor: #scaleToRight:!generating!private! !
!JangleGenerator categoriesFor: #value:!evaluating!generating!public! !

!JangleGenerator class methodsFor!

defaultDecayPeriod
	"answer the number of seconds that instances notes take to decay default"

	^ 0.60.!

defaultNotes
	"answer an Array of the notes to use by default"

	"NB an earlier version of this accidentally produced the first note of the
	array twice as often as the others.
	I rather liked the effect and have kept it deliberately"
	^ #(264.0 264.0 297.0 316.8 352.0 396.0 422.4 495.0).!

format: aWAVEFORMATEX
	"answer a new instance that will generate sound data in the given format"

	^ (self new)
		format: aWAVEFORMATEX;
		yourself.
!

new
	"private -- use #format:"

	^ (self basicNew)
		initialize;
		yourself.! !
!JangleGenerator class categoriesFor: #defaultDecayPeriod!constants!public! !
!JangleGenerator class categoriesFor: #defaultNotes!constants!public! !
!JangleGenerator class categoriesFor: #format:!instance creation!public! !
!JangleGenerator class categoriesFor: #new!instance creation!private! !

WaveOutPlayer guid: (GUID fromString: '{E12F1E9D-C904-44F6-9EE1-CEBD318513B2}')!
WaveOutPlayer comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these represents one Window''s "wave out device".  While it is open it can be used to make noises.

Instances can be used in two "modes".  You can either create one (as per the examples) and then #play: buffers until you get sick of it.  Of you can set a recycler block which will be used to refill buffers as they finish playing.  E.g, you can set up two buffers and a recycler that fills them in to get continuous playing with "double-buffering" for smoothness.

Instances generate a few events to say what''s happening to them.  They can be #paused, #restarted, and #reset.

See the class-side methods in category ''examples'' for some examples.
'!
!WaveOutPlayer categoriesForClass!Unclassified! !
!WaveOutPlayer methodsFor!

audioFormat
	"answer the WAVEFORMATEX that we have been opened to play, or nil if
	we are not open"

	^ format.!

breakLoop
	"attempt to 'break' our current playback, i.e. to abandon the current looping buffer and move on
	to whatever is next in sequence.
	NB: this does /NOT/ affect buffer recycling (which is a different concept from looping)"

	| code  |

	self isOpen ifFalse: [^ self].

	code := self library waveOutBreakLoop: handle.
	self checkMMError: code.

	"breaking the loop implicitly restarts paused buffers"
	isPaused := false.
!

canPlayFormat: aWAVEFORMATEX
	"answer whether we stand for a WOM device that is actually installed and able to reproduce
	the given format"

	| code |

	code := self library
			waveOutOpen: nil
			uDeviceID: deviceIndex-1
			pwfx: aWAVEFORMATEX
			dwCallback: 0
			dwCallbackInstance: CALLBACK_NULL
			fdwOpen: (WAVE_ALLOWSYNC | WAVE_FORMAT_QUERY).

	^ code isZero.!

capabilities
	"answer a WAVEOUTCAPS describing the capabilites of the device we
	connect to"

	^ capabilities ifNil: [capabilities := self getCapabilities].!

checkMMError: anInteger
	"private -- check that anInteger is not an error return; convert it into
	a thrown WaveOutError if it is"

	anInteger > 0 ifTrue: [WaveOutError signalCode: anInteger].!

close
	"attempt to close this device"

	| code oldHandle |

	self isOpen ifFalse: [^ self].

	"ensure we're not still playing"
	self reset.

	"make us look closed before we try it, that way if it fails we won't try again"
	oldHandle := handle.
	handle := nil.

	code := self library waveOutClose: oldHandle.
	self checkMMError: code.

	self notifyCloseRequested.!

deviceIndex
	"answer our device index (note that this is 1-based, and 0 means 'system default')"

	^ deviceIndex.
!

deviceIndex: anInteger
	"private -- set our device index to anInteger"

	deviceIndex := anInteger.!

die
	"private -- throw away any record of resources we may have been using"

	capabilities := format := handle := preparedBuffers := queuedBuffers := recycler := helper := nil.
	isPaused := false.
	self beUnfinalizable.
!

discardBuffer: aWAVEHDR
	"private -- unprepare the given buffer and then trigger a notification to say that we are
	finished with it"

	self
		unprepareBuffer: aWAVEHDR;
		notifyBufferFinished: aWAVEHDR.

	self isIdle ifTrue: [self notifyIdle].!

discardOrRecycleBuffer: aWAVEHDR
	"private -- if we are set up to recycle buffers continuously then do so with this one, otherwise
	upprepare it, and discard it (triggering notification as we do so)"

	self isRecyclingBuffers
		ifTrue: [self recycleBuffer: aWAVEHDR]
		ifFalse: [self discardBuffer: aWAVEHDR].!

displayOn: aStream
	"append a user-centric description of ourself to aStream"

	aStream display: self capabilities szPname.!

finalize
	"called by the finaliser if we are still open when we are GCed"

	[self close]
		on: Error
		do: [:ex | "ignore it..."].!

getCapabilities
	"private -- answer a WAVEOUTCAPS describing the capabilites of the device we
	connect to.
	This is cached by #capabilites"

	| caps code |

	caps := WAVEOUTCAPS new.
	code := self library
			waveOutGetDevCaps: deviceIndex-1
			pwoc: caps
			cbwoc: caps byteSize.
	self checkMMError: code.

	^ caps.	!

getPositionInto: anMMTIME
	"read our playback position into anMMTIME (which /may/ fill it in with the requested format,
	or may change the format).
	Has no effect if we are not open"

	| code |

	self isOpen ifFalse: [^ self].

	code := self library
			waveOutGetPosition: handle
			pmmt: anMMTIME
			cbmmt: anMMTIME byteSize.
	self checkMMError: code.

!

handle
	"private -- answer our handle"

	^ handle.
!

initialize
	"private -- establish a coherent initial state"

	 isPaused := false.
!

isIdle
	"answer whether we have finished playing all the submitted buffers"

	^ queuedBuffers isNil or: [queuedBuffers isEmpty].!

isOpen
	"answer whether we are currently open"

	^ handle notNil.!

isPaused
	"answer whether we are currently paused"

	^self isOpen and: [ isPaused].!

isRecyclingBuffers
	"answer whether we are set up to recycle buffers"

	^ self isOpen and: [recycler notNil].!

library
	"answer the WinMM library we make use of"

	^ WinMMLibrary default.!

makeCallbackWindow
	"private -- nasty, nasty, nasty.  Because the callback function API doesn't work (it deadlocks -- as,
	admitedly, it is documented to do -- we have to use a private window to recieve and redirect messages
	back to us"

	^ (WaveOutHelperView new)
		player: self;
		parentView: View desktop;
		create;
		yourself.!

notifyBufferFinished: aWAVEHDR
	"private -- trigger notification to inform Observers that we have finished
	playing the given buffer"

	self trigger: #finished: with: aWAVEHDR.

	!

notifyBufferQueued: aWAVEHDR
	"private -- trigger notification to inform Observers that we have enqueued
	the given buffer for playing"

	self trigger: #enqueued: with: aWAVEHDR.

	!

notifyClosed
	"private -- trigger notification to inform Observers that we are now closed"

	self trigger: #closed.

	!

notifyCloseRequested
	"private -- trigger notification to inform Observers that we have been asked to close down"

	self trigger: #closeRequested.

	!

notifyIdle
	"private -- trigger notification to inform Observers that we have finished
	playing the all the buffers that we have been asked to, and are now
	idle and silent as the grave"

	self trigger: #idle.
	!

notifyOpen
	"private -- trigger notification to inform Observers that we are now open"

	self trigger: #opened.

	!

notifyPaused
	"private -- trigger notification to inform Observers that we are paused"

	self trigger: #paused.

	!

notifyReset
	"private -- trigger notification to inform Observers that we have been reset"

	self trigger: #reset.

	!

notifyRestarted
	"private -- trigger notification to inform Observers that we have been restarted after a pause"

	self trigger: #restarted.

	!

onWaveOutClose
	"private -- handler for the MM_WOM_CLOSE message
	forwarded from our helper window"

"	Transcript display: 'WOP closed'; cr.	"

	helper ifNotNil: [:it | it destroy].
	self die.
	self notifyClosed.
!

onWaveOutDone: anInteger
	"private -- handler for the MM_WOM_DONE message
	forwarded from our helper window"

	| buffer |

"	Transcript display: 'WOP done: '; display: anInteger hex; cr.		"

	"find the corresponding buffer (and check it !!)"
	buffer := queuedBuffers removeFirst.
	buffer yourAddress = anInteger ifFalse:
		[| message |
		message := 'WOP: expected buffer: 0x%X, found 0x%X'
					sprintfWith: buffer yourAddress
					with: anInteger.
		Notification signal: message.
		^ self].

	self discardOrRecycleBuffer: buffer.!

onWaveOutOpen
	"private -- handler for the MM_WOM_OPEN message
	forwarded from our helper window"

"	Transcript display: 'WOP open'; cr.		"

	self notifyOpen.!

openForFormat: aWAVEFORMATEX
	"attempt to open this device.  The format defines the kind of audio data we will send to it"

	| code newHelper newHandle |

	"ensure we are closed first"
	self close.

	newHelper := self makeCallbackWindow.
	newHandle := ExternalHandle new.
	code := self library
			waveOutOpen: newHandle yourAddress
			uDeviceID: deviceIndex-1
			pwfx: aWAVEFORMATEX
			dwCallback: newHelper asParameter value
			dwCallbackInstance: 0
			fdwOpen: (WAVE_ALLOWSYNC | CALLBACK_WINDOW).					"<-- not sure ALLOWSYNC is optimal, or even correct"
	[self checkMMError: code] ifCurtailed: [newHelper destroy].

	handle := newHandle value.
	helper := newHelper.
	preparedBuffers := OrderedCollection new.
	queuedBuffers := OrderedCollection new.
	self beFinalizable.

!

packMultiplier: aNumber
	"private -- pack the given positive number into a 32-bit integer as required by the MM API"

	| integerPart factionalPart packed |

	integerPart := aNumber integerPart.
	factionalPart := aNumber fractionPart.
	packed := (integerPart asInteger bitShift: 16) bitOr: (factionalPart * 16rFFFF) asInteger.

	^ packed.!

pause
	"attempt to pause our current playback"

	| code  |

	self isOpen ifFalse: [^ self].

	code := self library waveOutPause: handle.
	self checkMMError: code.

	isPaused := true.
	self notifyPaused.

!

pitch
	"answer our pitch multiplier or nil if we are not open"

	| packed code |

	self isOpen ifFalse: [^ nil].

	packed := DWORD new.
	code := self library
			waveOutGetPitch: handle
			pdwPitch: packed yourAddress.
	self checkMMError: code.

	^ self unpackMultiplier: packed value.!

pitch: aNumber
	"set our pitch multiplier; will fail if we are not open"

	| packed code |

	packed := self packMultiplier: aNumber.
	code := self library
			waveOutSetPitch: handle
			dwPitch: packed.
	self checkMMError: code.!

play: aWAVEHDR
	"arrange to play the given buffer (after any currently playing buffers have completed)"

	self
		prepareBuffer: aWAVEHDR;
		writeBuffer: aWAVEHDR;
!

play: aWAVEHDR loops: anInteger
	"arrange to play the given buffer anInteger times (after any currently playing buffers have completed)."

	self prepareBuffer: aWAVEHDR.
	aWAVEHDR loops: anInteger.		"must do this after preparation"
	self writeBuffer: aWAVEHDR.
!

playbackRate
	"answer our playback rate multiplier or nil if we are not open"

	| packed code |

	self isOpen ifFalse: [^ nil].

	packed := DWORD new.
	code := self library
			waveOutGetPlaybackRate: handle
			pdwRate: packed yourAddress.
	self checkMMError: code.

	^ self unpackMultiplier: packed value.!

playbackRate: aNumber
	"set our playback rate multiplier; will fail if we are not open"

	| packed code |

	packed := self packMultiplier: aNumber.
	code := self library
			waveOutSetPlaybackRate: handle
			dwRate: packed.
	self checkMMError: code.!

position
	"answer our playback positon as an MMTIME or nil if we are not open.
	NB: the format of the resulting time data is up to the device, although we
	/ask/ for it in as a sample index, it may come back formatted differently"

	| time |

	self isOpen ifFalse: [^ nil].

	time := MMTIME sampleIndex.
	self getPositionInto: time.

	^ time.!

prepareBuffer: aWAVEHDR
	"private -- submit the given buffer to Windows for 'preparation'
	(According to Petzond, this tells Windows not to swap the data out)"

	| code |

	self isOpen ifFalse: [^ self].
	(preparedBuffers includes: aWAVEHDR) ifTrue: [^ self].

	aWAVEHDR
		dwFlags: 0;
		dwLoops: 1.

	code := self library
			waveOutPrepareHeader: handle
			pwh: aWAVEHDR
			cbwh: aWAVEHDR byteSize.
	self checkMMError: code.

	preparedBuffers addLast: aWAVEHDR.!

printOn: aStream
	"append a developer-centric description of ourself to aStream"

	aStream
		basicPrint: self;
		display: ' (';
		display: self;
		display: ')'.!

queueSize
	"answer how many buffers are currently queued up for playing"

	^ queuedBuffers size.!

recycle: aCollectionOfBuffers with: a1Block
	"this sets up automatic recycling of buffers as they are finished.

	a1Block should be a <monadicValuable> that is called whenever we
	have finished playing a buffer.  The parameter passed is the buffer's
	#data (NOT the buffer itself).  If the block does not answer true, then
	the buffer wil be discarded, otherwise it will be re-enqueued for playing.

	The Collection of buffers may contain any number of 'blank' buffers,
	and we will use the block to 'fill them in' before playing them.

	Note that the recycler applies to /all/ buffers, not just those supplied,
	and so you can set a recycler, and then #play: more buffers which
	will be played normally and then recycled thereafter.  This can be
	used to play a (non-blank) buffer forever with code like:

		player
			recycleWith: [:data | true];
			play: someBuffer.

	Recycling can be stopped by closing the player or by sending #stopRecycling"

	self isOpen ifFalse: [^ self].

	recycler := a1Block.
	aCollectionOfBuffers do: [:each | self prepareBuffer: each].
	aCollectionOfBuffers do: [:each | self recycleBuffer: each].
		!

recycleBuffer: aWAVEHDR
	"private -- refill the given buffer with data, and (if we haven't been asked to stop) re-submit it
	for playing"

	"the explicit test against true is because it's easy to forget to return a value from a block"
	(recycler value: aWAVEHDR data) = true ifTrue: [self writeBuffer: aWAVEHDR].!

recycleWith: a1Block
	"this sets up automatic recycling of buffers as they are finished.

	a1Block should be a <monadicValuable> that is called whenever we
	have finished playing a buffer.  The parameter passed is the buffer's
	#data (NOT the buffer itself).  If the block does not answer true, then
	the buffer wil be discarded, otherwise it will be re-enqueued for playing.

	Recycling can be stopped by closing the player or by sending #stopRecycling"

	self recycle: #() with: a1Block.!

reset
	"attempt to reset our current playback.
	Note, this clears any current recycler before doing anything or else the buffers
	would all just get restarted.
	Note #reset-ing removes any current 'pausedness'"

	| code  |

	self isOpen ifFalse: [^ self].

	self stopRecycling.

	code := self library waveOutReset: handle.
	self checkMMError: code.

	self notifyReset.

	isPaused ifTrue:
		[isPaused := false.
		self notifyRestarted].
!

restart
	"attempt to un-pause our current playback"

	| code  |

	self isPaused ifFalse: [^ self].

	code := self library waveOutRestart: handle.
	self checkMMError: code.

	isPaused := false.
	self notifyRestarted.
!

stopRecycling
	"this stops any automatic recycling of buffers as they are finished"

	recycler := nil.!

unpackMultiplier: anInteger
	"private -- unpack the given integer into a positive floating point value as required by the MM API"

	| integerPart factionalPart |

	integerPart := anInteger bitShift: -16.
	factionalPart := (anInteger bitAnd: 16rFFFF) asFloat / 16rFFFF.

	^ integerPart + factionalPart.!

unprepareBuffer: aWAVEHDR
	"private -- undo the effect of #prepareBuffer:"

	| code |

	self isOpen ifFalse: [^ self].
	preparedBuffers remove: aWAVEHDR ifAbsent: [^ self].

	code := self library
			waveOutUnprepareHeader: handle
			pwh: aWAVEHDR
			cbwh: aWAVEHDR byteSize.
	self checkMMError: code.

	aWAVEHDR dwFlags: 0.

!

volume
	"answer our volume, as a 2Array (left, right) of floats in the range 0.0 .. 1.0, or nil if we are not open"

	| packed code left right |

	self isOpen ifFalse: [^ nil].

	packed := DWORD new.
	code := self library
			waveOutGetVolume: handle
			pdwVolume: packed yourAddress.
	self checkMMError: code.

	left := packed value bitAnd: 16rFFFF.
	right := packed value bitShift: -16.

	^ Array
		with: (left / 65535.0)
		with: (right / 65535.0).
!

volume: a2Array
	"set our volume, as a 2Array (left, right) of floats in the range 0.0 .. 1.0.
	Will fail if we are not open"

	self volumeLeft: a2Array first right: a2Array second.!

volumeLeft: aLeftFloat right: aRightFloat
	"set our left and right channel volumes to those indicated.  The values should be in the range
	0.0 to 1.0.
	Will fail if we are not open"

	| left right packed code |

	left := (aLeftFloat * 16rFFFF) asInteger bitAnd: 16rFFFF.
	right := (aRightFloat * 16rFFFF) asInteger bitAnd: 16rFFFF.
	packed := (right bitShift: 16) bitOr: left.

	code := self library
			waveOutSetVolume: handle
			dwVolume: packed.
	self checkMMError: code.!

writeBuffer: aWAVEHDR
	"private -- submit the given buffer to Windows for playing.  It will be added to a qeueue if
	a buffer is already playing"

	| code |

	self isOpen ifFalse: [^ self].

"	Transcript display: 'WOP queueing: '; display: aWAVEHDR yourAddress hex; cr.		"

	code := self library
			waveOutWrite: handle
			pwh: aWAVEHDR
			cbwh: aWAVEHDR byteSize.
	self checkMMError: code.

	queuedBuffers addLast: aWAVEHDR.
! !
!WaveOutPlayer categoriesFor: #audioFormat!accessing!public! !
!WaveOutPlayer categoriesFor: #breakLoop!operations!public! !
!WaveOutPlayer categoriesFor: #canPlayFormat:!public!testing! !
!WaveOutPlayer categoriesFor: #capabilities!accessing!public! !
!WaveOutPlayer categoriesFor: #checkMMError:!exceptions!private! !
!WaveOutPlayer categoriesFor: #close!operations!public! !
!WaveOutPlayer categoriesFor: #deviceIndex!accessing!public! !
!WaveOutPlayer categoriesFor: #deviceIndex:!initializing!private! !
!WaveOutPlayer categoriesFor: #die!operations!private! !
!WaveOutPlayer categoriesFor: #discardBuffer:!bufffer handling!private! !
!WaveOutPlayer categoriesFor: #discardOrRecycleBuffer:!bufffer handling!private! !
!WaveOutPlayer categoriesFor: #displayOn:!displaying!public! !
!WaveOutPlayer categoriesFor: #finalize!finalizing!private! !
!WaveOutPlayer categoriesFor: #getCapabilities!accessing!private! !
!WaveOutPlayer categoriesFor: #getPositionInto:!accessing!public! !
!WaveOutPlayer categoriesFor: #handle!accessing!private! !
!WaveOutPlayer categoriesFor: #initialize!initializing!private! !
!WaveOutPlayer categoriesFor: #isIdle!public!testing! !
!WaveOutPlayer categoriesFor: #isOpen!public!testing! !
!WaveOutPlayer categoriesFor: #isPaused!public!testing! !
!WaveOutPlayer categoriesFor: #isRecyclingBuffers!bufffer handling!public!testing! !
!WaveOutPlayer categoriesFor: #library!accessing!private! !
!WaveOutPlayer categoriesFor: #makeCallbackWindow!helpers!private! !
!WaveOutPlayer categoriesFor: #notifyBufferFinished:!events!private! !
!WaveOutPlayer categoriesFor: #notifyBufferQueued:!events!private! !
!WaveOutPlayer categoriesFor: #notifyClosed!events!private! !
!WaveOutPlayer categoriesFor: #notifyCloseRequested!events!private! !
!WaveOutPlayer categoriesFor: #notifyIdle!events!private! !
!WaveOutPlayer categoriesFor: #notifyOpen!events!private! !
!WaveOutPlayer categoriesFor: #notifyPaused!events!private! !
!WaveOutPlayer categoriesFor: #notifyReset!events!private! !
!WaveOutPlayer categoriesFor: #notifyRestarted!events!private! !
!WaveOutPlayer categoriesFor: #onWaveOutClose!messages!private! !
!WaveOutPlayer categoriesFor: #onWaveOutDone:!bufffer handling!messages!private! !
!WaveOutPlayer categoriesFor: #onWaveOutOpen!messages!private! !
!WaveOutPlayer categoriesFor: #openForFormat:!operations!public! !
!WaveOutPlayer categoriesFor: #packMultiplier:!helpers!private! !
!WaveOutPlayer categoriesFor: #pause!operations!public! !
!WaveOutPlayer categoriesFor: #pitch!accessing!public! !
!WaveOutPlayer categoriesFor: #pitch:!accessing!public! !
!WaveOutPlayer categoriesFor: #play:!operations!public! !
!WaveOutPlayer categoriesFor: #play:loops:!operations!public! !
!WaveOutPlayer categoriesFor: #playbackRate!accessing!public! !
!WaveOutPlayer categoriesFor: #playbackRate:!accessing!public! !
!WaveOutPlayer categoriesFor: #position!accessing!public! !
!WaveOutPlayer categoriesFor: #prepareBuffer:!bufffer handling!private! !
!WaveOutPlayer categoriesFor: #printOn:!printing!public! !
!WaveOutPlayer categoriesFor: #queueSize!accessing!public! !
!WaveOutPlayer categoriesFor: #recycle:with:!operations!public! !
!WaveOutPlayer categoriesFor: #recycleBuffer:!bufffer handling!private! !
!WaveOutPlayer categoriesFor: #recycleWith:!operations!public! !
!WaveOutPlayer categoriesFor: #reset!operations!public! !
!WaveOutPlayer categoriesFor: #restart!operations!public! !
!WaveOutPlayer categoriesFor: #stopRecycling!operations!public! !
!WaveOutPlayer categoriesFor: #unpackMultiplier:!helpers!private! !
!WaveOutPlayer categoriesFor: #unprepareBuffer:!bufffer handling!private! !
!WaveOutPlayer categoriesFor: #volume!accessing!public! !
!WaveOutPlayer categoriesFor: #volume:!accessing!public! !
!WaveOutPlayer categoriesFor: #volumeLeft:right:!accessing!public! !
!WaveOutPlayer categoriesFor: #writeBuffer:!bufffer handling!private! !

!WaveOutPlayer class methodsFor!

availableDeviceCount
	"answer how many devices are available for use on this system.

		self availableDeviceCount.
	"

	^ WinMMLibrary default waveOutGetNumDevs.!

default
	"answer a new instance representing the wave mapper device (which is the default device)"

	"use +1 because the instance will subtract 1 before passing to Windows"
	^ self index: WAVE_MAPPER+1.!

example1
	"simple example of playing a 1-second generated wave sound

		self example1.
	"

	| format wave player |

	"decide what format we are going to play with -- in this case 16 bit mono at 44.1kHz.
	(step though this to see more options)"
	format := WAVEFORMATEX mono.

	"create the wave data that we wish to play -- a 1-second burst of 880 Hz sound"
	wave := self makePureTone: format hz: 880 duration: 1.

	"create a player to play it.
	This picks up the system's default player which is both connected to the
	default output device, and uses the 'wave mapper' which (apparently)
	is some sort of virtual device that maps requested formats to real devices
	that are capable of playing them (but don't take my word for it)"
	player := WaveOutPlayer default.

	"open the player to play our preferred format.  Each player can only
	play one format, but you can open more than one player if you wish"
	player openForFormat: format.

	"tell the player to play are low volume -- this is as much for safety as for the sake of the example
	(the wave itself is at maximum intensity, so it'd be pretty loud if the volume were turned up too)"
	player volumeLeft: 0.1 right: 0.1.

	"tell the player to start playing the sample.  This will return immediately leaving Windows playing
	the sound in the background"
	player play: wave.

	"pause for a bit to let the sound finish, otherwise the next line would 'kill' it prematurely.
	Normally there'd be no need for this, we could either leave the player playing, or -- if
	we were interested -- wait for it to trigger #idle or #finished: notification"
	MessageBox
		notify: 'Press OK when the noise stops'
		caption: self name.

	"for good measure, let's play it again a few times..."
	player play: wave loops: 4.
	MessageBox
		notify: 'Press OK when the noise stops again'
		caption: self name.

	"close the player.  This would happen by finalisation if we didn't do it explicitly"
	player close.!

example2
	"simple example of playing a 1-second generated wave sound several times.

		self example2.
	"

	| format wave player |

	"decide what format we are going to play with"
	format := WAVEFORMATEX mono.

	"create the wave data that we wish to play -- a 1-second burst of 880 Hz sound"
	wave := self makePureTone: format hz: 880 duration: 1.

	"create a player to play it.
	We'll use a short-hand form this time"
	player := WaveOutPlayer forFormat: format.
	player volumeLeft: 0.1 right: 0.1.

	"tell the player to start playing the sample.  This will return immediately leaving Windows
	playing the sound 10 times in the background.
	Note that it is /Windows/ that's doing the looping"
	player play: wave loops: 10.

	"pause for a bit to let the sound finish, otherwise the next line would 'kill' it prematurely"
	MessageBox
		notify: 'Press OK when the noise stops (or when you get sick of it)'
		caption: self name.

	"close the player.  This would happen by finalisation if we didn't do it explicitly"
	player close.!

example3
	"simple example of playing a continuously-generated sound by recycling buffers.

		self example3.
	"

	| format buffers recycler player |

	"decide what format we are going to play with.  In this case 16-bit stereo"
	format := WAVEFORMATEX stereo: 16.

	"create the buffers that will be used for the playback  We use half-second buffers in this
	example, and I've choosen to use six so as to give more slack in case Dolphiin can't
	always keep up with the player)"
	buffers := (1 to: 6) collect: [:i | WAVEHDR withWords: format nSamplesPerSec // 2].

	"create an example <monadicValuable> that will be used to generate the wave
	data as needed.  In fact I use this little example class, rather than, say, a Block"
	recycler := JangleGenerator format: format.

	"create and open a player for our choosen format"
	player := WaveOutPlayer forFormat: format.
	player volumeLeft: 0.2 right: 0.2.

	"tell the player to start cycling using the (currently blank) buffers that we've created
	and filling them in and recycling them as necessary"
	player recycle: buffers with: recycler.

	"pause for a bit to let the sound run"
	MessageBox notify: 'Press OK when when you get sick of this noise' caption: self name.

	"we may as well stop the recycler here.  It's unecessary since we're
	going to close it on the next line, but just for illustration"
	player stopRecycling.

	"close the player.  This would happen by finalisation if we didn't do it explicitly"
	player close.!

example4
	"simple example of playing several sounds at once by using several players.

		self example4.
	"

	| format players |

	format := WAVEFORMATEX stereo: 16.

	"This is the same as example3 only with a loop around it"
	players := (1 to: 2) collect:
			[:i || buffers recycler player |
			buffers := (1 to: 16) collect: [:j | WAVEHDR withWords: format nSamplesPerSec // 2].
			player := WaveOutPlayer forFormat: format.
			player volumeLeft: 0.2 right: 0.2.
			recycler := JangleGenerator format: format.
			player recycle: buffers with: recycler].

	"pause for a bit to let the sound run (watch the CPU working hard !!)"
	MessageBox
		notify: 'Press OK when if you tire of this soothing sound'
		caption: self name.

	"and close the players again -- this will stop the music"
	players do: [:each | each close].!

forFormat: aWAVEFORMATEX
	"answer a new instance that /should/ be able to play the given format.
	The instance will already be open, and therefore the caller should be prepared
	to #close it"

	"since the default is to open a wave-mapper device, it should be able to
	play any format for which a suitable device exists"
	^ (self default)
		openForFormat: aWAVEFORMATEX;
		yourself.!

index: anInteger
	"answer a new instance representing indexed device, where index should be either
	0 (meaning the default) or between 1 and our #availableDeviceCount"

	^ (self basicNew)
		initialize;
		deviceIndex: anInteger;
		yourself.!

initialize
	"private -- class-side initialisation

		self initialize.
	"

	SessionManager current when: #sessionStarted send: #onStartup to: self.
!

makePureTone: aWAVEFORMATEX hz: aFrequency duration: anInteger
	"private -- answer a WAVEHDR which has been set up with audio wave data
	corresponding to a pure note at the given frequency and duration in seconds, and
	where the data is encoded in the given format.
	The is a helper method for the examples, it has no other purpose, except
	perhaps as an example in its own right"

	| channels samples buffer data scale offset angleScale |

	channels := aWAVEFORMATEX nChannels.	"1 or 2"
	samples := aWAVEFORMATEX nSamplesPerSec * anInteger.

	"make an 8 or 16-bit buffer"
	buffer := (aWAVEFORMATEX wBitsPerSample = 8)
				ifTrue: [WAVEHDR withBytes: samples * channels]
				ifFalse: [WAVEHDR withWords: samples * channels].

	"get hold of its raw data array"
	data := buffer data.

	"work out how to scale value in the range [-1.0, +1.0] into the range implied by the number
	of bits per sample.  Unfortunately, for 8-bits we want a range [0, 255], but for 9 bits or more we
	want [-2**bits-1, +2**bits-1].   Sigh..."
	scale := 2 ** aWAVEFORMATEX wBitsPerSample / 2 - 1.
	offset := (scale <= 127) ifTrue: [1.0] ifFalse: [0.0].

	angleScale := aFrequency asFloat / aWAVEFORMATEX nSamplesPerSec * Float pi * 2.

	"fill it in, this is made slightly complicated by the need to allow for stereo"
	0 to: samples-1 do:
		[:i || angle value |
		angle := i * angleScale.
		value := (angle sin + offset * scale) rounded.
		channels = 1
			ifTrue: [data at: i+1 put: value]
			ifFalse: [data at: i*2+1 put: value; at: i*2+2 put: value]].

	^ buffer.!

onStartup
	"private -- make sure that all our instances know that they are dead"

	self primAllSubinstances do: [:each | each die].!

publishedEventsOfInstances
	"answer the published events triggered by our instances"

	^ (super publishedEventsOfInstances)
		add: #opened;
		add: #enqueued:;		"the given buffer has been added to the play list"
		add: #finished:;		"finished playing the given buffer, but may have more queued up to play"
		add: #idle;			"finished playing all the buffers submitted to date"
		add: #paused;		"playback has been temporarily suspended"
		add: #restarted;		"playback has been resumed"
		add: #closeRequested;	"close has been requested but the playback hasn't quiesced yet"
		add: #closed;			"close is now complete"
		yourself.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	SessionManager current removeEventsTriggeredFor: self.
! !
!WaveOutPlayer class categoriesFor: #availableDeviceCount!accessing!public! !
!WaveOutPlayer class categoriesFor: #default!instance creation!public! !
!WaveOutPlayer class categoriesFor: #example1!examples!public! !
!WaveOutPlayer class categoriesFor: #example2!examples!public! !
!WaveOutPlayer class categoriesFor: #example3!examples!public! !
!WaveOutPlayer class categoriesFor: #example4!examples!public! !
!WaveOutPlayer class categoriesFor: #forFormat:!instance creation!public! !
!WaveOutPlayer class categoriesFor: #index:!instance creation!public! !
!WaveOutPlayer class categoriesFor: #initialize!initializing!private! !
!WaveOutPlayer class categoriesFor: #makePureTone:hz:duration:!examples!private! !
!WaveOutPlayer class categoriesFor: #onStartup!event handling!private! !
!WaveOutPlayer class categoriesFor: #publishedEventsOfInstances!constants!development!events!public! !
!WaveOutPlayer class categoriesFor: #uninitialize!initializing!private! !

WaveOutError guid: (GUID fromString: '{0EEC66A2-7DB5-4D0F-9DD3-943E652F93AD}')!
WaveOutError comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Used for signalling errors and failures in the wave output stuff.'!
!WaveOutError categoriesForClass!Unclassified! !
!WaveOutError class methodsFor!

basicDecodeErrorCode: anInteger
	"private -- attempt to convert anInteger into a meaningfull error string"

	| string |

	string := String new: 256.
	WinMMLibrary default
		waveOutGetErrorText: anInteger
		pszText: string
		cchText: string size.
	^ string trimNulls.
!

decodeErrorCode: anInteger
	"attempt to convert anInteger into a meaningfull error string"

	^ [self basicDecodeErrorCode: anInteger]
		on: Error
		do: [:ex | ex return: ('Indecipherable WinMM error (', ex displayString , ')')].!

signalCode: anInteger
	"create and throw a new instance that contains the given error code"

	self
		signal: (self decodeErrorCode: anInteger)
		with: anInteger.! !
!WaveOutError class categoriesFor: #basicDecodeErrorCode:!private! !
!WaveOutError class categoriesFor: #decodeErrorCode:!private! !
!WaveOutError class categoriesFor: #signalCode:!instance creation!public! !

MMTIME guid: (GUID fromString: '{16D959C8-B242-4781-A1F4-8B216D5841EC}')!
MMTIME comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

Just bloody irritating, that''s all, just bloody irritating...'!
!MMTIME categoriesForClass!Unclassified! !
!MMTIME methodsFor!

cb
	"Answer the receiver's cb field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

getValidFields
	"private - overridden to allow for the union"

	"note: this is arranged to match the type values -- do not reorder!!"
	^ #(
	"milliseconds"		( #wType #ms )
	"sampleIndex"	( #wType #sample )
	"byteOffset"		( #wType #cb )
	"smpteTime"		( #wType #smtp_hour #smtp_min #smtp_sec #smtp_frame #smtp_fps )
	"midiTime"		( #wType #songptrpos )
	"midiTicks"		( #wType #ticks )
	) at: self wType highBit ifAbsent: [ ( #wType ) ].
!

initialize: anInteger
	"Private - Initialize the state of the receiver."

	super initialize: anInteger.
	self wType: TIME_MS.
!

isByteOffset
	"answer whether we hold time data in the byte offset format"

	^ self wType = TIME_BYTES.!

isMidiTicks
	"answer whether we hold time as MIDI ticks"

	^ self wType = TIME_TICKS.!

isMidiTime
	"answer whether we hold time data in MIDI time format"

	^ self wType = TIME_MIDI.!

isMilliseconds
	"answer whether we hold time in milliseconds"

	^ self wType = TIME_MS.
!

isSampleIndex
	"answer whether we hold time as a wave sample index"

	^ self wType = TIME_SAMPLES.!

isSmpteTime
	"answer whether we hold time data in the SMPTE time format"

	^ self wType = TIME_SMPTE.!

ms
	"Answer the receiver's ms field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

sample
	"Answer the receiver's sample field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

smtp_fps
	"Answer the receiver's smtp_fps field as a Smalltalk object."

	^(bytes byteAtOffset: 11)!

smtp_frame
	"Answer the receiver's smtp_frame field as a Smalltalk object."

	^(bytes byteAtOffset: 10)!

smtp_hour
	"Answer the receiver's smtp_hour field as a Smalltalk object."

	^(bytes byteAtOffset: 4)!

smtp_min
	"Answer the receiver's smtp_min field as a Smalltalk object."

	^(bytes byteAtOffset: 8)!

smtp_sec
	"Answer the receiver's smtp_sec field as a Smalltalk object."

	^(bytes byteAtOffset: 9)!

songptrpos
	"Answer the receiver's songptrpos field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ticks
	"Answer the receiver's ticks field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

typeName
	"answer the Symbol name of this format (the Symbol is the same
	as the selector of the corresponding instance creation method"

	"note: this is arranged to match the type values -- do not reorder!!"
	^ #(
				#milliseconds
				#sampleIndex
				#byteOffset
				#smpteTime
				#midiTime
				#midiTicks
	) at: self wType  highBit ifAbsent: [#unknown].

!

wType
	"Answer the receiver's wType field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

wType: anObject
	"Set the receiver's wType field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!MMTIME categoriesFor: #cb!**compiled accessors**!public! !
!MMTIME categoriesFor: #getValidFields!accessing!private! !
!MMTIME categoriesFor: #initialize:!initializing!private! !
!MMTIME categoriesFor: #isByteOffset!public!testing! !
!MMTIME categoriesFor: #isMidiTicks!public!testing! !
!MMTIME categoriesFor: #isMidiTime!public!testing! !
!MMTIME categoriesFor: #isMilliseconds!public!testing! !
!MMTIME categoriesFor: #isSampleIndex!public!testing! !
!MMTIME categoriesFor: #isSmpteTime!public!testing! !
!MMTIME categoriesFor: #ms!**compiled accessors**!public! !
!MMTIME categoriesFor: #sample!**compiled accessors**!public! !
!MMTIME categoriesFor: #smtp_fps!**compiled accessors**!public! !
!MMTIME categoriesFor: #smtp_frame!**compiled accessors**!public! !
!MMTIME categoriesFor: #smtp_hour!**compiled accessors**!public! !
!MMTIME categoriesFor: #smtp_min!**compiled accessors**!public! !
!MMTIME categoriesFor: #smtp_sec!**compiled accessors**!public! !
!MMTIME categoriesFor: #songptrpos!**compiled accessors**!public! !
!MMTIME categoriesFor: #ticks!**compiled accessors**!public! !
!MMTIME categoriesFor: #typeName!public! !
!MMTIME categoriesFor: #wType!**compiled accessors**!public! !
!MMTIME categoriesFor: #wType:!**compiled accessors**!public! !

!MMTIME class methodsFor!

byteOffset
	"answer a new instance for holding time data in the byte offset format"

	^ (self new)
		wType: TIME_BYTES;
		yourself.!

defineFields
	"
		self compileDefinition.

		typedef struct mmtime_tag {
			UINT wType; 
			union { 
				DWORD ms; 
				DWORD sample; 
				DWORD cb; 
				DWORD ticks; 
				struct { 
					BYTE hour; 
					BYTE min; 
					BYTE sec; 
					BYTE frame; 
					BYTE fps; 
					BYTE dummy; 
					BYTE pad[2] 
				} smpte; 
				struct { 
					DWORD songptrpos; 
				} midi; 
			} u; 
		} MMTIME;
 
	"

	self
		defineField: #wType type: DWORDField new;

		"union -- ugh!!"
		defineField: #ms type: DWORDField readOnly offset: 4;
		defineField: #sample type: DWORDField readOnly offset: 4;
		defineField: #cb type: DWORDField readOnly offset: 4;
		defineField: #ticks type: DWORDField readOnly offset: 4;
		defineField: #smtp_hour type: BYTEField readOnly offset: 4;
		defineField: #smtp_min type: BYTEField readOnly;
		defineField: #smtp_sec type: BYTEField readOnly;
		defineField: #smtp_frame type: BYTEField readOnly;
		defineField: #smtp_fps type: BYTEField readOnly;
		defineField: #dummy type: BYTEField filler;
		defineField: #pad type: WORDField filler;
		defineField: #songptrpos type: DWORDField readOnly offset: 4;

		yourself.!

midiTicks
	"answer a new instance for holding time as MIDI ticks"

	^ (self new)
		wType: TIME_TICKS;
		yourself.!

midiTime
	"answer a new instance for holding time data in MIDI time format"

	^ (self new)
		wType: TIME_MIDI;
		yourself.!

milliseconds
	"answer a new instance for holding time in milliseconds.
	This is the default"

	^ self new.!

sampleIndex
	"answer a new instance for holding time as a wave sample index"

	^ (self new)
		wType: TIME_SAMPLES;
		yourself.!

smpteTime
	"answer a new instance for holding time data in the SMPTE time format"

	^ (self new)
		wType: TIME_SMPTE;
		yourself.! !
!MMTIME class categoriesFor: #byteOffset!instance creation!public! !
!MMTIME class categoriesFor: #defineFields!initializing!public! !
!MMTIME class categoriesFor: #midiTicks!instance creation!public! !
!MMTIME class categoriesFor: #midiTime!instance creation!public! !
!MMTIME class categoriesFor: #milliseconds!instance creation!public! !
!MMTIME class categoriesFor: #sampleIndex!instance creation!public! !
!MMTIME class categoriesFor: #smpteTime!instance creation!public! !

WAVEFORMATEX guid: (GUID fromString: '{E91A9348-685B-4731-A3BA-0F3821F9583B}')!
WAVEFORMATEX comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

The Windows documentation states:

=========
The WAVEFORMATEX structure defines the format of waveform-audio data. Only format information common to all waveform-audio data formats is included in this structure. For formats that require additional information, this structure is included as the first member in another structure, along with the additional information.
=========

However, we use subclassing instead, so that Dolphin will allow any of the subclasses to be passed to the Windows MM lib.

Note, also, that this is all a bit of a mess (typicallty of MS).  They say that you should use this for everything /except/ for PCM coding, in which case you should use the PCMWAVEFORMAT stucture (which is identical to this in memory layout except that it lacks the final cSize field).  In fact we follow Petzold and use these for PCM too.  Actually, by default we are set up for PCM.


	'!
!WAVEFORMATEX categoriesForClass!Unclassified! !
!WAVEFORMATEX methodsFor!

cbSize
	"Answer the receiver's cbSize field as a Smalltalk object."

	^(bytes wordAtOffset: 16)!

cbSize: anObject
	"Set the receiver's cbSize field to the value of anObject."

	bytes wordAtOffset: 16 put: anObject!

initialize: anInteger
	"private -- establish a coherent intial state"

	super initialize: anInteger.
	self
		cbSize: anInteger;
		wFormatTag: self class defaultFormatTag.!

nAvgBytesPerSec
	"Answer the receiver's nAvgBytesPerSec field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

nAvgBytesPerSec: anObject
	"Set the receiver's nAvgBytesPerSec field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

nBlockAlign
	"Answer the receiver's nBlockAlign field as a Smalltalk object."

	^(bytes wordAtOffset: 12)!

nBlockAlign: anObject
	"Set the receiver's nBlockAlign field to the value of anObject."

	bytes wordAtOffset: 12 put: anObject!

nChannels
	"Answer the receiver's nChannels field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

nChannels: anObject
	"Set the receiver's nChannels field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject!

nSamplesPerSec
	"Answer the receiver's nSamplesPerSec field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

nSamplesPerSec: anObject
	"Set the receiver's nSamplesPerSec field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

wBitsPerSample
	"Answer the receiver's wBitsPerSample field as a Smalltalk object."

	^(bytes wordAtOffset: 14)!

wBitsPerSample: anObject
	"Set the receiver's wBitsPerSample field to the value of anObject."

	bytes wordAtOffset: 14 put: anObject!

wFormatTag
	"Answer the receiver's wFormatTag field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

wFormatTag: anObject
	"Set the receiver's wFormatTag field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject! !
!WAVEFORMATEX categoriesFor: #cbSize!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #cbSize:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #initialize:!initializing!private! !
!WAVEFORMATEX categoriesFor: #nAvgBytesPerSec!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nAvgBytesPerSec:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nBlockAlign!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nBlockAlign:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nChannels!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nChannels:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nSamplesPerSec!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #nSamplesPerSec:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #wBitsPerSample!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #wBitsPerSample:!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #wFormatTag!**compiled accessors**!public! !
!WAVEFORMATEX categoriesFor: #wFormatTag:!**compiled accessors**!public! !

!WAVEFORMATEX class methodsFor!

defaultFormatTag
	"answer the #wFormatTag that instances should use by default"

	^ WAVE_FORMAT_PCM.!

defineFields
	"
		self compileDefinition.

		typedef struct { 
			WORD  wFormatTag; 
			WORD  nChannels; 
			DWORD nSamplesPerSec; 
			DWORD nAvgBytesPerSec; 
			WORD  nBlockAlign; 
			WORD  wBitsPerSample; 
			WORD  cbSize; 
		} WAVEFORMATEX; 
	"

	self
		defineField: #wFormatTag type: WORDField new;
		defineField: #nChannels type: WORDField new;
		defineField: #nSamplesPerSec type: DWORDField new;
		defineField: #nAvgBytesPerSec type: DWORDField new;
		defineField: #nBlockAlign type: WORDField new;
		defineField: #wBitsPerSample type: WORDField new;
		defineField: #cbSize type: WORDField new;
		yourself.!

mono
	"Answer a new instance which is set up for:
		PCM
		1 channel
		16 bits per sample
		44.1 kHz
	"

	^ self mono: 16.
		!

mono: anInteger
	"Answer a new instance which is set up for:
		PCM
		1 channel
		anInteger bits per sample
		44.1 kHz
	"
	^ self mono: anInteger samplesPerSecond: 44010.
!

mono: anInteger samplesPerSecond: aBiggerInteger
	"Answer a new instance which is set up for:
		PCM
		1 channel
		anInteger bits-per-sample
		aBiggerInteger samples-per-second

	Common values for samples-per-second are:
			11025
			22050
			44100
			48000
			96000
	"

	| blockAlign averageBPS |

	blockAlign := anInteger / 8.
	averageBPS := aBiggerInteger * blockAlign.

	^ (self new)	"defaults to PCM"
		nChannels: 1;
		nSamplesPerSec: aBiggerInteger;
		nAvgBytesPerSec: averageBPS;
		nBlockAlign: blockAlign;
		wBitsPerSample: anInteger;
		yourself.
!

stereo
	"Answer a new instance which is set up for:
		PCM
		2 channels
		16 bits per sample
		44.1 kHz
	"
	^ self stereo: 16.
!

stereo: anInteger
	"Answer a new instance which is set up for:
		PCM
		2 channels
		anInteger bits per sample
		44.1 kHz
	"
	^ self stereo: anInteger samplesPerSecond: 44010.
!

stereo: anInteger samplesPerSecond: aBiggerInteger
	"Answer a new instance which is set up for:
		PCM
		1 channel
		anInteger bits-per-sample
		aBiggerInteger samples-per-second

	Common values for samples-per-second are:
			11025
			22050
			44100
			48000
			96000
	"

	| blockAlign averageBPS |

	blockAlign := anInteger / 4.
	averageBPS := aBiggerInteger * blockAlign.

	^ (self new)	"defaults to PCM"
		nChannels: 2;
		nSamplesPerSec: aBiggerInteger;
		nAvgBytesPerSec: averageBPS;
		nBlockAlign: blockAlign;
		wBitsPerSample: anInteger;
		yourself.
! !
!WAVEFORMATEX class categoriesFor: #defaultFormatTag!constants!public! !
!WAVEFORMATEX class categoriesFor: #defineFields!initializing!public! !
!WAVEFORMATEX class categoriesFor: #mono!instance creation!public! !
!WAVEFORMATEX class categoriesFor: #mono:!instance creation!public! !
!WAVEFORMATEX class categoriesFor: #mono:samplesPerSecond:!instance creation!public! !
!WAVEFORMATEX class categoriesFor: #stereo!instance creation!public! !
!WAVEFORMATEX class categoriesFor: #stereo:!instance creation!public! !
!WAVEFORMATEX class categoriesFor: #stereo:samplesPerSecond:!instance creation!public! !

WAVEHDR guid: (GUID fromString: '{CE9D1944-0941-42D1-B455-2F9416CC5029}')!
WAVEHDR comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these describes and holds (via its #data) a buffer of wave data for use with a WaveOuputDevice.'!
!WAVEHDR categoriesForClass!Unclassified! !
!WAVEHDR methodsFor!

beFirstInLoop
	"set the dwFlags to mark us as the first in a loop.
	Note: must be called /after/ we are prepared"

	self dwFlags: (self dwFlags bitOr: WHDR_BEGINLOOP).!

beLastInLoop
	"set the dwFlags to mark us as the last in a loop.
	Note: must be called /after/ we are prepared"

	self dwFlags: (self dwFlags bitOr: WHDR_ENDLOOP).!

data
	"answer our data buffer"

	^ data.!

data: aByteArrayOrSimilar
	"set our data buffer to that given.  This
	should be used in preference to #lpData: and/or #dwBufferLength: since it
	will automatically protect the buffer against GC, and also will keep the buffer
	and size settings consistant (and it's more readable ;-)"

	data := aByteArrayOrSimilar.
	self
		lpData: data yourAddress;
		dwBufferLength: data byteSize.!

dwBufferLength
	"Answer the receiver's dwBufferLength field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

dwBufferLength: anObject
	"Set the receiver's dwBufferLength field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

dwBytesRecorded
	"Answer the receiver's dwBytesRecorded field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dwBytesRecorded: anObject
	"Set the receiver's dwBytesRecorded field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

dwFlags
	"Answer the receiver's dwFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

dwFlags: anObject
	"Set the receiver's dwFlags field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

dwLoops
	"Answer the receiver's dwLoops field as a Smalltalk object."

	^(bytes dwordAtOffset: 20)!

dwLoops: anObject
	"Set the receiver's dwLoops field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

dwUser
	"Answer the receiver's dwUser field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

dwUser: anObject
	"Set the receiver's dwUser field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

isFirstInLoop
	"answer whether the dwFlags to mark us as the first in a loop"

	^ self dwFlags allMask: WHDR_BEGINLOOP.!

isInQueue
	"answer whether the dwFlags to mark us as being in the underlying driver's queue"

	^ self dwFlags allMask: WHDR_INQUEUE.!

isLastInLoop
	"answer whether the dwFlags to mark us as the last in a loop"

	^ self dwFlags allMask: WHDR_ENDLOOP.!

isPrepared
	"answer whether the dwFlags to mark us as having been prepared"

	^ self dwFlags allMask: WHDR_PREPARED.!

loops
	"answer how many times we have been asked to play"

	^ self dwLoops!

loops: anInteger
	"mark this buffer as wanting to be looped anInteger number of times.
	NB: the API seems to allow you to set different start and end buffers, but I
	haven't been able to make that work"

	self
		dwLoops: anInteger;
		beFirstInLoop;
		beLastInLoop!

lpData
	"Answer the receiver's lpData field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalAddress!

lpData: anObject
	"Set the receiver's lpData field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject! !
!WAVEHDR categoriesFor: #beFirstInLoop!accessing!public! !
!WAVEHDR categoriesFor: #beLastInLoop!accessing!public! !
!WAVEHDR categoriesFor: #data!accessing!public! !
!WAVEHDR categoriesFor: #data:!accessing!public! !
!WAVEHDR categoriesFor: #dwBufferLength!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwBufferLength:!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwBytesRecorded!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwBytesRecorded:!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwFlags!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwFlags:!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwLoops!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwLoops:!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwUser!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #dwUser:!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #isFirstInLoop!public!testing! !
!WAVEHDR categoriesFor: #isInQueue!public!testing! !
!WAVEHDR categoriesFor: #isLastInLoop!public!testing! !
!WAVEHDR categoriesFor: #isPrepared!public!testing! !
!WAVEHDR categoriesFor: #loops!accessing!public! !
!WAVEHDR categoriesFor: #loops:!accessing!public! !
!WAVEHDR categoriesFor: #lpData!**compiled accessors**!public! !
!WAVEHDR categoriesFor: #lpData:!**compiled accessors**!public! !

!WAVEHDR class methodsFor!

defineFields
	"
		self compileDefinition.

		typedef struct { 
			LPSTR		lpData; 
			DWORD		dwBufferLength; 
			DWORD		dwBytesRecorded; 
			DWORD_PTR	dwUser; 
 			DWORD		dwFlags; 
			DWORD		dwLoops; 
			struct wavehdr_tag	*lpNext; 
			DWORD_PTR	reserved; 
		} WAVEHDR; 
	"

	self
		defineField: #lpData type: LPVOIDField new;
		defineField: #dwBufferLength type: DWORDField new;
		defineField: #dwBytesRecorded type: DWORDField new;
		defineField: #dwUser type: DWORDField new;
		defineField: #dwFlags type: DWORDField new;
		defineField: #dwLoops type: DWORDField new;
		defineField: #lpNext type: DWORDField filler;
		defineField: #reserved type: DWORDField filler;
		yourself.!

withBytes: anInteger
	"answer a new instance that holds a data buffer that is a ByteArray of the given size"

	^ self withData: (ByteArray newFixed: anInteger).!

withData: aByteArrayOrSimilar
	"answer a new instance that holds the given data buffer"

	^ (self new)
		data: aByteArrayOrSimilar;
		yourself.!

withWords: anInteger
	"answer a new instance that holds a data buffer that is a SWORDArray of the given size"

	^ self withData: (SWORDArray new: anInteger).! !
!WAVEHDR class categoriesFor: #defineFields!initializing!public! !
!WAVEHDR class categoriesFor: #withBytes:!instance creation!public! !
!WAVEHDR class categoriesFor: #withData:!instance creation!public! !
!WAVEHDR class categoriesFor: #withWords:!instance creation!public! !

WAVEOUTCAPS guid: (GUID fromString: '{82D67301-1DF0-4B2E-8F66-EF92AF8FF27D}')!
WAVEOUTCAPS comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

One of these describes the capabilities of a waveform-audio output device.'!
!WAVEOUTCAPS categoriesForClass!Unclassified! !
!WAVEOUTCAPS methodsFor!

canGetAccuratePosition
	"answer whether we indicate that the device can provide sample-accurate position information"

	^ self dwSupport allMask: WAVECAPS_SAMPLEACCURATE.!

dwFormats
	"Answer the receiver's dwFormats field as a Smalltalk object."

	^(bytes dwordAtOffset: 40)!

dwSupport
	"Answer the receiver's dwSupport field as a Smalltalk object."

	^(bytes dwordAtOffset: 52)!

formatNames
	"answer an Array of the Symbol names of the formats we support"

	^ formatNames ifNil: [formatNames := self class namesOfFormats: self dwFormats].
!

isPlaybackSynchronous
	"answer whether we indicate that the device will block whilst playing a buffer"

	^ self dwSupport allMask: WAVECAPS_SYNC.!

supportsChangeChannelVolumes
	"answer whether we indicate that the device can changing playback volume for each channel separately.
	NB: take with a grain of salt"

	^ self dwSupport allMask: WAVECAPS_LRVOLUME.!

supportsChangePitch
	"answer whether we indicate that the device can changing playback pitch"

	^ self dwSupport allMask: WAVECAPS_PITCH.!

supportsChangePlaybackRate
	"answer whether we indicate that the device can changing playback rate"

	^ self dwSupport allMask: WAVECAPS_PLAYBACKRATE.!

supportsChangeVolume
	"answer whether we indicate that the device can changing playback volume.
	NB: take with a grain of salt"

	^ self dwSupport allMask: WAVECAPS_VOLUME.!

szPname
	"Answer the receiver's szPname field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 8)!

vDriverVersion
	"Answer the receiver's vDriverVersion field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

wChannels
	"Answer the receiver's wChannels field as a Smalltalk object."

	^(bytes wordAtOffset: 44)!

wMid
	"Answer the receiver's wMid field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

wPid
	"Answer the receiver's wPid field as a Smalltalk object."

	^(bytes wordAtOffset: 2)! !
!WAVEOUTCAPS categoriesFor: #canGetAccuratePosition!public!testing! !
!WAVEOUTCAPS categoriesFor: #dwFormats!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #dwSupport!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #formatNames!public! !
!WAVEOUTCAPS categoriesFor: #isPlaybackSynchronous!public!testing! !
!WAVEOUTCAPS categoriesFor: #supportsChangeChannelVolumes!public!testing! !
!WAVEOUTCAPS categoriesFor: #supportsChangePitch!public!testing! !
!WAVEOUTCAPS categoriesFor: #supportsChangePlaybackRate!public!testing! !
!WAVEOUTCAPS categoriesFor: #supportsChangeVolume!public!testing! !
!WAVEOUTCAPS categoriesFor: #szPname!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #vDriverVersion!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #wChannels!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #wMid!**compiled accessors**!public! !
!WAVEOUTCAPS categoriesFor: #wPid!**compiled accessors**!public! !

!WAVEOUTCAPS class methodsFor!

defineFields
	"
		self compileDefinition.

			typedef struct { 
				WORD	wMid; 
				WORD	wPid; 
				MMVERSION	vDriverVersion; 
				TCHAR	szPname[MAXPNAMELEN]; 
				DWORD	dwFormats; 
				WORD	wChannels; 
				WORD	wReserved1; 
				DWORD	dwSupport; 
			} WAVEOUTCAPS; 
	"

	self
		defineField: #wMid type: WORDField readOnly;
		defineField: #wPid type: WORDField readOnly;
		defineField: #vDriverVersion type: DWORDField readOnly;
		defineField: #szPname type: (StringField length: 32) beReadOnly;
		defineField: #dwFormats type: DWORDField readOnly;
		defineField: #wChannels type: WORDField readOnly;
		defineField: #WORD type: DWORDField filler;
		defineField: #dwSupport type: DWORDField readOnly;
		yourself.!

namesOfFormats: anInteger
	"answer an Array of the Symbol names of the formats expressed as a bitmap in anInteger"

	^ #(
			"mono 8	mono 16	stereo 8		stereo 16"
"11.025 kHz"		#'1M08'	#'1M16'	#'1S08'		#'1S16'
"22.05 kHz"		#'2M08'	#'2M16'	#'2S08'		#'2S16'
"44.1 kHz"		#'4M08'	#'4M16'	#'4S08'		#'4S16'
"48 kHz"		#'48M08'	#'48M16'	#'48S08'	#'48S16'
"96 kHz"		#'96M08'	#'96M16'	#'96S08'	#'96S16'
	) select:
		[:each || full mask |
		full := 'WAVE_FORMAT_' , each.
		mask := Win32Constants at: full asSymbol ifAbsent: [-1].
		anInteger allMask: mask].! !
!WAVEOUTCAPS class categoriesFor: #defineFields!initializing!public! !
!WAVEOUTCAPS class categoriesFor: #namesOfFormats:!helpers!public! !

WaveOutHelperView guid: (GUID fromString: '{A8CB15BF-9487-48FB-AF3F-308B4D493569}')!
WaveOutHelperView comment: 'Copyright © Chris Uppal, 2005.
chris.uppal@metagnostic.org

The /only/ purpose of these things is to provide windows that a WaveOuputPlayer can nominate to receive progress / state change messages.

All we do is receive one of three messages and send them back to our owning player.'!
!WaveOutHelperView categoriesForClass!Unclassified! !
!WaveOutHelperView methodsFor!

mmWomClose: message wParam: wParam lParam: lParam
	"handler for a MM_WOM_CLOSE message"

	player onWaveOutClose.

	^ nil.!

mmWomDone: message wParam: wParam lParam: lParam
	"handler for a MM_WOM_DONE message."

	player onWaveOutDone:lParam.

	^ nil.!

mmWomOpen: message wParam: wParam lParam: lParam
	"handler for a MM_WOM_OPEN message."

	player onWaveOutOpen.

	^ nil.!

player: aWaveOutPlayer
	"private -- set the player that owns us and to which we will forward message"

	player := aWaveOutPlayer.! !
!WaveOutHelperView categoriesFor: #mmWomClose:wParam:lParam:!event handling-win32!private! !
!WaveOutHelperView categoriesFor: #mmWomDone:wParam:lParam:!event handling-win32!private! !
!WaveOutHelperView categoriesFor: #mmWomOpen:wParam:lParam:!event handling-win32!private! !
!WaveOutHelperView categoriesFor: #player:!private! !

!WaveOutHelperView class methodsFor!

initialize
	"private -- class-side initialisation

		self initialize.
	"

	self assert: [(MessageMap at: 1 + MM_WOM_OPEN) isNil].
	self assert: [(MessageMap at: 1 + MM_WOM_CLOSE) isNil].
	self assert: [(MessageMap at: 1 + MM_WOM_DONE) isNil].

	MessageMap
		at: 1 + MM_WOM_OPEN put: #mmWomOpen:wParam:lParam:;
		at: 1 + MM_WOM_CLOSE put: #mmWomClose:wParam:lParam:;
		at: 1 + MM_WOM_DONE put: #mmWomDone:wParam:lParam:;
		yourself.!

uninitialize
	"private -- class-side tear-down.

		self uninitialize.
	"

	MessageMap
		at: 1 + MM_WOM_OPEN put: nil;
		at: 1 + MM_WOM_CLOSE put: nil;
		at: 1 + MM_WOM_DONE put:nil;
		yourself.
! !
!WaveOutHelperView class categoriesFor: #initialize!initializing!private! !
!WaveOutHelperView class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

