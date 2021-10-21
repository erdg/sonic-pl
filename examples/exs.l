# examples from the sonic pi docs, plus some randoms
# for test coverage

# usage
#
#  (ex-whatever)   -- send to sonic pi
#  (ex-whatever T) -- debug output
#  (ex-whatever !) -- debug output piped to ruby formatter

# should play a lame industrial beat in c minor
(sp ex-lame-industrial
   (live-loop :notz do
      (play (chord :c3 :minor7).choose)
      (sleep 0.5) )
   (live-loop :padz do
      (sync :notz)
      (synth :prophet notes: (chord :c4 :minor7) release: 3)
      (sleep 4) )
   (live-loop :bazz do
      (sync :notz)
      (synth :tb303 note: :c2 release: 3 amp: 0.4)
      (sleep 4) )
   (live-loop :bd do
      (sync :notz)
      (sample :bd_haus)
      (sleep 1) ) )


(sp ex-filtered-dnb
   (use-sample-bpm :loop-amen)
   (with-fx :rlpf  cutoff: 10 cutoff-slide: 4  do |c|
      (live-loop :dnb do
         (sample :bass-dnb-f amp: 5)
         (sample :loop-amen amp: 5)
         (sleep 1)
         (control c cutoff: (rrand 40 120) cutoff-slide: (rrand 1 4)) ) ) )


(sp ex-basic-variable
   (= s (synth :prophet note: 60 release: 5 cutoff: 60))
   (sleep 1)
   (control s cutoff: 80)
   (sleep 1)
   (control s cutoff: 100) )


(sp ex-basic-functions
   (define :ratio do |note x y|
      (play (hz-to-midi (midi-to-hz note) * x / y)) )
   (ratio :c4 1 1)
   (ratio :c4 5 4)
   (ratio :c4 3 2)
   (ratio :c4 7 4)
   (sleep 1)
   (define :just-7th-chord do |note|
      (ratio note 1 1)
      (ratio note 5 4)
      (ratio note 3 2)
      (ratio note 7 4) )
   (just-7th-chord :d4) )


(sp ex-ambient-experiment
   (use-synth :hollow)
   (with-fx :reverb mix: 0.7 do
      (live-loop :note1 do
         (play [:D4 :E4].choose attack: 6 release: 6)
         (sleep 8) )
      (live-loop :note2 do
         (play [:Fs4 :G4].choose attack: 4 release: 5)
         (sleep 8) )
      (live-loop :note3 do
         (play [:A4 :Cs5].choose attack: 5 release: 5)
         (sleep 8) ) ) )


(sp ex-jungle
   (with-fx :lpf  cutoff: 90  do
      (with-fx :reverb  mix: 0.5  do
         (with-fx :compressor  pre-amp: 20  do
            (with-fx :distortion  distort: 0.4  do
               (live-loop :jungle do
                  (use-random-seed 667)
                  (4.times do
                     (sample :loop-amen
                        beat-stretch: 1
                        rate: [1 1 1 -1].choose / 2
                        finish: 0.5
                        amp: 0.1 )
                     (sample :loop-amen
                        beat-stretch: 1 )
                     (sleep 1) ) ) ) ) ) ) )


(sp ex-fm-noise
   (use-synth :fm)
   (live-loop :sci-fi do
      (= p
         (play (chord :Eb3 :minor).choose
            divisor: "0.01"
            div-slide: (rrand 0 10)
            depth: (rrand "0.001" 2)
            attack: "0.01"
            release: (rrand 0 5)
            amp: 0.5 ) )
      (control p divisor: (rrand "0.001" 50))
      (sleep [0.5 1 2].choose) ) )


(sp ex-reich-phase
   (= reich-notes (ring :E4 :Fs4 :B4))
   (live-loop :slower do
      (play reich-notes.tick release: 0.1)
      (sleep 0.3) )
   (live-loop :faster do
      (play reich-notes.tick release: 0.1)
      (sleep 0.296) ) )

(sp ex-echo-drama
   (use-synth :tb303)
   (use-bpm 45)
   (use-random-seed 3)
   (use-debug false)
   (with-fx :reverb do
      (with-fx :echo  delay: 0.5 decay: 0.4  do
         (live-loop :echoes do
            (play (chord [:b1 :b2 :e1 :e2 :b3 :e3].choose :minor).choose
               cutoff: (rrand 40 100)
               amp: 0.5
               attack: 0
               release: (rrand 1 2)
               cutoff-max: 120 )
            (sleep [0.25 0.5 0.5 0.5 1 1].choose) ) ) ) )


(sp ex-tron-bike
   (use-random-seed 10)
   (= notes (ring :b1 :b2 :e1 :e2 :b3 :e3))
   (live-loop :tron do
      (with-synth :dsaw do
         (with-fx :slicer phase: [0.25 0.125].choose do
            (with-fx :reverb room: 0.5 mix: 0.3 do
               (= n1 (chord notes.choose :minor).choose)
               (= n2 (chord notes.choose :minor).choose)
               (= p (play n1 amp: 2 release: 8 note-slide: 4 cutoff: 30 cutoff-slide: 4 detune: (rrand 0 0.2)))
               (control p note: n2 cutoff: (rrand 80 120)) ) ) )
      (sleep 8) ) )


(sp ex-turbo-test
   (play 60)
   (sleep 1)
   (play 64) )


(sp ex-acid

   (use-debug false)
   (load-sample :bd-fat)

   (8.times do
      (sample :bd-fat  amp: (line 0 5 steps: 8).tick)
      (sleep 0.5) )

   (live-loop :drums do
      (sample :bd-fat amp: 5)
      (sleep 0.5) )

   (live-loop :acid do
      (cue :foo)
      (4.times do |i|
         (use-random-seed 667)
         (16.times do
            (use-synth :tb303)
            (play (chord :e3 :minor).choose
               attack: 0
               release: 0.1
               cutoff: (rrand-i 50 90) + i * 10 )
            (sleep 0.125) ) )

      (cue :bar)
      (32.times do |i|
         (use-synth :tb303)
         (play (chord :a3 :minor).choose
            attack: 0
            release: 0.1
            cutoff: (rrand-i 70 98) + i
            res: (rrand 0.9 0.95) )
         (sleep 0.125) )

      (cue :baz)
      (with-fx :reverb  mix: 0.3  do |r|
         (32.times do |m|
            (control r  mix: (rrand 0.2 0.8))
            (use-synth :prophet)
            (play (chord :e :minor).choose
               attack: 0
               release: 0.08
               cutoff: (rrand-i 110 130) )
            (sleep 0.125) ) )

      (cue :quux)
      (in-thread do
         (use-random-seed 668)
         (with-fx :echo  phase: 0.125  do
            (16.times do
               (use-synth :tb303)
               (play (chord :a :minor).choose
                  attack: 0
                  release: 0.1
                  cutoff: (rrand 50 100))
               (sleep 0.25) ) ) )

      (sleep 4) )
)


(sp ex-ambient

   (load-samples (sample-names :ambi))
   (sleep 2)

   (with-fx :reverb mix: 0.8 do
     (live-loop :foo do
       # try changing the sp- vars..
       (= sp-name (choose (sample_names :ambi)))
       # (= sp-name (choose (sample_names :drum)))
       (= sp-time [1 2].choose)
       # (= sp-time 0.5)
       (= sp-rate 1)
       # (= sp-rate 4)

       (= s (sample sp-name
               amp: 2
               cutoff: (rrand 70 130)
               rate: sp-rate * (choose [0.5 1])
               pan: (rrand -1 1)
               pan-slide: sp-time ) )
       (control s pan: (rrand 1 1))
       (sleep sp-time) ) ) )


(sp ex-idm-bb
   (live-loop :idm-bb do
      (= n [1 2 4 8 16].choose)
      (sample :drum-heavy-kick  amp: 2)
      (sample :ambi-drone
         rate: [0.25 0.5 0.125 1].choose
         amp: 0.25 if (one-in 8) )
      (sample :ambi-lunar-land
         rate: [0.5 0.125 1 -1 -0.5].choose
         amp: 0.25 if (one-in 8) )
      (sample :loop-amen
         attack: 0
         release: 0.05
         start: 1 - 1.0 / n
         rate: [1 1 1 1 1 1 -1].choose )
      (sleep (sample-duration :loop-amen) / n) ) )


(sp ex-chord-inversions
   ([1 3 6 4].each do |d|
      ((range -3 3).each do |i|
         (play-chord (chord-degree d :c :major 3 invert: i))
         (sleep 0.25) ) ) )

(sp ex-nested-iterations
   (4.times do
      (sample :drum-heavy-kick)
      (2.times do
         (sample :elec-blip2 rate: 2)
         (sleep 0.25) )
      (sample :elec-snare)
      (4.times do
         (sample :drum-tom-mid-soft)
         (sleep 0.125) ) ) )

(sp ex-basic-function-params

   (define :chord-player do |root repeats|
      (repeats.times do
         (play (chord root :minor) release: 3)
         (sleep 0.5) ) )

   (chord-player :e3 2)
   (sleep 0.5)
   (chord-player :a3 3)
   (chord-player :g3 4) )

(sp ex-cue-names
   (in-thread do
      (loop do
         (cue [:foo :bar :baz].choose)
         (sleep 0.5) ) )
   (in-thread do
      (loop do
         (sync :foo)
         (sample :elec-beep) ) )
   (in-thread do
      (loop do
         (sync :bar)
         (sample :elec-blip) ) )
   (in-thread do
      (loop do
         (sync :baz)
         (sample :elec-blip2) ) ) )


(sp ex-rand-surfer
   (live-loop :rand-surfer do
      (use-random-seed 1)
      (use-synth :dsaw)
      (= notes (scale :e2 :minor-pentatonic num-octaves: 2))
      (16.times do
         (play notes.choose release: 0.1 cutoff: (rrand 70 120))
         (sleep 0.125) ) ) )


(sp ex-looper-trooper
   (with-fx :reverb room: 1 do
      (live-loop :time do
         (synth :prophet release: 8 note: :e1 cutoff: 90 amp: 3)
         (sleep 8) ) )
   (live-loop :machine do
      (sample :loop-garzul rate: 0.5 finish: 0.25)
      (sample :loop-garzul beat-stretch: 4 amp: 1)
      (sleep 4) )
   (live-loop :kik do
      (sample :bd-haus amp: 2)
      (sleep 0.5) )
   (with-fx :echo do
      (live-loop :vortex do
         (= notes (scale :e3 :minor-pentatonic num-octaves: 3))
         (16.times do
            (play notes.choose release: 0.1 amp: 0.5)
            (sleep 0.125) ) ) ) )


(sp ex-magpi-coded-beats
   (use-bpm 100)
   (live-loop :amen-break do
      (= p [0.125 0.25 0.5].choose)
      (with-fx :slicer phase: p wave: 0 mix: (rrand 0.7 1) do
         (= r [1 1 1 -1].choose)
         (sample :loop-amen beat-stretch: 2 rate: r amp: 2) )
      (sleep 2) )
   (live-loop :bass-drum do
      (sample :bd-haus cutoff: 70 amp: 1.5)
      (sleep 0.5) )
   (live-loop :landing do
      (= bass-line (knit :e1 3 [:c1 :c2].choose 1))
      (with-fx :slicer phase: [0.25 0.5].choose invert-wave: 1 wave: 0 do
         (= s (synth :square note: bass-line.tick sustain: 4 cutoff: 60))
         (control s cutoff-slide: 4 cutoff: 120)
         (sleep 4) ) ) )


[sp ex-monday-blues
   (use-debug false)
   (load-samples [:drum-heavy-kick :drum-snare-soft])
   (live-loop :drums do
      # (puts "slow drums")
      (6.times do
         (sample :drum-heavy-kick rate: 0.8)
         (sleep 0.5) )
      # (puts "fast drums")
      (8.times do
         (sample :drum-heavy-kick rate: 0.8)
         (sleep 0.125) ) )
   (live-loop :synths do
      (use-synth :mod-saw)
      (use-synth-defaults
         amp: 0.5
         attack: 0
         sustain: 1
         release: 0.25
         mod-range: 12
         mod-phase: 0.5
         mod-invert-wave: 1 )
      (= notes
         (ring :F :C :D :D :G :D :D) )
      (notes.each do |n|
         (tick)
         (play (note n octave: 1) cutoff: (line 90 130 steps: 32).look) )
      (sleep 1) )
   (live-loop :snr delay: 12.5 do
      (sample :drum-snare-soft )
      (sleep 1) )
]


# this works, but something sounds ever so slightly different.
# there's some kind of resonant hum that seems to grow over
# time and occasionally clash with the chord changes. I don't
# notice it when loading the actual example in sonic pi

[sp ex-cloud-beat
   (use-bpm 100)

   # HISS
   (live-loop :hiss-loop do
      (sample :vinyl-hiss amp: 2)
      (sleep (sample-duration :vinyl-hiss)) )

   (define :hihat do
      (use-synth :pnoise)
      (with-fx :hpf cutoff: 120 do
         (play release: "0.01" amp: 13) ) )


   (live-loop :hihat-loop do
      (= divisors (ring 2 4 2 2 2 2 2 6))
      (divisors.tick.times do
         (hihat)
         (sleep 1.0 / divisors.look) ) )

   (live-loop :snare-loop do
      (sleep (ring 2.5 3).tick)
      (with-fx :lpf cutoff: 100 do
         (sample :sn-dub sustain: 0 release: 0.05 amp: 3) )
      (sleep (ring 1.5 1).look) )

   (define :bassdrum do |note1 duration note2 = note1|
      (use-synth :sine)
      (with-fx :hpf cutoff: 100 do
         (play note1 + 24  amp: 40 release: 0.01) )
      (with-fx :distortion distort: 0.1 mix: 0.3 do
         (with-fx :lpf cutoff: 26 do
            (with-fx :hpf cutoff: 55 do
               (= bass (play note1 amp: 85 release: duration note-slide: duration))
               (control bass note: note2) ) ) )
      (sleep duration) )

   # UGLY 'if', but it works
   (live-loop :bass-loop do
      (bassdrum 36 1.5)
      (if (bools 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0).tick
         then
            (bassdrum 36 0.5 40);   # semi-colons are gross but necessary for now
            (bassdrum 38 1 10);
         else
            (bassdrum 36 1.5) )
      (bassdrum 36 1.0 (ring 10 10 10 40).look) )

   (= chord1 (chord :c4  :maj9 num-octaves: 2))
   (= chord2 (chord :es4 :maj9 num-octaves: 2))
   (= chord3 (chord :b3  :maj9 num-octaves: 2))
   (= chord4 (chord :d4  :maj9 num-octaves: 2))

   (= chord-low1 (chord :c2  :maj9))
   (= chord-low2 (chord :es2 :maj9))
   (= chord-low3 (chord :b1  :maj9))
   (= chord-low4 (chord :d2  :maj9))

   (= chord-high chord1)
   (= chord-low  chord-low1)

   (live-loop :chord-selector delay: -0.5 do
      (= chord-high (knit chord1 2 chord2 2 chord3 4 chord4 4).tick)
      (= chord-low  (knit chord-low1 2 chord-low2 2 chord-low3 4 chord-low4 4).look)
      (sleep 8) )

   (define :chord-player do |the-chord|
      (use-synth :blade)
      (the-chord.each do |note|
         (play note
            attack: (rand 4)
            release: (rand 6..8)
            cutoff: (rand 50..85)
            vibrato-rate: (rand 0.01..2)
            amp: 0.55 ) ) )

   (with-fx :reverb room: 0.99 mix: 0.7 do
      (live-loop :chord-loop do
         (chord-player chord-high.pick(6))
         (chord-player chord-low.take(3))
         (sleep 8) ) )

]

# https://in-thread.sonic-pi.net/t/new-to-sonic-pi-so-every-little-helps/3312/5
[sp ex-trap-hats
   (use-bpm 86)
   (define :hats do |d|
      (density d do
         (sample :drum-cymbal-closed)
         (sleep 1) ) )
   (live-loop :trap-hats do
      (hats (ring 4 4 3 8 4 6 4 16).tick) )
   (= n [1 0 1 0 2 0 0 0 1 0 0 1 2 0 1 0])
   (live-loop :trap-beat do
      (n.each-with-index do |i|
         (sample :bd-haus if i == 1)
         (sample :drum-snare-hard if i == 2)
         (sleep 0.25) ) )
]

[sp ex-density-bass
   (use-bpm 96)
   (load-samples [:elec-tick :bd-gas])
   (live-loop :density-bass do
      (density [2 2 2 3].choose do
         (sample :bd-gas amp: 3)
         (sleep 1) ) )
   (live-loop :hats do
      (density (ring 4 3 2 4 2 8 4 16).tick do
         (sample :elec-tick)
         (sleep 1) ) )
]


# https://in-thread.sonic-pi.net/t/stutter-beat-repeat/513/14
[sp ex-stutter-beat
   (define :pl do |s p = 0|
      (sample s, pan: p)
      (sleep 1) )
   (define :chos do
      (return [:bd-haus :bd-sone :bd-klub :drum-soft-snare :drum-cymbal-closed].choose ) )
   (live-loop :stutter do
      (use-bpm 120)
      (= s chos)
      (pl chos -1)
      (density [2 3 4].choose do
         (sample chos pan: -1)
         (sleep 1) )
      (density [2 3 4].choose do
         (sample chos pan: -0.5)
         (sleep 1) )
      (pl chos -0.25) )
   (live-loop :stutter2 do
         (use-bpm 120)
         (= s chos)
         (pl chos 1)
         (pl chos 1)
         (density [2 3 4].choose do
            (sample chos pan: 1)
            (sleep 1) )
         (density [2 3 4].choose do
            (sample chos pan: 0.5)
            (sleep 1) )
         (pl chos 0.25) )
]

# to be continued
[sp ex-aphex-twin-beat
   (use-bpm "280")
   (with-fx :reverb  mix: 0.1 room: 0.6  do |reverb|
      (with-fx :echo  phase: 0.75 mix: 0.1 decay: 5.0  do |echo|
         (live-loop "main" do
            (if (tick :fx-start) == 0 then
               (set :reverb reverb)
               (set :echo echo) )
            (control (get :reverb))
            (control (get :echo))
            (cue "bar")
            (sleep 4) )
         (live-loop "hihats" do
            (sync "/cue/bar")
            (= a 0.3)
            (= pedal [1 1 1 2].ring.tick(:pedal))
            (in-thread do
               (= accent [1.0 0.4 0.4 0.4].ring)
               (4.times do
                  (sample :drum-cybal-closed amp: a*accent.tick)
                  (sleep 1.0)
                  (in-thread do
                     (= n (dice 8) - 3)
                     (n.time do
                        (sample :drum-cymbal-closed amp: a)
                        (sleep 1.0 / 4) ) ) ) ) )
         ]
