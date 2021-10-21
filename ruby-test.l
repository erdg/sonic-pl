(

   (3.times do
      (play 60)
      (sleep 1) )

   ([60 62 64].each do |n|
      (play n)
      (sleep 1) )

   (live-loop :notes do
      (play (chord :c3 :minor7).choose)
      (sleep 0.5) )

   (live-loop :pad do
      (sync :notes)
      (synth :prophet notes: (chord :c4 :minor7) release: 3)
      (sleep 4) )

   (live-loop :bass do
      (sync :notes)
      (synth :tb303 note: :c2 release: 3 amp: 0.4)
      (sleep 4) )

   (live-loop :bd do
      (sync :notes)
      (sample :bd-haus)
      (sleep 1) )

   (use-sample-bpm :loop-amen)
   (with-fx :rlpf  cutoff: 10 cutoff-slide: 4  do |c|
      (live-loop :dnb do
         (sample :bass-dnb-f amp: 5)
         (sample :loop-amen amp: 5)
         (sleep 1)
         (control c cutoff: (rrand 40 120) cutoff-slide: (rrand 1 4)) ) )

   (= s (synth :prophet note: 60 release: 5 cutoff: 60))
   (sleep 1)
   (control s cutoff: 80)
   (sleep 1)
   (control s cutoff: 100)

   (define :ratio do |note x y|
      (play (hz-to-midi (midi-to-hz note) * x / y)) )
   (ratio :c4 1 1)
   (ratio :c4 5 4)
   (ratio :c4 3 2)
   (ratio :c4 7 4)


   (define :just-7th-chord do |note|
      (ratio note 1 1)
      (ratio note 5 4)
      (ratio note 3 2)
      (ratio note 7 4) )
   (just-7th-chord :d4)


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
         (sleep 8) ) )


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
                     (sleep 1) ) ) ) ) ) )


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
      (sleep [0.5 1 2].choose) )


   (= reich-notes (ring :E4 :Fs4 :B4))
   (live-loop :slower do
      (play reich-notes.tick release: 0.1)
      (sleep 0.3) )
   (live-loop :faster do
      (play reich-notes.tick release: 0.1)
      (sleep 0.296) )

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
            (sleep [0.25 0.5 0.5 0.5 1 1].choose) ) ) )


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
      (sleep 8) )


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
      (sleep (sample-duration :loop-amen) / n) )


   ([1 3 6 4].each do |d|
      ((range -3 3).each do |i|
         (play-chord (chord-degree d :c :major 3 invert: i))
         (sleep 0.25) ) )

   (4.times do
      (sample :drum-heavy-kick)
      (2.times do
         (sample :elec-blip2 rate: 2)
         (sleep 0.25) )
      (sample :elec-snare)
      (4.times do
         (sample :drum-tom-mid-soft)
         (sleep 0.125) ) )


   (define :chord-player do |root repeats|
      (repeats.times do
         (play (chord root :minor) release: 3)
         (sleep 0.5) ) )

   (chord-player :e3 2)
   (sleep 0.5)
   (chord-player :a3 3)
   (chord-player :g3 4)

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
         (sample :elec-blip2) ) )


   (live-loop :rand-surfer do
      (use-random-seed 1)
      (use-synth :dsaw)
      (= notes (scale :e2 :minor-pentatonic num-octaves: 2))
      (16.times do
         (play notes.choose release: 0.1 cutoff: (rrand 70 120))
         (sleep 0.125) ) )


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
            (sleep 0.125) ) ) )


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
         (sleep 4) ) )


   (use-debug false)
   (load-samples [:drum-heavy-kick :drum-snare-soft])
   (live-loop :drums do
      (6.times do
         (sample :drum-heavy-kick rate: 0.8)
         (sleep 0.5) )
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

   (if (one-in 2)
      then
         (play 60)
         (sleep 1)
      else
         (play 64)
         (sleep 1) )

   (if (one-in 2)
      then
         (play 60)
         (sleep 1)
      elseif (one-in 4)
         (play 64)
         (sleep 1)
      else
         (play 67)
         (sleep 1) )

   (if (one-in 2) && (one-in 4)
      then
         (play 60)
         (sleep 1)
      elseif (one-in 2) && (one-in 4)
         (play 66)
         (sleep 1)
      else
         (play 67)
         (sleep 1) )

   (= counter 1)
   (while counter <= 5 do
      (puts counter)
      (= counter counter + 1) )

   (case note
      when :c || :c4 then
         (play 60)
      when :e4 then
         (play 64)
      else
         (play 67) )

   ([2 4 6 8].all? do |num| num % 2 == 0)

)



