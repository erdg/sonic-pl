[sp loop1
   (load-samples [:bd-gas])

   (live-loop :floor do
      (4.times do
         (density [2 2 2 3].ring.tick do
            (sample :bd-gas  amp: 2  pan: (rrand -0.2 0.2))
            (sleep 1) ) ) )

   (live-loop :offbeat do
      (use-synth :pnoise)
      (4.times do
         (sleep 0.5)
         (play release: 0.1 amp: 0.04) ) )

   (= bass-notes [:C2 :Ef1 :F1 :Af1 :Bf1].ring)

   (define :bass do |note|
      (use-synth :blade)
      (use-synth-defaults
         amp: 1.6 )
      (play note sustain: 1 release: 1 cutoff: 60) )

   (live-loop :bass1 do
      (sync :floor)
      (= octaves [2 1 1 1 1].ring)
      (bass bass-notes.tick)
      (sleep 2) )

   (live-loop :pad1 do
      (sync :floor)
      (use-synth :hollow)
      (use-synth-defaults
         cutoff: (rrand-i 66 96)
         amp: 0.6 )
      (= ns [5 8 13 27].choose)
      (ns.times do
         (play [:Ef4 :F4 :G4 :Bf4].ring.tick
            pan: (rrand -0.6 0.6) )
         (sleep 1.0 / ns) ) )

   (live-loop :pad2 do
      (sync :floor)
      (sleep 0.5)
      (use-synth :growl)
      (use-synth-defaults
         amp: (rrand 0.8 1.4)
         cutoff: (rrand-i 86 112)
         sustain: 0.06
         release: 0.06 )
      (= ns [11 19 33].choose)
      (= rs (range -1 1 step: 0.1))
      (ns.times do
         (sleep 1.0 / ns)
         (play [:Ef4 :F4 :G4 :Bf4].choose
            pan: rs.ring.tick ) ) )

]
