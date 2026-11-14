# Schema


# Data
- Selection Strategy
  - By Scale
    - Diatonic
    - Enharmonic
    - Soft Chromatic
    - Hard Chromatic
  - By Classification
    - Classification (First Mode, Second Mode, Third Mode, Fourth Mode, Plagal First Mode, Plagal Second Mode, Grave Mode, Plagal Fourth Mode)
      - Full Signature, for any signature belonging to that classification (across all Ichos/Indicator variants)
        - Genre (only when the signature is shared by more than one mode, e.g., Lower Mode One: Eirmologic / Sticheraric)
  - By Signature
    - Ichos
      - First
        - Base Pa, Fhtora Diatonic Pa
        - Base Ke, Fthora Diatonic Ke
      - Second
        - Base Pa, Fthora Hard Chromatic Pa 
        - Base Bou, Fthora Hard Chromatic Pa (verify this)
        - Base Di, Fthora Soft Diatonic Di
      - Third
        - Base Ga, Fthora Ga
      - Third Nana
        - Base Ga, Fthora Diatonic Ni
        - Base Ga, Fthora Diatonic Ga
      - Classification: Authentic Mode Three
        - Base Ga, Fthora Diatonic Ga
      - Fourth
        - Base Di, Fthora Diatonic Di
        - Base Bou, Fthora Diatonic Bou
          - Descending SynechesElafron
      - Legetos
      - Varys
      - VarysZo
    - Ichos Plagal
      - Plagal First
      - Plagal Second
      - Plagal Fourth
    - Genre (after the signature, only when it is shared by more than one mode, e.g., Lower Mode One: Eirmologic / Sticheraric)


# TODO
- [x] I need the build-by-signature path to collect the signature as it goes
- [x] I need the two-column layout to have a min width threshold so it's a flat list (non-grid) beneath a threshold. I think lg.
- [ ] Elements of the modal signatures are overflowing the modal on the mode info view. We'll need to clean that up.
- [ ] Bottom three pitches need rendering fixes
- [ ] I need to decide how to handle the range setting, particularly with regard to how this interacts with the mode-derived range.
- [ ] I'll need to do some testing with mobile usability for modes with greater ranges.
