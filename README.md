# ByzanTone

ByzanTone is a tool for learning the pitches and intervals of Byzantine chant.
It provides an interactive interface to help users understand and practice the
scales and intervals used in Byzantine music. ByzanTone is deployed on GitHub
Pages and is publicly available at
[josh-mills.github.io/byzantone/](https://josh-mills.github.io/byzantone/).

## Features (in development)

- Visualization of Byzantine scales
- Interactive pitch and interval training
- Customizable audio settings
- Real-time feedback on pitch accuracy

## Installation

To install and run ByzanTone locally, follow these steps:

1. **Clone the repository**:
    ```sh
    git clone https://github.com/josh-mills/byzantone.git
    cd byzantone
    ```

2. **Install dependencies**: Install the Elm compiler following the instructions
   [here](https://guide.elm-lang.org/install/elm.html), and Node.js if not
   already installed. Then install the Node dependencies:

    ```sh
    npm install
    ```

3. **Run the development server**:
    ```sh
    npm run dev
    ```
    This will run the application in development watch mode at
    `http://localhost:3002`.

## Usage

### Starting the Application

ByzanTone is publicly available at [GitHub
Pages](https://josh-mills.github.io/byzantone/), but can also be run locally. To
build and start the application, run the following command:
```sh
npm run build && npm run start
```

### Keyboard Shortcuts

- `Up/Down Arrow Keys`: step up or down from the currently playing tone
- `Escape`: stop the currently playing tone
- `1–9`: ascend 1–9 tones from the currently playing tone
- `Shift + 1–9`: descend 1–9 tones from the currently playing tone

## Acknowledgements

- Daniel Garthur, for his [byzhtml](https://github.com/danielgarthur/byzhtml),
  which provides custom web components for displaying Byzantine Chant neumes in
  HTML, and for his [Standard Byzantine Music Font
  Layout](https://github.com/neanes/sbmufl) project.

- [Elm](https://elm-lang.org/), for being a delightful language for front-end
  development.

- John Michael Boyer, for his _[Byzantine Chant: The Received
  Tradition](https://byzchantlessonbook.com/)_, a monument of psaltic pedagogy
  unparalleled in the anglophone world.