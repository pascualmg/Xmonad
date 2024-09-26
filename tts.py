import os
import sys
import signal
import subprocess
import argparse
from gtts import gTTS
import gtts.lang
from langdetect import detect

# Configuración por defecto
DEFAULT_SPEED = 1.25
DEFAULT_CLIPBOARD_TOOL = 'xsel'

def kill_previous_instances():
    os.system("pkill mpg321")

def list_languages():
    print("Idiomas disponibles:")
    for lang, name in gtts.lang.tts_langs().items():
        print(f"{lang}: {name}")

def get_clipboard_content(tool):
    if tool == 'xsel':
        return subprocess.getoutput("xsel -o")
    elif tool == 'wl-clipboard':
        return subprocess.getoutput("wl-paste")
    elif tool == 'emacs':
        return subprocess.getoutput("emacsclient --eval '(car kill-ring)'")
    else:
        raise ValueError(f"Herramienta de portapapeles no soportada: {tool}")

def speak(text, speed=DEFAULT_SPEED):
    kill_previous_instances()

    detected_lang = detect(text)
    tts = gTTS(text=text, lang=detected_lang, tld='es')
    tts.save("output.mp3")

    cmd = f"sox output.mp3 output_speed.mp3 tempo {speed} && mpg321 output_speed.mp3"
    process = subprocess.Popen(cmd, shell=True)
    process.wait()

    os.remove("output.mp3")
    os.remove("output_speed.mp3")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Text-to-Speech avanzado para Linux')
    parser.add_argument('--speed', type=float, default=DEFAULT_SPEED, help='Velocidad de reproducción')
    parser.add_argument('--list-languages', action='store_true', help='Listar idiomas disponibles')
    parser.add_argument('--clipboard', choices=['xsel', 'wl-clipboard', 'emacs'],
                        default=DEFAULT_CLIPBOARD_TOOL, help='Herramienta de portapapeles a usar')
    parser.add_argument('text', nargs='*', help='Texto a leer (opcional si se usa entrada estándar o portapapeles)')
    args = parser.parse_args()

    def signal_handler(sig, frame):
        kill_previous_instances()
        for file in ["output.mp3", "output_speed.mp3"]:
            if os.path.exists(file):
                os.remove(file)
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)

    if args.list_languages:
        list_languages()
    else:
        if args.text:
            input_text = " ".join(args.text)
        elif not sys.stdin.isatty():
            input_text = sys.stdin.read().strip()
        else:
            input_text = get_clipboard_content(args.clipboard)

        if input_text:
            speak(input_text, args.speed)
        else:
            print("Sin texto para leer. Usa --help para ver las opciones.")
