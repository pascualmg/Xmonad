import os
import sys
import signal
from gtts import gTTS
import subprocess

# Velocidad de reproducción (1.0 es normal, 2.0 es el doble de rápido, 0.5 es la mitad de rápido)
SPEED = 1.5

def kill_previous_instances():
    # Mata todas las instancias previas de mpg321
    os.system("pkill mpg321")

def speak(text, lang='es', speed=SPEED):
    kill_previous_instances()

    tts = gTTS(text=text, lang=lang)
    tts.save("output.mp3")

    # Usa sox para ajustar la velocidad y mpg321 para reproducir
    cmd = f"sox output.mp3 output_speed.mp3 tempo {speed} && mpg321 output_speed.mp3"
    process = subprocess.Popen(cmd, shell=True)

    # Espera a que termine la reproducción
    process.wait()

    # Limpia los archivos temporales
    os.remove("output.mp3")
    os.remove("output_speed.mp3")

if __name__ == "__main__":
    # Configura un manejador de señales para limpiar archivos si el script es interrumpido
    def signal_handler(sig, frame):
        kill_previous_instances()
        if os.path.exists("output.mp3"):
            os.remove("output.mp3")
        if os.path.exists("output_speed.mp3"):
            os.remove("output_speed.mp3")
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)

    if len(sys.argv) > 1:
        input_text = " ".join(sys.argv[1:])
    else:
        input_text = sys.stdin.read().strip()

    if input_text:
        speak(input_text)
    else:
        speak("sin texto para leer!")
