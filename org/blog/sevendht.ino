#include <dht.h> // DHTlib

#define DELAY 2000

#define PIN_DHT 5
#define PIN_ENA 4
#define PIN_CLK 3
#define PIN_DAT 2

byte num2bits[] = { B10110111, B00100100, B01110011, B01110110, B11100100, B11010110, B11010111, B10110100, B11110111, B11110110 };
bool flip = false;
dht DHT;

void setup() {
  pinMode(PIN_ENA, OUTPUT);
  pinMode(PIN_CLK, OUTPUT);
  pinMode(PIN_DAT, OUTPUT);
}

void writeNum(byte n) { writeNumRaw((n > 99) ? 0 : n); }
void writeNumRaw(byte n) {
  digitalWrite(PIN_ENA, HIGH);
  shiftOut(PIN_DAT, PIN_CLK, LSBFIRST, num2bits[n % 10]); // right
  shiftOut(PIN_DAT, PIN_CLK, LSBFIRST, num2bits[n / 10 % 10]); // left
  digitalWrite(PIN_ENA, LOW);
}

void loop() {
  flip = !flip;
  int chk = DHT.read11(PIN_DHT);
  if (flip)
    writeNum(DHT.temperature);
  else
    writeNum(DHT.humidity);
  delay(DELAY);
}
