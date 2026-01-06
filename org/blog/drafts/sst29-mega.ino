#define WE 14
#define OE 15

// Port Registers:
// ←→ A = data
// -> C = addr lower
// -> L = addr higher

byte data[] = {
    'A',  'B',  'C',  'D',  'E',  'F',  'G',  'H',
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    0x7f, 0xbf, 0xdf, 0xef, 0xf7, 0xfb, 0xfd, 0xfe,
    0x00, 0xff, 0x55, 0xaa, '0',  '1',  '2',  '3'
};

void setAddress(word addr) {
  DDRC = DDRL = 0xff; // 1 = OUTPUT
  PORTC = addr & 0x00ff;
  PORTL = addr >> 8;
}

void writeEEPROM(int addr, byte data) {
  digitalWrite(OE, HIGH);
  setAddress(addr);
  digitalWrite(WE, LOW);
  DDRA = 0xff; // 1 = pinMode = OUTPUT
  PORTA = data;
  delayMicroseconds(1);
  digitalWrite(WE, HIGH);
  /* delay(10); */ // FIX: Sometimes needed?
}

byte readEEPROM(int addr) {
  byte data = 0;
  DDRA = 0x00; // 0 = pinMode = INPUT
  setAddress(addr);
  digitalWrite(WE, HIGH);
  digitalWrite(OE, LOW);
  data = PINA;
  digitalWrite(OE, HIGH);
  return data;
}

void setup() {
  digitalWrite(WE, HIGH);
  pinMode(WE, OUTPUT); digitalWrite(WE, HIGH);
  pinMode(OE, OUTPUT); digitalWrite(OE, HIGH);

  DDRC = DDRL = 0xff; // 1 = OUTPUT

  Serial.begin(57600);
  while(!Serial);

  printContents();
  Serial.print("Programming...");
  int s = 127;
  for (word address = 0; (address < sizeof(data)); address++) {
    writeEEPROM(address, data[address]);
  }
  for (word address = sizeof(data); (address <= s); address++) {
    writeEEPROM(address, 0xea);
  }
  while (readEEPROM(s) != readEEPROM(s)) {
    Serial.print(".");
    delayMicroseconds(100);
  };
  Serial.println(" done");

  delay(100);
  printContents();
}

void loop() {
}

void printContents() {
  for (int base = 0; base <= 255; base += 16) {
    byte data[16];
    for (int offset = 0; offset <= 15; offset += 1) {
      data[offset] = readEEPROM(base + offset);
    }
    char buf[80];
    sprintf(buf, "%03x:  %02x %02x %02x %02x %02x %02x %02x %02x   %02x %02x %02x %02x %02x %02x %02x %02x",
            base, data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);
    Serial.println(buf);
  }
}
