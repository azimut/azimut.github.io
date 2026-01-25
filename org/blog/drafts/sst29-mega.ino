#define NBLOCKS 5
#define WE 4
#define OE 3

// Port Registers:
// <- A = data
// -> C = addr lower
// -> L = addr higher

byte data[] = {
    'A',  'B',  'C',  'D',  'E',  'F',  'G',  'H',
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    0x7f, 0xbf, 0xdf, 0xef, 0xf7, 0xfb, 0xfd, 0xfe,
    0x00, 0xff, 0x55, 0xaa, '0',  '1',  '2',  '3'
};

void setAddress(word addr) {
  DDRC = 0xff; // 1 = OUTPUT
  DDRL = 0xff; // 1 = OUTPUT
  PORTC = (addr >> 0) & 0x00ff;
  PORTL = (addr >> 8) & 0x00ff;
}

void writeEEPROM(int addr, byte wdata) {
  digitalWrite(OE, HIGH);
  setAddress(addr);
  digitalWrite(WE, LOW);
  DDRA = 0xff; // 1 = pinMode = OUTPUT
  PORTA = wdata;
  delayMicroseconds(1);
  digitalWrite(WE, HIGH);
}

byte readEEPROM(int addr) {
  byte rdata = 0;
  DDRA = 0x00; // 0 = pinMode = INPUT
  setAddress(addr);
  digitalWrite(WE, HIGH);
  digitalWrite(OE, LOW);
  rdata = PINA;
  digitalWrite(OE, HIGH);
  return rdata;
}

void setup() {
  digitalWrite(WE, HIGH);
  pinMode(WE, OUTPUT); digitalWrite(WE, HIGH);
  pinMode(OE, OUTPUT); digitalWrite(OE, HIGH);

  DDRC = 0xff; // 1 = OUTPUT
  DDRL = 0xff; // 1 = OUTPUT

  Serial.begin(57600);
  while(!Serial);

  /* Serial.print("\nDisabling SDP..."); */
  /* disable_sdp(); */
  /* Serial.println(" done"); */

  // Dump
  printContents();
  // Programming
  Serial.print("Programming ... ");
  for (word address = 0; (address < sizeof(data)); address++) {
    // Wait after each TBLCO (Byte Load Cycle Time)
    if (address != 0 && address % 128 == 0) {
      while (readEEPROM(address-1) != readEEPROM(address-1)) {
        delayMicroseconds(100);
      };
    }
    writeEEPROM(address, data[address]);
  }
  // Padding
  for (word address = sizeof(data); address % 16 != 0; address++) {
    writeEEPROM(address, 0x00); // NOP = 0x00
  }
  Serial.println("done");
  delay(500);
  // Validation
  Serial.print("Validating...");
  byte okvalidation = 0;
  for (word address = 0; address < sizeof(data); address++) {
    if (readEEPROM(address) != data[address]) {
      okvalidation = 1;
      break;
    }
  }
  if (okvalidation != 0) {
    Serial.println("FAIL");
  } else {
    Serial.println("OK");
  }
  // Dump
  printContents();
}

void loop() {
}

void printContents() {
  for (int base = 0; base <= (NBLOCKS * 127)+1; base += 16) {
    byte bdata[16];
    for (int offset = 0; offset <= 15; offset += 1) {
      bdata[offset] = readEEPROM(base + offset);
    }
    char sbuf[80];
    sprintf(sbuf, "%03x:  %02x %02x %02x %02x %02x %02x %02x %02x   %02x %02x %02x %02x %02x %02x %02x %02x",
            base, bdata[0], bdata[1], bdata[2], bdata[3], bdata[4], bdata[5], bdata[6], bdata[7],
            bdata[8], bdata[9], bdata[10], bdata[11], bdata[12], bdata[13], bdata[14], bdata[15]);
    Serial.println(sbuf);
  }
}

void disable_sdp() {
  digitalWrite(OE, HIGH);
  DDRA = 0xff;
  setByte(0x5555, 0xaa);
  setByte(0x2aaa, 0x55);
  setByte(0x5555, 0x80); // 0x80
  setByte(0x5555, 0xaa);
  setByte(0x2aaa, 0x55);
  setByte(0x5555, 0x20); // 0x20
  DDRA = 0x00;
  delay(10);
}

void setByte(int addr, byte data) {
  setAddress(addr); // digitalWrite(OE, HIGH);
  PORTA = data;
  delayMicroseconds(1);
  digitalWrite(WE, LOW);
  delayMicroseconds(1);
  digitalWrite(WE, HIGH);
}
