package org.barabu.emv

import android.app.Activity
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.graphics.drawable.Animatable

import EmvUtils._
import TLV._

class MainActivity extends AppCompatActivity {
  // allows accessing `.value` on TR.resource.constants
  implicit val context = this

  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)

    val vt = findViewById(R.id.text1).asInstanceOf[android.widget.TextView]

    val tlvPattern1 = Array[Byte](0x6F, 0x1A, 0x84, 0x07, 0xA0, 0x00, 0x00, 0x00, 0x04, 0x10, 0x10, 0xA5, 0x0F, 0x50, 0x0A, 0x4D, 0x61, 0x73, 0x74, 0x65, 0x72, 0x43, 0x61, 0x72, 0x64, 0x87, 0x01, 0x01)

    val tlv1 = TLV(tlvPattern1)
    printTlvHierarchy(tlv1)

    vt.setText("Hello, Hello, Hello")
    
  }

}
