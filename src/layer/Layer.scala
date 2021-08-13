package layer

import picture.Picture

class Layer (val picture: Picture,
             val name: String,
             var transparency: Double = 1,
             var active: Boolean = true) {
}
