Description
===

My custom keyboard layouts for GNU/Linux.


Installation
===

- Add content of us_dev to the end of /usr/share/X11/xkb/symbols/us
- Add content of ru_dev to the end of /usr/share/X11/xkb/symbols/ru
- Edit /usr/share/X11/xkb/rules/evdev.xml and add follow sections

        <variant>
          <configItem>
            <name>us-dev</name>
            <description>English (US, for development)</description>
          </configItem>
        </variant>

    and

        <variant>
          <configItem>
            <name>ru-dev</name>
            <description>Russian (for development)</description>
          </configItem>
        </variant>

to appropriate sections of the file.

- Run 'sudo dpkg-reconfigure xkb-data' to apply changes
