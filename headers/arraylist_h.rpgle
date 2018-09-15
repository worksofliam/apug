        
        /if not defined (ARRAYLIST_H)
        /define ARRAYLIST_H
        
        ///
        // \brief List Implementation : ArrayList
        //
        // A list implementation with a memory block as backend. The memory
        // will be dynamically allocated and deallocated. Therefore the list
        // can grow and shrink dynamically as requested.
        //
        // <br><br>
        //
        // This list implementation works with a head data structure.
        //
        // <br><br>
        //
        // The entries are stored in an "array" which consists of pointers.
        // The pointers of the array store the start address of the memory
        // of the values.
        //
        // <br><br>
        //
        // All values are internally null-terminated. So a value of x'00'
        // won't work as expected and should be avoided.
        //
        // <br><br>
        //
        // Access to the element is accomplished through accessing the arraylist
        // with an index (position). The index is 0-based. So the first element
        // has an index of 0 (zero).
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        // \project ArrayList
        ///
        
        
        //                          The MIT License (MIT)
        // 
        // Copyright (c) 2016 Mihael Schmidt
        // 
        // Permission is hereby granted, free of charge, to any person obtaining a copy 
        // of this software and associated documentation files (the "Software"), to deal 
        // in the Software without restriction, including without limitation the rights 
        // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
        // copies of the Software, and to permit persons to whom the Software is 
        // furnished to do so, subject to the following conditions:
        // 
        // The above copyright notice and this permission notice shall be included in 
        // all copies or substantial portions of the Software.
        // 
        // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
        // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
        // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
        // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
        // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
        // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
        // SOFTWARE.
        
        
        ///
        // \brief Create arraylist
        //
        // Creates an arraylist.
        //
        // <br><br>
        //
        // The initial size is 10. The default increment size
        // is 0 which means with each incrementation the arraylist will double its
        // size.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Initial arraylist size (default: 10)
        // \param Incrementation size (default: 0 - double)
        //
        // \return Pointer to arraylist
        ///
        dcl-pr arraylist_create pointer extproc('arraylist_create');
          initSize uns(10) const options(*nopass);
          incSize uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Dispose arraylist
        //
        // Disposes the arraylist and all its elements. The pointer will be set
        // to *null.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        ///
        dcl-pr arraylist_dispose extproc('arraylist_dispose');
          arraylist pointer;
        end-pr;
        
        ///
        // \brief Add element
        //
        // Adds an element to the arraylist by copying the content to dynamically
        // allocated memory. Values are stored null-terminated.
        //
        // <br><br>
        //
        // If a position is passed the caller must be certain that the
        // position is inside the bounds of the arraylist. If the position is
        // outside of the arraylist an escape message will be sent.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        // \param Pointer to new entry
        // \param Length of new entry (in byte)
        // \param Position
        ///
        dcl-pr arraylist_add extproc('arraylist_add');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
          pos uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Prepend element to the arraylist
        //
        // Adds an element to the beginning of the arraylist by copying the content to
        // dynamically allocated memory. Values are stored null-terminated. If the
        // the arraylist is not empty all other elements will be pushed down by one
        // position.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        // \param Pointer to new entry
        // \param Length of new entry (in byte)
        ///
        dcl-pr arraylist_addFirst extproc('arraylist_addFirst');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Append element to the arraylist
        //
        // Adds an element to the end of the arraylist by copying the content to
        // dynamically allocated memory. Values are stored null-terminated.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        // \param Pointer to new entry
        // \param Length of new entry (in byte)
        ///
        dcl-pr arraylist_addLast extproc('arraylist_addLast');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Get element
        //
        // Returns a pointer to the elment at the given position. The element is
        // null-terminated. Changes to the element through the returned pointer is
        // not recommended. Use the appropriate procedures instead.
        //
        // <br><br>
        //
        // If the requested element position is not in the arraylist then an escape
        // message will be sent.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        // \param Position
        //
        // \return Pointer to the null-terminated element
        //         or *null if arraylist is empty
        ///
        dcl-pr arraylist_get pointer extproc('arraylist_get');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get first element
        //
        // Returns a pointer to the first elment in the arraylist. The element is
        // null-terminated. Changes to the element through the returned pointer is
        // not recommended. Use the appropriate procedures instead.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        //
        // \return Pointer to the null-terminated element or *null if the arraylist is empty
        ///
        dcl-pr arraylist_getFirst pointer extproc('arraylist_getFirst');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Get last element
        //
        // Returns a pointer to the last elment in the arraylist. The element is
        // null-terminated. Changes to the element through the returned pointer is
        // not recommended. Use the appropriate procedures instead.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        //
        // \return Pointer to the null-terminated element
        //         or *null if the arraylist is empty
        ///
        dcl-pr arraylist_getLast pointer extproc('arraylist_getLast');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Check if arraylist is empty
        //
        // Checks if the arraylist is empty.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        //
        // \return *on = arraylist is empty <br>
        //         *off = arraylist is not empty
        ///
        dcl-pr arraylist_isEmpty ind extproc('arraylist_isEmpty');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Get arraylist size
        //
        // Returns the number of elements currently in the arraylist.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        //
        // \return Number of elements in the arraylist
        ///
        dcl-pr arraylist_getSize uns(10) extproc('arraylist_getSize');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Get arraylist capacity
        //
        // Returns the number of elements which can be stored in the current
        // arraylist.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to arraylist
        //
        // \return Number of elements able to store in the arraylist
        ///
        dcl-pr arraylist_getCapacity uns(10) extproc('arraylist_getCapacity');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Clear arraylist
        //
        // Deletes all entries. The capacity of the arraylist remains the same.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        ///
        dcl-pr arraylist_clear extproc('arraylist_clear');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Remove an element
        //
        // Removes an element from the arraylist. If the given position is outside of
        // the bounds of the arraylist an escape message will be sent.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        // \param Element index to be removed
        ///
        dcl-pr arraylist_remove extproc('arraylist_remove');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Remove the first element
        //
        // Removes the first element from the arraylist.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        ///
        dcl-pr arraylist_removeFirst extproc('arraylist_removeFirst');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Remove the last element
        //
        // Removes the last element from the arraylist.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        ///
        dcl-pr arraylist_removeLast extproc('arraylist_removeLast');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Remove a range of elements
        //
        // Removes a range of elements from the arraylist. The range must be inside
        // the bounds of the arraylist. If the range is outside the arraylist an
        // escape message will be sent.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        // \param Range starting index
        // \param Number of elements to remove
        ///
        dcl-pr arraylist_removeRange extproc('arraylist_removeRange');
          arraylist pointer const;
          index uns(10) const;
          count uns(10) const;
        end-pr;
        
        ///
        // \brief Contains element
        //
        // Checks if the arraylist contains the passed data.
        // The check will be done byte by byte, so trailing spaces also count.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        // \param Pointer to data
        // \param Data length
        //
        // \return *on if the arraylist contains the data, *off otherwise
        ///
        dcl-pr arraylist_contains ind extproc('arraylist_contains');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Get index of element
        //
        // Returns the index of the passed element.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        // \param Pointer to data
        // \param Data length
        //
        // \return index of the element or -1 if the element is not in the arraylist
        ///
        dcl-pr arraylist_indexOf int(10) extproc('arraylist_indexOf');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Get last index of element
        //
        // Returns the last index of the passed element.
        //
        // \author Mihael Schmidt
        // \date   16.04.2011
        //
        // \param Pointer to the arraylist
        // \param Pointer to data
        // \param Data length
        //
        // \return last index of the element
        //         or -1 if the element is not in the arraylist
        ///
        dcl-pr arraylist_lastIndexOf int(10) extproc('arraylist_lastIndexOf');
          arraylist pointer const;
          valuePtr pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Frequency of element
        //
        // Returns the number of times the passed element is in the arraylist.
        //
        // \author Mihael Schmidt
        // \date   2011-04-19
        //
        // \param Pointer to the arraylist
        // \param Pointer to data
        // \param Data length
        //
        // \return frequency of the passed data in the arraylist
        ///
        dcl-pr arraylist_frequency uns(10) extproc('arraylist_frequency');
          arraylist pointer const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Replace element
        //
        // Replaces the given element with the new data.
        //
        // \author Mihael Schmidt
        // \date 2011-04-19
        //
        // \param Pointer to the arraylist
        // \param Index to data which should be replaced
        // \param Pointer to the new data
        // \param Length of the new data
        ///
        dcl-pr arraylist_replace extproc('arraylist_replace');
          arraylist pointer const;
          index uns(10) const;
          value pointer const;
          valueLength uns(10) const;
        end-pr;
        
        ///
        // \brief Create a copy of the arraylist
        //
        // Returns a copy of the arraylist.
        //
        // \author Mihael Schmidt
        // \date 2011-04-19
        //
        // \param Pointer to the arraylist
        //
        // \return Pointer to the new arraylist
        ///
        dcl-pr arraylist_copy pointer extproc('arraylist_copy');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Create a sublist
        //
        // Returns a sublist of this arraylist.
        //
        // \author Mihael Schmidt
        // \date 2011-04-19
        //
        // \param Pointer to the arraylist
        //
        // \return Pointer to the new arraylist (sublist)
        ///
        dcl-pr arraylist_sublist pointer extproc('arraylist_sublist');
          arraylist pointer const;
          startIndex uns(10) const;
          length uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Swap arraylist items
        //
        // \author Mihael Schmidt
        // \date 2011-04-19
        //
        // \param Pointer to the arraylist
        // \param Item to swap
        // \param Item to swap
        ///
        dcl-pr arraylist_swap extproc('arraylist_swap');
          arraylist pointer const;
          itemPos1 uns(10) const;
          itemPos2 uns(10) const;
        end-pr;
        
        ///
        // \brief Add all elements to the arraylist
        //
        // Adds all elements from the source arraylist to the destination arraylist.
        //
        // \param Pointer to the destination arraylist
        // \param Pointer to the source arraylist
        ///
        dcl-pr arraylist_addAll extproc('arraylist_addAll');
          destArraylist pointer const;
          sourceArraylist pointer const;
        end-pr;
        
        ///
        // \brief Add integer value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds an integer to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addInteger extproc('arraylist_addInteger');
          arraylist pointer const;
          value int(10) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add short integer value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a short integer to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addShort extproc('arraylist_addShort');
          arraylist pointer const;
          value int(5) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add long integer value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a long integer to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addLong extproc('arraylist_addLong');
          arraylist pointer const;
          value int(20) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add packed decimal value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a packed decimal to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addDecimal extproc('arraylist_addDecimal');
          arraylist pointer const;
          value packed(15 : 5) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add float value to the arraylist
        // 
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a float to the arraylist.
        // 
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        /// 
        dcl-pr arraylist_addFloat extproc('arraylist_addFloat');
          arraylist pointer const;
          value float(4) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add double value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a double to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addDouble extproc('arraylist_addDouble');
          arraylist pointer const;
          value float(8) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add date value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a date to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addDate extproc('arraylist_addDate');
          arraylist pointer const;
          value date const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add boolean value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a boolean to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addBoolean extproc('arraylist_addBoolean');
          arraylist pointer const;
          value ind const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Add character value to the arraylist
        //
        // This procedure is a wrapper for the <em>add</em> procedure and
        // adds a character string to the arraylist.
        //
        // \param Pointer to the arraylist
        // \param Value
        // \param Position (default: append)
        ///
        dcl-pr arraylist_addString extproc('arraylist_addString');
          arraylist pointer const;
          value varchar(65535) const;
          index uns(10) const options(*nopass);
        end-pr;
        
        ///
        // \brief Get integer value from arraylist
        //
        // Returns the previously inserted integer value from the arraylist.
        // If the value cannot be interpreted as an integer an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getInteger int(10) extproc('arraylist_getInteger');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get short integer value from arraylist
        //
        // Returns the previously inserted short integer value from the arraylist.
        // If the value cannot be interpreted as a short integer an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getShort int(5) extproc('arraylist_getShort');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get long integer value from arraylist
        //
        // Returns the previously inserted long integer value from the arraylist.
        // If the value cannot be interpreted as a long integer an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getLong int(20) extproc('arraylist_getLong');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get packed decimal value from arraylist
        //
        // Returns the previously inserted packed decimal value from the arraylist.
        // If the value cannot be interpreted as a packed decimal an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getDecimal packed(15:5) 
                                    extproc('arraylist_getDecimal');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get float value from arraylist
        //
        // Returns the previously inserted float value from the arraylist.
        // If the value cannot be interpreted as a float an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getFloat float(4) extproc('arraylist_getFloat');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get dobule value from arraylist
        //
        // Returns the previously inserted double value from the arraylist.
        // If the value cannot be interpreted as a double an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getDouble float(8) extproc('arraylist_getDouble');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get date value from arraylist
        //
        // Returns the previously inserted date value from the arraylist.
        // If the value cannot be interpreted as a date an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getDate date extproc('arraylist_getDate');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get boolean value from arraylist
        //
        // Returns the previously inserted boolean value from the arraylist.
        // If the value cannot be interpreted as a boolean an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getBoolean ind extproc('arraylist_getBoolean');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Get character value from arraylist
        //
        // Returns the previously inserted character string value from the arraylist.
        // If the value cannot be interpreted as a char value an escape message
        // will be sent.
        //
        // \param Pointer to the arraylist
        // \param Position
        //
        // \return Value
        ///
        dcl-pr arraylist_getString varchar(65535) 
                                   extproc('arraylist_getString');
          arraylist pointer const;
          index uns(10) const;
        end-pr;
        
        ///
        // \brief Execute procedure for every arraylist entry
        //
        // The passed procedure will be executed for every entry
        // in the arraylist.
        //
        // <br><br>
        //
        // The user can pass data through a pointer to the procedure.
        // The pointer will not be touched by this procedure itself, so it
        // can be *null.
        //
        // <br><br>
        //
        // The value of list entry can be changed through the passed procedure.
        //
        // <br><br>
        //
        // The parameters for the passed procedure are:
        // <ul>
        //   <li>Pointer to the entry value (const)</li>
        //   <li>Value length (const) </li>
        //   <li>Pointer to the user data (const)</li>
        // </ul>
        //
        // \param Pointer to the arraylist
        // \param Procedure pointer
        // \param Pointer to user data
        ///
        dcl-pr arraylist_foreach extproc('arraylist_foreach');
          arraylist pointer const;
          procPtr pointer(*proc) const;
          userData pointer const;
        end-pr;
        
        ///
        // \brief To character array
        //
        // Copies all entries of this arraylist to the passed array. Entries will be
        // truncated if they are too big for the array. If the array is not big
        // enough, the last entries will be silently dropped.
        //
        // \param Pointer to the arraylist
        // \param Pointer to the array
        // \param Element count
        // \param Element length
        //
        ///
        dcl-pr arraylist_toCharArray extproc('arraylist_toCharArray');
          arraylist pointer const;
          arrayPtr pointer const;
          count uns(10) const;
          length uns(10) const;
        end-pr;
        
        ///
        // \brief Reverse order of arraylist entries
        //
        // Reverses the order of the entries of the arraylist.
        //
        // \param Pointer to the arraylist
        ///
        dcl-pr arraylist_reverse extproc('arraylist_reverse');
          arraylist pointer const;
        end-pr;
        
        ///
        // \brief Split character string
        //
        // The passed character string will be split into tokens by either
        // a passed or the default separator. All tokens will be added to
        // a new arraylist which will be returned.
        //
        // <br><br>
        //
        // Empty (but not blank) values will be dropped silently.
        //
        // \author Mihael Schmidt
        // \date   26.01.2009
        //
        // \param Character string (null-terminated)
        // \param Separator (default: ;)
        //
        // \return Pointer to the filled arraylist
        ///
        dcl-pr arraylist_split pointer extproc('arraylist_split');
          string varchar(65535) const;
          separator char(1) const options(*nopass);
        end-pr;
        
        ///
        // \brief Return character representation of arraylist
        //
        // Returns a string with the arraylist items separated either by
        // the passed or default separator. The items can be
        // enclosed by a passed character. The maximum character length
        // returned is 65535. Every character/item after that will be
        // dropped silently. Items will not be trimmed for this operation.
        //
        // <br><br>
        //
        // If the third parameter is passed, the third parameter will be
        // pre- and appended to the item. If the fourth parameter is also
        // passed the third parameter will be prepended to the item and the
        // fourth parameter will be appended to the item.
        //
        // \param Pointer to the arraylist
        // \param separator (default: ,)
        // \param enclosing character (default: nothing)
        // \param enclosing character at the end of item (default: nothing)
        //
        // \return character representation of all arraylist items
        ///
        dcl-pr arraylist_toString varchar(65535) extproc('arraylist_toString');
          arraylist pointer const;
          separator char(1) const options(*omit : *nopass);
          enclosing varchar(100) const options(*nopass);
          enclosingEnd varchar(100) const options(*nopass);
        end-pr;
        
        /endif