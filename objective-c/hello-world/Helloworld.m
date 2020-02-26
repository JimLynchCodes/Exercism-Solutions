#import <Foundation/Foundation.h>
#import "HelloWorld.h"

@implementation HelloWorld
- (NSString*)hello:(NSString*)input;
{
   NSString* name = input ? input : @"World";
   
   return [NSString stringWithFormat:@"Hello, %@!", name];
}
@end