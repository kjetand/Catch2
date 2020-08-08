#include <catch2/internal/catch_clara.hpp>

#include <catch2/internal/catch_console_width.hpp>
#include <catch2/internal/catch_textflow.hpp>
#include <catch2/internal/catch_platform.hpp>

#include <algorithm>

namespace {
    bool isOptPrefix( char c ) {
        return c == '-'
#ifdef CATCH_PLATFORM_WINDOWS
               || c == '/'
#endif
            ;
    }

    std::string normaliseOpt( std::string const& optName ) {
#ifdef CATCH_PLATFORM_WINDOWS
        if ( optName[0] == '/' )
            return "-" + optName.substr( 1 );
        else
#endif
            return optName;
    }

} // namespace

namespace Catch {
    namespace clara {
        namespace detail {

            Args::Args( int argc, char const* const* argv ):
                m_exeName( argv[0] ), m_args( argv + 1, argv + argc ) {}

            Args::Args( std::initializer_list<std::string> args ):
                m_exeName( *args.begin() ),
                m_args( args.begin() + 1, args.end() ) {}

            void TokenStream::loadBuffer() {
                m_tokenBuffer.resize( 0 );

                // Skip any empty strings
                while ( it != itEnd && it->empty() )
                    ++it;

                if ( it != itEnd ) {
                    auto const& next = *it;
                    if ( isOptPrefix( next[0] ) ) {
                        auto delimiterPos = next.find_first_of( " :=" );
                        if ( delimiterPos != std::string::npos ) {
                            m_tokenBuffer.push_back(
                                { TokenType::Option,
                                  next.substr( 0, delimiterPos ) } );
                            m_tokenBuffer.push_back(
                                { TokenType::Argument,
                                  next.substr( delimiterPos + 1 ) } );
                        } else {
                            if ( next[1] != '-' && next.size() > 2 ) {
                                std::string opt = "- ";
                                for ( size_t i = 1; i < next.size(); ++i ) {
                                    opt[1] = next[i];
                                    m_tokenBuffer.push_back(
                                        { TokenType::Option, opt } );
                                }
                            } else {
                                m_tokenBuffer.push_back(
                                    { TokenType::Option, next } );
                            }
                        }
                    } else {
                        m_tokenBuffer.push_back(
                            { TokenType::Argument, next } );
                    }
                }
            }

            TokenStream& TokenStream::operator++() {
                if ( m_tokenBuffer.size() >= 2 ) {
                    m_tokenBuffer.erase( m_tokenBuffer.begin() );
                } else {
                    if ( it != itEnd )
                        ++it;
                    loadBuffer();
                }
                return *this;
            }

            ParserResult convertInto( std::string const& source,
                                      std::string& target ) {
                target = source;
                return ParserResult::ok( ParseResultType::Matched );
            }

            ParserResult convertInto( std::string const& source,
                                      bool& target ) {
                std::string srcLC = source;
                std::transform( srcLC.begin(),
                                srcLC.end(),
                                srcLC.begin(),
                                []( unsigned char c ) {
                                    return static_cast<char>(
                                        std::tolower( c ) );
                                } );
                if ( srcLC == "y" || srcLC == "1" || srcLC == "true" ||
                     srcLC == "yes" || srcLC == "on" )
                    target = true;
                else if ( srcLC == "n" || srcLC == "0" || srcLC == "false" ||
                          srcLC == "no" || srcLC == "off" )
                    target = false;
                else
                    return ParserResult::runtimeError(
                        "Expected a boolean value but did not recognise: '" +
                        source + "'" );
                return ParserResult::ok( ParseResultType::Matched );
            }

            ExeName::ExeName():
                m_name( std::make_shared<std::string>( "<executable>" ) ) {}

            ExeName::ExeName( std::string& ref ): ExeName() {
                m_ref = std::make_shared<BoundValueRef<std::string>>( ref );
            }

            InternalParseResult
            ExeName::parse( std::string const&,
                            TokenStream const& tokens ) const {
                return InternalParseResult::ok(
                    ParseState( ParseResultType::NoMatch, tokens ) );
            }

            ParserResult ExeName::set( std::string const& newName ) {
                auto lastSlash = newName.find_last_of( "\\/" );
                auto filename = ( lastSlash == std::string::npos )
                                    ? newName
                                    : newName.substr( lastSlash + 1 );

                *m_name = filename;
                if ( m_ref )
                    return m_ref->setValue( filename );
                else
                    return ParserResult::ok( ParseResultType::Matched );
            }

            InternalParseResult Arg::parse( std::string const&,
                                            TokenStream const& tokens ) const {
                auto validationResult = validate();
                if ( !validationResult )
                    return InternalParseResult( validationResult );

                auto remainingTokens = tokens;
                auto const& token = *remainingTokens;
                if ( token.type != TokenType::Argument )
                    return InternalParseResult::ok( ParseState(
                        ParseResultType::NoMatch, remainingTokens ) );

                assert( !m_ref->isFlag() );
                auto valueRef =
                    static_cast<detail::BoundValueRefBase*>( m_ref.get() );

                auto result = valueRef->setValue( remainingTokens->token );
                if ( !result )
                    return InternalParseResult( result );
                else
                    return InternalParseResult::ok( ParseState(
                        ParseResultType::Matched, ++remainingTokens ) );
            }

            Opt::Opt( bool& ref ):
                ParserRefImpl( std::make_shared<BoundFlagRef>( ref ) ) {}

            std::vector<HelpColumns> Opt::getHelpColumns() const {
                std::ostringstream oss;
                bool first = true;
                for ( auto const& opt : m_optNames ) {
                    if ( first )
                        first = false;
                    else
                        oss << ", ";
                    oss << opt;
                }
                if ( !m_hint.empty() )
                    oss << " <" << m_hint << '>';
                return { { oss.str(), m_description } };
            }

            bool Opt::isMatch( std::string const& optToken ) const {
                auto normalisedToken = normaliseOpt( optToken );
                for ( auto const& name : m_optNames ) {
                    if ( normaliseOpt( name ) == normalisedToken )
                        return true;
                }
                return false;
            }

            InternalParseResult Opt::parse( std::string const&,
                                            TokenStream const& tokens ) const {
                auto validationResult = validate();
                if ( !validationResult )
                    return InternalParseResult( validationResult );

                auto remainingTokens = tokens;
                if ( remainingTokens &&
                     remainingTokens->type == TokenType::Option ) {
                    auto const& token = *remainingTokens;
                    if ( isMatch( token.token ) ) {
                        if ( m_ref->isFlag() ) {
                            auto flagRef =
                                static_cast<detail::BoundFlagRefBase*>(
                                    m_ref.get() );
                            auto result = flagRef->setFlag( true );
                            if ( !result )
                                return InternalParseResult( result );
                            if ( result.value() ==
                                 ParseResultType::ShortCircuitAll )
                                return InternalParseResult::ok( ParseState(
                                    result.value(), remainingTokens ) );
                        } else {
                            auto valueRef =
                                static_cast<detail::BoundValueRefBase*>(
                                    m_ref.get() );
                            ++remainingTokens;
                            if ( !remainingTokens )
                                return InternalParseResult::runtimeError(
                                    "Expected argument following " +
                                    token.token );
                            auto const& argToken = *remainingTokens;
                            if ( argToken.type != TokenType::Argument )
                                return InternalParseResult::runtimeError(
                                    "Expected argument following " +
                                    token.token );
                            auto result = valueRef->setValue( argToken.token );
                            if ( !result )
                                return InternalParseResult( result );
                            if ( result.value() ==
                                 ParseResultType::ShortCircuitAll )
                                return InternalParseResult::ok( ParseState(
                                    result.value(), remainingTokens ) );
                        }
                        return InternalParseResult::ok( ParseState(
                            ParseResultType::Matched, ++remainingTokens ) );
                    }
                }
                return InternalParseResult::ok(
                    ParseState( ParseResultType::NoMatch, remainingTokens ) );
            }

            Result Opt::validate() const {
                if ( m_optNames.empty() )
                    return Result::logicError( "No options supplied to Opt" );
                for ( auto const& name : m_optNames ) {
                    if ( name.empty() )
                        return Result::logicError(
                            "Option name cannot be empty" );
#ifdef CATCH_PLATFORM_WINDOWS
                    if ( name[0] != '-' && name[0] != '/' )
                        return Result::logicError(
                            "Option name must begin with '-' or '/'" );
#else
                    if ( name[0] != '-' )
                        return Result::logicError(
                            "Option name must begin with '-'" );
#endif
                }
                return ParserRefImpl::validate();
            }

            Help::Help( bool& showHelpFlag ):
                Opt( [&]( bool flag ) {
                    showHelpFlag = flag;
                    return ParserResult::ok( ParseResultType::ShortCircuitAll );
                } ) {
                static_cast<Opt&> ( *this )(
                    "display usage information" )["-?"]["-h"]["--help"]
                    .optional();
            }

            Parser& Parser::operator|=(Parser const& other) {
                m_options.insert(m_options.end(), other.m_options.begin(), other.m_options.end());
                m_args.insert(m_args.end(), other.m_args.begin(), other.m_args.end());
                return *this;
            }

            std::vector<HelpColumns> Parser::getHelpColumns() const {
                std::vector<HelpColumns> cols;
                for (auto const& o : m_options) {
                    auto childCols = o.getHelpColumns();
                    cols.insert(cols.end(), childCols.begin(), childCols.end());
                }
                return cols;
            }

            void Parser::writeToStream(std::ostream& os) const {
                if (!m_exeName.name().empty()) {
                    os << "usage:\n" << "  " << m_exeName.name() << ' ';
                    bool required = true, first = true;
                    for (auto const& arg : m_args) {
                        if (first)
                            first = false;
                        else
                            os << ' ';
                        if (arg.isOptional() && required) {
                            os << '[';
                            required = false;
                        }
                        os << '<' << arg.hint() << '>';
                        if (arg.cardinality() == 0)
                            os << " ... ";
                    }
                    if (!required)
                        os << ']';
                    if (!m_options.empty())
                        os << " options";
                    os << "\n\nwhere options are:\n";
                }

                auto rows = getHelpColumns();
                size_t consoleWidth = CATCH_CONFIG_CONSOLE_WIDTH;
                size_t optWidth = 0;
                for (auto const& cols : rows)
                    optWidth = (std::max)(optWidth, cols.left.size() + 2);

                optWidth = (std::min)(optWidth, consoleWidth / 2);

                for (auto const& cols : rows) {
                    auto row =
                        TextFlow::Column(cols.left).width(optWidth).indent(2) +
                        TextFlow::Spacer(4) +
                        TextFlow::Column(cols.right).width(consoleWidth - 7 - optWidth);
                    os << row << '\n';
                }
            }

            Result Parser::validate() const {
                for (auto const& opt : m_options) {
                    auto result = opt.validate();
                    if (!result)
                        return result;
                }
                for (auto const& arg : m_args) {
                    auto result = arg.validate();
                    if (!result)
                        return result;
                }
                return Result::ok();
            }

            InternalParseResult Parser::parse(std::string const& exeName, TokenStream const& tokens) const {

                struct ParserInfo {
                    ParserBase const* parser = nullptr;
                    size_t count = 0;
                };
                const size_t totalParsers = m_options.size() + m_args.size();
                assert(totalParsers < 512);
                // ParserInfo parseInfos[totalParsers]; // <-- this is what we really want to do
                ParserInfo parseInfos[512];

                {
                    size_t i = 0;
                    for (auto const& opt : m_options) parseInfos[i++].parser = &opt;
                    for (auto const& arg : m_args) parseInfos[i++].parser = &arg;
                }

                m_exeName.set(exeName);

                auto result = InternalParseResult::ok(ParseState(ParseResultType::NoMatch, tokens));
                while (result.value().remainingTokens()) {
                    bool tokenParsed = false;

                    for (size_t i = 0; i < totalParsers; ++i) {
                        auto& parseInfo = parseInfos[i];
                        if (parseInfo.parser->cardinality() == 0 || parseInfo.count < parseInfo.parser->cardinality()) {
                            result = parseInfo.parser->parse(exeName, result.value().remainingTokens());
                            if (!result)
                                return result;
                            if (result.value().type() != ParseResultType::NoMatch) {
                                tokenParsed = true;
                                ++parseInfo.count;
                                break;
                            }
                        }
                    }

                    if (result.value().type() == ParseResultType::ShortCircuitAll)
                        return result;
                    if (!tokenParsed)
                        return InternalParseResult::runtimeError("Unrecognised token: " + result.value().remainingTokens()->token);
                }
                // !TBD Check missing required options
                return result;
            }

            InternalParseResult ParserBase::parse(Args const& args) const {
                return parse(args.exeName(), TokenStream(args));
            }

} // namespace detail
    }     // namespace clara
} // namespace Catch
